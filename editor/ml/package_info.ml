(*
 * This is the information about modules.
 * Its really derived from Filter_cache
 *)

include Tactic_cache
include Tactic_type

include Io_proof
include Package_type
include Proof_type

open Parsetree

open Printf

open Debug
open Imp_dag

open Theory

open Filter_summary_type
open Filter_summary
open Filter_summary_util
open Filter_cache
open Filter_util
open Filter_prog
open Infix

open Tactic_cache
open Tactic_type

open Io_proof_type
open Io_proof
open Package_type
open Proof_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Package_info%t" eflush

let debug_package_info =
   create_debug (**)
      { debug_name = "package_info";
        debug_description = "display package operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The proofs are saved in the summary as Io_proof.proof,
 * but we also construct Proof_edit.t structures on demand.
 *)
type 'a proof_info =
   ProofRaw of string * proof
 | ProofEdit of 'a

(************************************************************************
 * REFERENCES                                                           *
 ************************************************************************)

let debug_item =
   let loc = 0, 0 in
   let unit_expr = <:expr< () >> in
      ref <:str_item< $exp: unit_expr$ >>

(*
 * Make a dummy tactic_argument.
 *)
let null_tactic_argument =
   { ref_label = "main";
     ref_args = [];
     ref_fcache = extract (new_cache ());
     ref_rsrc =
        { ref_d =       (fun _ _ -> raise (Failure "d tactic is not implemented"));
          ref_eqcd =    (fun _ ->   raise (Failure "eqcd tactic is not implemented"));
          ref_typeinf = (fun _ _ -> raise (Failure "typeinf tactic is not implemented"));
          ref_squash =  (fun _ ->   raise (Failure "squash tactic is not implemented"));
          ref_subtype = (fun _ ->   raise (Failure "subtype tactic is not implemented"))
        }
   }

(*
 * References:
 *   tactic_argument: double pointer for a reference to the tactic_argument.
 *   io_proofs: a hashtable of all the proofs in the module being evaluated
 *   tactics: a hashtable of the tactic arrays for each of the proofs
 *)
let tactic_argument = ref (ref null_tactic_argument)
let io_proofs = Hashtbl.create 97
let tactics = Hashtbl.create 97

(*
 * Clear the current tactic argument.
 *)
let clear_tactic_argument () =
   tactic_argument := ref null_tactic_argument

(*
 * Install a new argument (called when a theory is evaluated).
 *)
let install_tactic_argument arg =
   eprintf "Package_info.install_tactic_argument%t" eflush;
   (!tactic_argument) := arg

(*
 * Remove all the proofs.
 *)
let clear_proofs () =
   Hashtbl.clear io_proofs

(*
 * Remove all the tactics.
 *)
let clear_tactics () =
   Hashtbl.clear tactics

(*
 * Prove an theorem.
 * This dereferences the tactic_argument to get at the resources.
 *)
let prove name tactics' =
   let arg = !tactic_argument in
   let proof = Hashtbl.find io_proofs name in
   let _ = Hashtbl.add tactics name tactics' in
   let _ =
      if !debug_package_info then
         begin
            eprintf "Added tactics for %s\n" name;
            Array.iter (fun (name, _) -> eprintf "\t%s\n" name) tactics';
            eflush stderr
         end
   in
   let prove () =
      Proof.check (Proof.proof_of_io_proof !arg tactics' proof)
   in
      prove

(*
 * Labels.
 *)
let loc = 0, 0

let pt             = <:expr< $uid: "Proof_type"$ >>
let ref_label_id   = <:expr< $pt$ . $lid: "ref_label"$ >>
let ref_args_id    = <:expr< $pt$ . $lid: "ref_args"$ >>
let ref_fcache_id  = <:expr< $pt$ . $lid: "ref_fcache"$ >>
let ref_rsrc_id    = <:expr< $pt$ . $lid: "ref_rsrc"$ >>

let tt             = <:expr< $uid: "Tactic_type"$ >>
let ref_d_id       = <:expr< $tt$ . $lid: "ref_d"$ >>
let ref_eqcd_id    = <:expr< $tt$ . $lid: "ref_eqcd"$ >>
let ref_typeinf_id = <:expr< $tt$ . $lid: "ref_typeinf"$ >>
let ref_squash_id  = <:expr< $tt$ . $lid: "ref_squash"$ >>
let ref_subtype_id = <:expr< $tt$ . $lid: "ref_subtype"$ >>

let cache_default = <:expr< $uid: "Tactic_cache"$ . $lid: "new_cache"$ () >>

let make_extract expr =
   let extract_expr =
      <:expr< $uid: "Tactic_cache"$ . $lid: "extract"$ >>
   in
      <:expr< $extract_expr$ $expr$ >>

let wild_patt = <:patt< _ >>

let fail_expr msg =
   let fail_expr = <:expr< $uid: "Failure"$ $str: msg$ >> in
      <:expr< raise $fail_expr$ >>

let wild_fun_expr e =
   <:expr< fun [$list: [wild_patt, None, e]$] >>

let d_default =
   let fail_expr = fail_expr "d tactic is not implemented" in
      wild_fun_expr (wild_fun_expr fail_expr)

let eqcd_default =
   let fail_expr = fail_expr "eqcd tactic is not implemented" in
      wild_fun_expr fail_expr

let typeinf_default =
   let fail_expr = fail_expr "typeinf is not implemented" in
      wild_fun_expr (wild_fun_expr fail_expr)

let squash_default =
   let fail_expr = fail_expr "squash tactic is not implemented" in
      wild_fun_expr fail_expr

let subtype_default =
   let fail_expr = fail_expr "subtype tactic is not implemented" in
      wild_fun_expr fail_expr

(*
 * After the entire module has been evaluated,
 * build a tactic_argument, and eval the following:
 *
 * let _ = Package_info.install_tactic_argument args
 *
 * where args : Tactic_type.tactic_argument is the following expression:
 *    { Tactic_type.ref_label = "main";
 *      Tactic_type.ref_args = [];
 *      Tactic_type.ref_fcache = cache_resource.resource_extract cache_resource;
 *      Tactic_type.ref_rsrc =
 *         { ref_d = d_resource.Resource.resource_extract d_resource;
 *           ref_eqcd = eqcd_resource.Resource.resource_extract eqcd_resource;
 *           ref_typeinf = typeinf_resource.Resource.resource_extract typeinf_resource;
 *           ref_squash = squash_resource.Resource.resource_extract squash_resource;
 *           ref_subtype = subtype_resource.Resource.resource_extract subtype_resource
 *         }
 *     }
 *
 * The resources are checked first to see if they exist, and if they
 * don't, then dummy values are plugged in.
 *)
let install_tactic_arg_expr resources =
   let _ =
      if !debug_package_info then
         let print (_, { resource_name = name }) =
            eprintf "\tresource %s%t" name eflush
         in
            eprintf "Package_info.install_tactic_arg: resources:\n";
            List.iter print resources
   in
   let rec get_resource name default =
      if !debug_package_info then
         eprintf "Package_info.install_tactic_arg: check for resource %s%t" name eflush;
      let rec search = function
         (_, { resource_name = name' }) :: t ->
            if !debug_package_info then
               eprintf "Package_info.install_tactic_arg: %s = %s%t" name name' eflush;
            if name = name' then
               (<:expr< $lid: name$ . $uid: "Resource"$ . $lid: "resource_extract"$ $lid: name$ >>)
            else
               search t
       | [] ->
            default
      in
         search resources
   in
   let loc = 0, 0 in
   let label_expr = <:expr< $str: "main"$ >> in
   let args_expr  = <:expr< [] >> in
   let cache_expr = get_resource "cache_resource" cache_default in
   let d_expr = get_resource "d_resource" d_default in
   let eqcd_expr = get_resource "eqcd_resource" eqcd_default in
   let typeinf_expr = get_resource "typeinf_resource" typeinf_default in
   let squash_expr = get_resource "squash_resource" squash_default in
   let subtype_expr = get_resource "subtype_resource" subtype_default in
   let rsrc_expr =
      (<:expr< { $list: [ ref_d_id, d_expr;
                          ref_eqcd_id, eqcd_expr;
                          ref_typeinf_id, typeinf_expr;
                          ref_squash_id, squash_expr;
                          ref_subtype_id, subtype_expr
                        ]$
               } >>)
   in
   let arg_expr =
      (<:expr< { $list: [ ref_label_id, label_expr;
                          ref_args_id, args_expr;
                          ref_fcache_id, make_extract cache_expr;
                          ref_rsrc_id, rsrc_expr
                        ]$
               } >>)
   in
   let install_expr =
      <:expr< $uid: "Printexc"$ . $lid: "print"$
              $uid: "Package_info"$ . $lid: "install_tactic_argument"$
              $arg_expr$
      >>
   in
      if !debug_package_info then
         eprintf "Install tactic arg%t" eflush;
      <:str_item< $exp: install_expr$ >>

(*
 * Default tactic array includes the initial identity tactic.
 *)
let null_tactics = ["null", [|"idT", idT|]]

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * For debugging, we keep a display form base.
 *)
let debug_forms = ref Dform_print.null_mode_base

(*
 * The proofs are marshaled into the file as IO_proof.proof.
 *)
module Convert =
struct
   (*
    * The type of proofs.
    *)
   type t = Proof_edit.t proof_info ref
   type raw = proof

   (*
    * Get a raw proof from the proof.
    *)
   let to_raw name proof =
      match !proof with
         ProofRaw (_, proof) ->
            proof
       | ProofEdit ped ->
            let proof = Proof.io_proof_of_proof (Proof.main (Proof_edit.proof_of_ped ped)) in
               if !debug_package_info then
                  begin
                     eprintf "Converting the ped back to a regular proof: %s%t" name eflush;
                     let db = Dform_print.get_mode_base !debug_forms "prl" in
                        Io_proof.print_proof db proof
                  end;
               proof

   (*
    * Get a proof from the raw proof.
    *)
   let of_raw name proof =
      normalize_proof proof;
      if !debug_package_info then
         begin
            eprintf "Converting io proof for %s%t" name eflush;
            let db = Dform_print.get_mode_base !debug_forms "prl" in
               Io_proof.print_proof db proof
         end;
      ref (ProofRaw (name, proof))

   (*
    * Convert the proof to a term.
    * We just save the io representation.
    *)
   let to_term name proof =
      term_of_proof (to_raw name proof)

   (*
    * Convert back to a proof.
    *)
   let of_term name proof =
      ref (ProofRaw (name, proof_of_term proof))

   (*
    * When we compile, we extract the tactics into a separate array
    * so that they can be compiled with the theory.  The term we produce
    * has type (unit -> extract).  Expands to:
    *
    *    (Package_info.prove name tactics)
    *)
   let to_expr name proof =
      let loc = 0, 0 in
      let proof = to_raw name proof in
      let tactics = tactics_of_proof proof in
         Hashtbl.add io_proofs name proof;
         <:expr< $uid: "Package_info"$ . $lid: "prove"$ $str: name$ $tactics$ >>
end

(*
 * Extraction functions.
 *)
module Extract = MakeExtract (Convert)

(*
 * Build the cache from the converter.
 *)
module Cache = MakeCaches (Convert)

(*
 * Now build the package.
 *)
module Package =
struct
   (*
    * A package may have a signature, and an implementation
    * as a cache.
    *)
   type package =
      { mutable pack_status : status;
        pack_name : string;
        mutable pack_sig  : Cache.StrFilterCache.sig_info option;
        mutable pack_info : Cache.StrFilterCache.info option;
        pack_tactics : (string * ((string * tactic) array)) list;
        pack_arg : tactic_argument
      }

   (*
    * Built from a filter cache.
    *)
   type t =
      { pack_cache : Cache.StrFilterCache.t;
        pack_dag : package ImpDag.t;
        mutable pack_packages : package ImpDag.node list
      }

   (*
    * Proof is either in raw form, or it is editable.
    *)
   type proof = Proof_edit.t proof_info ref

   (*
    * Create the cache.
    * Add placeholders for all the theories.
    *)
   let create path =
      let dag = ImpDag.create () in
      let hash = Hashtbl.create 17 in
      let mk_package name =
         { pack_status = Incomplete;
           pack_name = name;
           pack_sig = None;
           pack_info = None;
           pack_tactics = null_tactics;
           pack_arg = null_tactic_argument
         }
      in
      let find_or_create name =
         try Hashtbl.find hash name with
            Not_found ->
               let pack = mk_package name in
               let node = ImpDag.insert dag pack in
                  Hashtbl.add hash name node;
                  node
      in
      let add_theory thy =
         let { thy_name = name } = thy in
         let node = find_or_create name in
         let add_parent { thy_name = name } =
            ImpDag.add_edge dag (find_or_create name) node
         in
            List.iter add_parent (Theory.get_parents thy);
            node
      in
         { pack_cache = Cache.StrFilterCache.create path;
           pack_dag = dag;
           pack_packages = List.map add_theory (get_theories ())
         }

   (*
    * See if a theory is already loaded.
    *)
   let is_theory_loaded name =
      let rec search = function
         { thy_name = name' } :: t ->
            if name = name' then
               true
            else
               search t
       | [] ->
            false
      in
         search (get_theories ())

   (*
    * Get a theory by name.
    *)
   let get_theory name =
      let rec search = function
         thy :: t ->
            if thy.thy_name = name then
               thy
            else
               search t
       | [] ->
            raise Not_found
      in
         search (get_theories ())

   (*
    * Get the refiner.
    *)
   let refiner { pack_name = name } =
      (get_theory name).thy_refiner

   (*
    * Get the list of display forms.
    *)
   let get_dforms name =
      (get_theory name).thy_dformer

   let dforms { pack_name = name } =
      get_dforms name

   (*
    * Get the name of the package.
    *)
   let name { pack_name = name } =
      name

   (*
    * Get the filename for the package.
    *)
   let filename pack = function
      { pack_info = Some info } ->
         Cache.StrFilterCache.filename pack.pack_cache info
    | { pack_info = None } ->
         raise (Failure "Package_info.filename: package is not loaded")

   (*
    * Get the status of the package.
    *)
   let status { pack_status = status } =
      status

   (*
    * Set the status of the package.
    *)
   let set_status pack status =
      pack.pack_status <- status

   (*
    * "Touch" the package, meaning update its writable status.
    *)
   let touch pack =
      match pack.pack_status with
         ReadOnly ->
            raise (Failure "touch")
       | _ ->
            pack.pack_status <- Modified

   (*
    * Get the items in the module.
    *)
   let info = function
      { pack_info = Some info } ->
         Cache.StrFilterCache.info info
    | { pack_info = None; pack_name = name } ->
         raise (NotLoaded name)

   let sig_info = function
      { pack_sig = Some info } ->
         info
    | { pack_sig = None; pack_info = Some info } as pack ->
         let info = Cache.StrFilterCache.sig_info info InterfaceType in
            pack.pack_sig <- Some info;
            info
    | { pack_sig = None; pack_info = None; pack_name = name } ->
         raise (NotLoaded name)

   let find info name =
      match info with
         { pack_info = Some info } ->
            fst (Cache.StrFilterCache.find info name)
       | { pack_info = None; pack_name = name } ->
            raise (NotLoaded name)

   let set info item =
      eprintf "Setting item%t" eflush;
      match info with
         { pack_info = Some info } ->
            Cache.StrFilterCache.set_command info (item, (0, 0))
       | { pack_info = None; pack_name = name } ->
            raise (NotLoaded name)

   (*
    * DAG access.
    *)
   let get_node { pack_dag = dag; pack_packages = packages } info =
      let rec search = function
         node :: t ->
            let info' = ImpDag.node_value dag node in
               if info' == info then
                  node
               else
                  search t
       | [] ->
            raise Not_found
      in
         search packages

   let packages { pack_dag = dag; pack_packages = packages } =
      List.map (ImpDag.node_value dag) packages

   let roots { pack_dag = dag } =
      List.map (ImpDag.node_value dag) (ImpDag.roots dag)

   let parents pack info =
      let { pack_dag = dag } = pack in
      let node = get_node pack info in
         List.map (ImpDag.node_value dag) (ImpDag.node_out_edges dag node)

   let children pack info =
      let { pack_dag = dag } = pack in
      let node = get_node pack info in
         List.map (ImpDag.node_value dag) (ImpDag.node_in_edges dag node)

   (*
    * Get a node by its name.
    *)
   let load_check dag name node =
      let { pack_name = name' } = ImpDag.node_value dag node in
         name' = name

   let is_loaded { pack_dag = dag; pack_packages = packages } name =
      List_util.existsp (load_check dag name) packages

   let get_package { pack_dag = dag; pack_packages = packages } name =
      List_util.find (load_check dag name) packages

   (*
    * Access to cache.
    *)
   let mk_opname pack opname =
      match pack.pack_info with
         Some info ->
            Cache.StrFilterCache.mk_opname info opname
       | None ->
            raise (Failure (sprintf "Package_info.mk_opname: %s not initialized" pack.pack_name))

   (*
    * Add a parent edge.
    * We only allow parents with toplevel names.
    *)
   let insert_parent pack node = function
      [parent] ->
         begin
            try
               let pnode = get_package pack parent in
                  ImpDag.add_edge pack.pack_dag pnode node
            with
               Not_found ->
                  raise (Failure "Package_info.maybe_add_package: parent is not defined")
         end
    | path ->
         raise (Failure ("Package_info.insert_parent: parent is not toplevel: " ^ string_of_path path))

   (*
    * Add an implementation package.
    * This replaces any current version of the package,
    * and adds the edges to the parents.
    *)
   let add_implementation pack info =
      let { pack_dag = dag; pack_packages = packages } = pack in
      let { pack_name = name; pack_info = info' } = info in
      let rec remove = function
         node :: t ->
            let { pack_name = name' } = ImpDag.node_value dag node in
               if name' = name then
                  begin
                     ImpDag.delete dag node;
                     t
                  end
               else
                  node :: remove t
       | [] ->
            []
      in
      let node = ImpDag.insert dag info in
      let parents =
         match info' with
            Some info ->
               Cache.StrFilterCache.parents info
          | None ->
               raise (Invalid_argument "Package_info.add_implementation")
      in
         pack.pack_packages <- node :: (remove packages);
         List.iter (insert_parent pack node) parents

   (*
    * Add a signature package.
    * It adds the package, and creates
    * the edges to the parents.
    *)
   let maybe_add_package pack path sig_info =
      match path with
         [name] ->
            let { pack_dag = dag; pack_packages = packages } = pack in
            let node =
               try
                  let node = get_package pack name in
                  let pinfo = ImpDag.node_value dag node in
                     pinfo.pack_sig <- Some sig_info;
                     if pinfo.pack_status = Incomplete then
                        pinfo.pack_status <- ReadOnly;
                     node
               with
                  Not_found ->
                     let pinfo =
                        { pack_status = ReadOnly;
                          pack_sig = Some sig_info;
                          pack_info = None;
                          pack_name = name;
                          pack_tactics = null_tactics;
                          pack_arg = null_tactic_argument
                        }
                     in
                     let node = ImpDag.insert dag pinfo in
                        pack.pack_packages <- node :: packages;
                        node
            in
            let parents = Filter_summary.parents sig_info in
               List.iter (insert_parent pack node) parents
       | _ ->
            raise (Failure "Package_info.maybe_add_package: nested modules are not implemented")

   (*
    * When a module is inlined, add the resources and infixes.
    * The info is the _signature_.  Later we may want to replace
    * it with the implementation.
    *)
   let inline_hook pack root_path cache (path, info) () =
      (* Include all the resources *)
      if !debug_package_info then
         eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;

      (* Add all the infix words *)
      List.iter add_infix (get_infixes info);

      (* Add this node to the pack *)
      maybe_add_package pack path info

   (*
    * Get a loaded theory.
    *)
   let get pack name =
      let { pack_dag = dag } = pack in
         ImpDag.node_value dag (get_package pack name)

   (*
    * Save a package.
    * This happens only if it is modified.
    *)
   let save pack = function
      { pack_status = ReadOnly; pack_name = name } ->
         raise (Failure (sprintf "Package_info.save: package '%s' is read-only" name))
    | { pack_status = Unmodified } ->
         ()
    | { pack_status = Incomplete; pack_name = name } ->
         raise (Failure (sprintf "Package_info.save: package '%s' is incomplete" name))
    | { pack_status = Modified; pack_info = Some info } ->
         Cache.StrFilterCache.save info
    | { pack_status = Modified; pack_info = None } ->
         raise (Invalid_argument "Package_info.save")

   (*
    * Create an empty package.
    *)
   let create_package pack name =
      let info =
         { pack_status = Modified;
           pack_sig = None;
           pack_info = Some (Cache.StrFilterCache.create_cache pack.pack_cache (**)
                                name ImplementationType InterfaceType);
           pack_name = name;
           pack_tactics = null_tactics;
           pack_arg = null_tactic_argument
         }
      in
         add_implementation pack info;
         info

   (*
    * tactic_argument for the package.
    *)
   let argument { pack_arg = arg } =
      arg

   (*
    * A new proof cannot be saved.
    *)
   let new_proof { pack_arg = { ref_label = label; ref_args = args } } hyps goal =
      let aterm =
         { aterm_goal = goal;
           aterm_hyps = hyps;
           aterm_label = label;
           aterm_args = args
         }
      in
      let step =
         { step_goal = aterm;
           step_subgoals = [aterm];
           step_ast = (<:expr< $lid: "idT"$ >>);
           step_text = "idT"
         }
      in
      let proof =
         { proof_status = StatusPartial;
           proof_step = ProofStep step;
           proof_children = [ChildGoal aterm];
           proof_extras = []
         }
      in
         ref (ProofRaw ("null", proof))

   (*
    * Convert a proof on demand.
    *)
   let ped_of_proof { pack_tactics = tactics; pack_arg = arg } proof =
      match !proof with
         ProofEdit ped ->
            ped
       | ProofRaw (name, proof') ->
            let _ =
               if !debug_package_info then
                  begin
                     eprintf "Lookup up from tactics in %s\nHere are the choices" name;
                     List.iter (fun (name, _) -> eprintf " %s" name) tactics;
                     eflush stderr
                  end
            in
            let tactics = List.assoc name tactics in
            let proof' = Proof.proof_of_io_proof arg tactics proof' in
            let ped = Proof_edit.ped_of_proof [] proof' in
               proof := ProofEdit ped;
               ped

   let proof_of_ped _ proof ped =
      proof := ProofEdit ped;
      proof

   (*
    * Build the package from its info.
    *)
   let build_package pack name status info =
      let tacl = ref null_tactics in
      let add name tactics =
         Ref_util.push (name, tactics) tacl
      in
      let _ = Hashtbl.iter add tactics in
      let info =
         { pack_status = status;
           pack_sig = None;
           pack_info = Some info;
           pack_name = name;
           pack_tactics = !tacl;
           pack_arg = !(!tactic_argument)
         }
      in
         add_implementation pack info;
         info

   (*
    * This function is used to contruct the tactic array when
    * a theory is loaded.
    *)
   let loaded_tactics info =
      let rec collect exprs = function
         (name, h)::t ->
            let exprs =
               match h with
                  Interactive proof ->
                     Convert.to_expr name proof :: exprs
                | Primitive _
                | Derived _ ->
                     exprs
            in
               collect exprs t
       | [] ->
            List.rev exprs
      in
      let proofs = Cache.StrFilterCache.proofs info in
      let exprs = collect [] proofs in
      let mk_item expr =
         let loc = 0, 0 in
            (<:str_item< $exp: expr$ >>)
      in
         List.map mk_item exprs

   (*
    * Load a package.
    * We search for the description, and load it.
    * If the ML file has already been loaded, retrieve
    * the refiner and display forms.  Else construct the code
    * to be evaluated, and return it.
    *)
   let load pack name =
      try
         let loc = 0, 0 in
         let { pack_cache = cache } = pack in
         let path = [name] in
         let info, () =
            clear_proofs ();
            clear_tactics ();
            Cache.StrFilterCache.load cache name ImplementationType InterfaceType (inline_hook pack path) ()
         in
         let status =
            let filename = Cache.StrFilterCache.filename cache info in
            let filename = (Filename_util.root filename) ^ ".ml" in
               if Sys.file_exists filename then
                  ReadOnly
               else
                  Unmodified
         in
         let info' = Cache.StrFilterCache.info info in
         let mod_name = String.capitalize name in
         let arg_item = install_tactic_arg_expr (Cache.StrFilterCache.resources info) in
         let item =
            if is_theory_loaded name then
               let open_item = (<:str_item< open $[ mod_name ]$ >>) in
               let _ = debug_forms := get_dforms name in
               let items = open_item :: loaded_tactics info @ [arg_item] in
               let mod_expr = (<:module_expr< struct $list: items$ end >>) in
               let dumb_name = "Arg" ^ mod_name in
                  (<:str_item< module $dumb_name$ = $mod_expr$ >>)
            else
               let items = Extract.extract_str info' (Cache.StrFilterCache.resources info) name in
               let items = (List.map fst items) @ [arg_item] in
               let mod_expr = (<:module_expr< struct $list: items$ end >>) in
                   (<:str_item< module $mod_name$ = $mod_expr$ >>)
         in
         let _ = debug_item := item in
         let pt_item = Ast2pt.str_item item [] in
            if Toploop.execute_phrase false (Ptop_def pt_item) then
               let info' = build_package pack name status info in
                  clear_proofs ();   (* so these can be garbage collected *)
                  clear_tactics ();
                  Cache.StrFilterCache.set_mode info InteractiveSummary;
                  info'
            else
               raise (Failure "Package_info.load: evaluation failed")
      with
         Not_found
       | Sys_error _ ->
             raise (Failure (sprintf "Package_info.load: '%s' not found" name))

         (*
          * BUG: We have to peek into the type system.
          * Have to copy <ocaml-src>/typing/typecore.cmi into CAMLLIB
          * This is really a bug in Toploop.execute_phrase, which
          * should not raise an exception.
          *)
       | Typecore.Error (_, err) ->
             Typecore.report_error err;
             eflush stdout;
             raise (Failure "Package_info.load: load failed")
end

(*
 * $Log$
 * Revision 1.15  1998/06/15 22:31:42  jyh
 * Added CZF.
 *
 * Revision 1.14  1998/06/12 13:45:06  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.13  1998/06/09 20:51:12  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.12  1998/06/01 13:52:13  jyh
 * Proving twice one is two.
 *
 * Revision 1.11  1998/05/29 14:52:47  jyh
 * Better Makefiles.
 *
 * Revision 1.10  1998/05/28 13:45:42  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.9  1998/04/28 18:29:41  jyh
 * ls() works, adding display.
 *
 * Revision 1.8  1998/04/24 02:41:26  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.7  1998/04/23 20:03:41  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.6  1998/04/17 02:25:29  jyh
 * Implementing shell.
 *
 * Revision 1.5  1998/04/17 01:30:41  jyh
 * Editor is almost constructed.
 *
 * Revision 1.4  1998/04/16 14:55:44  jyh
 * Upgrading packages.
 *
 * Revision 1.3  1998/04/15 22:28:49  jyh
 * Converting packages from summaries.
 *
 * Revision 1.2  1998/04/15 12:39:32  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.1  1997/08/06 16:17:16  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/10/23 15:17:50  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:33:23  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:29  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:55  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
