(*
 * This is the information about modules.
 * Its really derived from Filter_cache
 *)

include Tactic_cache
include Tacticals

include Io_proof
include Package_type
include Proof_type

open Parsetree

open Printf

open Nl_debug
open Imp_dag

open File_base_type

open Refiner.Refiner.TermMan
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Resource
open Theory

open Filter_summary_type
open Filter_summary
open Filter_summary_util
open Filter_cache
open Filter_util
open Filter_prog
open Infix

open Tactic_cache
open Tacticals

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

let debug_sentinal = load_debug "sentinal"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The proofs are saved in the summary as Io_proof.proof,
 * but we also construct Proof_edit.t structures on demand.
 *)
type 'a proof_info =
   ProofRaw of string * Proof.io_proof
 | ProofEdit of 'a

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Make a dummy tactic_argument.
 *)
let null_tactic_argument =
   { ref_label = "main";
     ref_args = [];
     ref_fcache = extract (new_cache ())
   }

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
   type raw = Proof.io_proof

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
                  eprintf "Converting the ped back to a regular proof: %s%t" name eflush;
               proof

   (*
    * Get a proof from the raw proof.
    *)
   let of_raw name proof =
      if !debug_package_info then
         eprintf "Converting io proof for %s%t" name eflush;
      ref (ProofRaw (name, proof))

   (*
    * Convert the proof to a term.
    * We just save the io representation.
    *)
   let to_term name proof =
      let norm = Term_copy.normalize_term (Term_copy.create_norm ()) in
         term_of_proof norm (to_raw name proof)

   (*
    * Convert back to a proof.
    *)
   let of_term name proof =
      let denorm = Term_copy.denormalize_term (Term_copy.create_denorm ()) in
         ref (ProofRaw (name, proof_of_term denorm [] proof))

   (*
    * When we compile, we extract the tactics into a separate array
    * so that they can be compiled with the theory.  The term we produce
    * has type (unit -> extract).  Expands to:
    *
    *    (Package_info.prove name tactics)
    *
    * BUG: jyh: I backed this out, and right now proofs
    * always fail.
    *)
   let to_expr name proof =
      let loc = 0, 0 in
      let unit_patt = <:patt< () >> in
      let error_expr = <:expr< raise ( $uid: "Refiner"$ . $uid: "Refiner"$ . $uid: "RefineError"$
                                       ($str: "Package_info.to_expr"$,
                                              ($uid:"Refiner"$
                                               . $uid: "Refiner"$
                                               . $uid: "RefineError"$
                                               . $uid: "StringError"$
                                               $str: "interactive proofs not implemented"$))) >>
      in
         <:expr< fun [ $list: [unit_patt, None, error_expr]$ ] >>
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
        pack_eval : MLast.expr -> tactic;
        pack_name : string;
        mutable pack_sig  : Cache.StrFilterCache.sig_info option;
        mutable pack_info : Cache.StrFilterCache.info option;
        pack_arg : tactic_argument
      }

   (*
    * Built from a filter cache.
    *)
   type t =
      { pack_cache : Cache.StrFilterCache.t;
        pack_create_tactic : MLast.expr -> tactic;
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
   let create create_tactic path =
      let dag = ImpDag.create () in
      let hash = Hashtbl.create 17 in
      let mk_package name =
         { pack_status = Incomplete;
           pack_eval = create_tactic;
           pack_name = name;
           pack_sig = None;
           pack_info = None;
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
           pack_create_tactic = create_tactic;
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
            let { pack_create_tactic = eval; pack_dag = dag; pack_packages = packages } = pack in
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
                          pack_eval = eval;
                          pack_sig = Some sig_info;
                          pack_info = None;
                          pack_name = name;
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
         Cache.StrFilterCache.save info (AlwaysSuffix "prlb")
    | { pack_status = Modified; pack_info = None } ->
         raise (Invalid_argument "Package_info.save")

   (*
    * Create an empty package.
    *)
   let create_package pack name =
      let info =
         { pack_status = Modified;
           pack_eval = pack.pack_create_tactic;
           pack_sig = None;
           pack_info = Some (Cache.StrFilterCache.create_cache pack.pack_cache (**)
                                name ImplementationType InterfaceType);
           pack_name = name;
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
   let new_proof { pack_arg = { ref_label = label; ref_args = args } } name hyps goal =
      let denorm = Term_copy.denormalize_term (Term_copy.create_denorm ()) in
      let aterm =
         { aterm_goal = denorm goal;
           aterm_hyps = List.map denorm hyps;
           aterm_label = label;
           aterm_args = Tactic_type.map_attributes denorm args
         }
      in
      let step =
         let loc = 0, 0 in
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
         ref (ProofRaw (name, proof))

   (*
    * Get the status of the proof.
    *)
   let status_of_proof proof =
      match !proof with
         ProofEdit ped ->
            Proof_edit.status_of_ped ped
       | ProofRaw (_, { proof_status = status }) ->
            match status with
               Io_proof_type.StatusBad ->
                  Proof.Bad
             | Io_proof_type.StatusPartial ->
                  Proof.Partial
             | Io_proof_type.StatusAsserted ->
                  Proof.Asserted
             | Io_proof_type.StatusComplete ->
                  Proof.Complete

   (*
    * Convert a proof on demand.
    *)
   let ped_of_proof { pack_name = name; pack_eval = eval; pack_arg = arg } proof =
      match !proof with
         ProofEdit ped ->
            ped
       | ProofRaw (name', proof') ->
            let _ =
               if !debug_package_info then
                  begin
                     eprintf "Attributes are:\n";
                     List.iter (fun (name, _) -> eprintf "\t%s%t" name eflush) arg.ref_args;
                     eflush stderr
                  end
            in
            let refiner =
               if !debug_sentinal then
                  eprintf "Find refiner for %s.%s%t" name name' eflush;
               try find_refiner (get_theory name).thy_refiner name' with
                  Not_found ->
                     raise (RefineError ("ped_of_proof", StringStringError ("refiner not found", name')))
            in
            let sentinal = sentinal_of_refiner refiner in
            let proof' = Proof.proof_of_io_proof arg eval sentinal proof' in
            let ped = Proof_edit.ped_of_proof [] proof' in
               proof := ProofEdit ped;
               ped

   let proof_of_ped _ proof ped =
      proof := ProofEdit ped;
      proof

   (*
    * Get a tactic_arg for a module.
    *)
   let get_tactic_arg modname =
      let cache =
         let cache =
            try
               let rsrc = Base_cache.get_resource modname in
                  rsrc.resource_extract rsrc
            with
               Not_found ->
                  Tactic_cache.new_cache ()
         in
            Tactic_cache.extract cache
      in
      let add_attribute name con get_resource attributes =
         try
            let rsrc = get_resource modname in
               (name, con (rsrc.resource_extract rsrc)) :: attributes
         with
            Not_found ->
               attributes
      in
      let mk_int_tac_arg x =
         Tactic_type.IntTacticArg x
      in
      let mk_tac_arg x =
         Tactic_type.TacticArg x
      in
      let mk_typeinf_arg x =
         Tactic_type.TypeinfArg x
      in
      let attributes =
         add_attribute "d" mk_int_tac_arg Base_dtactic.get_resource []
      in
      let attributes =
         add_attribute "trivial" mk_tac_arg Base_auto_tactic.get_trivial_resource attributes
      in
      let attributes =
         add_attribute "auto" mk_tac_arg Base_auto_tactic.get_auto_resource attributes
      in
      let attributes =
         add_attribute "eqcd" mk_tac_arg Itt_equal.get_resource attributes
      in
      let attributes =
         add_attribute "typeinf" mk_typeinf_arg Typeinf.get_resource attributes
      in
      let attributes =
         add_attribute "squash" mk_tac_arg Itt_squash.get_resource attributes
      in
      let attributes =
         add_attribute "subtype" mk_tac_arg Itt_subtype.get_resource attributes
      in
         { ref_label = "main";
           ref_args = attributes;
           ref_fcache = cache
         }

   (*
    * Build the package from its info.
    *)
   let build_package pack name status info =
      let info =
         { pack_status = status;
           pack_eval = pack.pack_create_tactic;
           pack_sig = None;
           pack_info = Some info;
           pack_name = name;
           pack_arg = get_tactic_arg name
         }
      in
         add_implementation pack info;
         info

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
            Cache.StrFilterCache.load cache name (**)
               ImplementationType InterfaceType
               (inline_hook pack path) () (NewerSuffix "prlb")
         in
         let info' = build_package pack name Unmodified info in
            Cache.StrFilterCache.set_mode info InteractiveSummary;
            info'
      with
         Not_found
       | Sys_error _ ->
            raise (Failure (sprintf "Package_info.load: '%s' not found" name))
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
