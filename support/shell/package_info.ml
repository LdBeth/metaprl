(*
 * This is the information about modules.
 * Its really derived from Filter_cache
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

extends Package_sig

open Parsetree

open Printf

open Mp_debug
open Imp_dag
open String_set

open File_base_type

open Refiner.Refiner.TermMan
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Mp_resource
open Theory

open Filter_summary_type
open Filter_summary
open Filter_summary_util
open Filter_cache
open Filter_util
open Infix

open Tactic_type
open Tactic_type.Tacticals

open Package_sig

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Package_info%t"

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
   type t = parse_arg
   type cooked = (parse_arg * Proof_edit.ped) proof_info ref
   type raw = Proof.io_proof

   (*
    * Get a raw proof from the proof.
    *)
   let to_raw_aux arg name proof =
      match !proof with
         ProofRaw (_, proof) ->
            arg, proof
       | ProofEdit ((parse, eval) as arg, ped) ->
            let proof = Proof.io_proof_of_proof true parse eval (Proof.root (Proof_edit.proof_of_ped ped)) in
               if !debug_package_info then
                  eprintf "Converting the ped back to a regular proof: %s%t" name eflush;
               arg, proof

   let to_raw arg name proof =
      snd (to_raw_aux arg name proof)

   (*
    * Get a proof from the raw proof.
    *)
   let of_raw _ name proof =
      ref (ProofRaw (name, proof))

   (*
    * Convert the proof to a term.
    *)
   let to_term arg name proof =
      let (parse, eval), proof = to_raw_aux arg name proof in
         Proof.term_of_io_proof parse eval proof

   (*
    * Convert back to a proof.
    *)
   let of_term (parse, eval) name proof =
      ref (ProofRaw (name, Proof.io_proof_of_term parse eval proof))

   (*
    * Convert the proof to a term.
    *)
   let to_term_io arg name proof =
      let (parse, eval), proof = to_raw_aux arg name proof in
         Proof.term_io_of_io_proof parse eval proof

   (*
    * Convert back to a proof.
    *)
   let of_term_io (parse, eval) name proof =
      ref (ProofRaw (name, Proof.io_proof_of_term_io parse eval proof))

   (*
    * When we compile, we extract the tactics into a separate array
    * so that they can be compiled with the theory.  The term we produce
    * has type (unit -> extract).  Expands to:
    *
    *    (Package_info/prove name tactics)
    *
    * XXX: BUG: jyh: I backed this out, and right now proofs
    * always fail.
    *)
   let to_expr _ name proof =
      let loc = 0, 0 in
      let unit_patt = <:patt< () >> in
      let error_expr =
         <:expr< raise ( Refiner.Refiner.RefineError
                           ("Package_info/to_expr",
                              (Refiner.Refiner.RefineError.StringError
                                               "interactive proofs not implemented"))) >>
      in
         <:expr< fun [ $list: [unit_patt, None, error_expr]$ ] >>
end

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
    * This is the global info.
    *)
   type t =
      { pack_lock : Mutex.t;
        pack_cache : Cache.StrFilterCache.t;
        pack_dag : package ImpDag.t;
        mutable pack_packages : package ImpDag.node list
      }

   (*
    * This is the info we save when the structure is
    * loaded.
    *)
   and pack_str_info =
      { pack_str_info : Cache.StrFilterCache.info;
        pack_parse : parse_arg
      }

   (*
    * A package may have a signature, and an implementation
    * as a cache.  Modifications are in-place as information
    * is collected.
    *)
   and package =
      { pack_info : t;

        pack_name : string;

        mutable pack_status : status;
        mutable pack_sig_info : Cache.StrFilterCache.sig_info option;
        mutable pack_str : pack_str_info option;
        mutable pack_infixes : string list
      }

   (*
    * Proof is either in raw form, or it is editable.
    *)
   type proof = Convert.cooked

   (************************************************************************
    * CONSTRUCTION                                                         *
    ************************************************************************)

   (*
    * Create the cache.
    * Add placeholders for all the theories.
    *)
   let create path =
      let dag = ImpDag.create () in
      let pack =
         { pack_lock = Mutex.create ();
           pack_cache = Cache.StrFilterCache.create path;
           pack_dag = dag;
           pack_packages = []
         }
      in
      let hash = Hashtbl.create 17 in
      let mk_package name =
         { pack_info = pack;
           pack_name = name;
           pack_status = Incomplete;
           pack_sig_info = None;
           pack_str = None;
           pack_infixes = []
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
         pack.pack_packages <- List.map add_theory (get_theories ());
         pack

   (*
    * Lock the pack.
    *)
   let synchronize_pack pack f =
      let lock = pack.pack_lock in
         Mutex.lock lock;
         try
            let result = f pack in
               Mutex.unlock lock;
               result
         with
            exn ->
               Mutex.unlock lock;
               raise exn

   let synchronize_node node f =
      let lock = node.pack_info.pack_lock in
         Mutex.lock lock;
         try
            let result = f node in
               Mutex.unlock lock;
               result
         with
            exn ->
               Mutex.unlock lock;
               raise exn

   (************************************************************************
    * REFINER ACCESS                                                       *
    ************************************************************************)

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

   let sentinal { pack_name = name } =
      Tactic_type.Tactic.sentinal_of_refiner name

   let sentinal_object { pack_name = name } name' =
      Tactic_type.Tactic.sentinal_of_refiner_object name name'

   (*
    * Get the list of display forms.
    *)
   let get_dforms name =
      (get_theory name).thy_dformer

   let dforms { pack_name = name } =
      get_dforms name


   (************************************************************************
    * LOADING                                                              *
    ************************************************************************)

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
                  raise (Failure "Package_info/maybe_add_package: parent is not defined")
         end
    | path ->
         raise (Failure ("Package_info/insert_parent: parent is not toplevel: " ^ string_of_path path))

   (*
    * Add a signature package.
    * It adds the package, and creates
    * the edges to the parents.
    *)
   let maybe_add_package pack path sig_info infixes =
      match path with
         [name] ->
            let { pack_dag = dag;
                  pack_packages = packages
                } = pack
            in
            let node =
               try
                  let node = get_package pack name in
                  let pinfo = ImpDag.node_value dag node in
                     pinfo.pack_sig_info <- Some sig_info;
                     if pinfo.pack_status = Incomplete then
                        pinfo.pack_status <- ReadOnly;
                     pinfo.pack_infixes <- infixes;
                     node
               with
                  Not_found ->
                     let pinfo =
                        { pack_info = pack;
                          pack_name = name;
                          pack_status = ReadOnly;
                          pack_sig_info = Some sig_info;
                          pack_str = None;
                          pack_infixes = infixes
                        }
                     in
                     let node = ImpDag.insert dag pinfo in
                        pack.pack_packages <- node :: packages;
                        node
            in
            let parents = Filter_summary.parents sig_info in
               List.iter (insert_parent pack node) parents
       | _ ->
            raise (Failure "Package_info/maybe_add_package: nested modules are not implemented")

   (*
    * When a module is inlined, add the resources and infixes.
    * The info is the _signature_.  Later we may want to replace
    * it with the implementation.
    *)
   let inline_hook pack root_path cache (path, info) infixes =
      if !debug_package_info then
         eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;

      (* Add all the infix words and add the package *)
      let infixes = StringSet.union (StringSet.of_list (get_infixes info)) infixes in
         maybe_add_package pack path info (StringSet.elements infixes);
         infixes

   (*
    * Add an implementation package.
    * This replaces any current version of the package,
    * and adds the edges to the parents.
    *)
   let add_implementation pack_info =
      let { pack_info = pack; pack_name = name; pack_str = info } = pack_info in
      let { pack_dag = dag; pack_packages = packages } = pack in
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
      let parents =
         match info with
            Some { pack_str_info = info } ->
               Cache.StrFilterCache.parents info
          | None ->
               raise (Invalid_argument "Package_info/add_implementation")
      in
      let node = ImpDag.insert dag pack_info in
         pack.pack_packages <- node :: remove packages;
         List.iter (insert_parent pack node) parents

   (*
    * Load a package.
    * We search for the description, and load it.
    * If the ML file has already been loaded, retrieve
    * the refiner and display forms.  Else construct the code
    * to be evaluated, and return it.
    *)
   let load_aux arg pack_info =
      let { pack_info = pack; pack_name = name } = pack_info in
      let { pack_cache = cache } = pack in
         try
            let path = [name] in
            let info, infixes =
               Cache.StrFilterCache.load cache arg name (**)
                  ImplementationType InterfaceType
                  (inline_hook pack path) StringSet.empty AnySuffix
            in
            let pack_str =
               { pack_str_info = info;
                 pack_parse = arg
               }
            in
               pack_info.pack_str <- Some pack_str;
               pack_info.pack_status <- Unmodified;
               pack_info.pack_infixes <- StringSet.elements infixes;
               add_implementation pack_info;
               Cache.StrFilterCache.set_mode info InteractiveSummary
      with
         Not_found
       | Sys_error _ ->
            raise (Failure (sprintf "Package_info/load: '%s' not found" name))

   (*
    * Make sure the str info is valid.
    *)
   let auto_loading_str arg pack_info f =
      synchronize_node pack_info (function
         { pack_str = Some _ } ->
            f pack_info
       | { pack_str = None } ->
            load_aux arg pack_info;
            f pack_info)

   let auto_load_str arg pack_info =
      auto_loading_str arg pack_info (fun pack_info -> ())

   (*
    * Load the package if it is not loaded already.
    *)
   let load pack arg name =
      synchronize_pack pack (fun pack ->
            let pack_info =
               try ImpDag.node_value pack.pack_dag (get_package pack name) with
                  Not_found ->
                     { pack_info = pack;
                       pack_status = Unmodified;
                       pack_name = name;
                       pack_sig_info = None;
                       pack_str = None;
                       pack_infixes = []
                     }
            in
               auto_load_str arg pack_info;
               pack_info)

   (************************************************************************
    * DESTRUCTION                                                          *
    ************************************************************************)

   (*
    * Get the name of the package.
    *)
   let name { pack_name = name } =
      name

   (*
    * Get the filename for the package.
    *)
   let filename pack_info arg =
      auto_loading_str arg pack_info (function
         { pack_info = { pack_cache = cache }; pack_str = Some { pack_str_info = info } } ->
            Cache.StrFilterCache.filename cache info
       | { pack_name = name; pack_str = None } ->
            raise (NotLoaded name))

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
   let info pack_info arg =
      auto_loading_str arg pack_info (function
         { pack_str = Some { pack_str_info = info } } ->
            Cache.StrFilterCache.info info
       | { pack_str = None; pack_name = name } ->
            raise (NotLoaded name))

   let sig_info_aux = function
      { pack_sig_info = Some info } ->
         info
    | { pack_sig_info = None; pack_str = Some { pack_str_info = str_info; pack_parse = arg } } as pack ->
         let sig_info = Cache.StrFilterCache.sig_info str_info arg InterfaceType in
            pack.pack_sig_info <- Some sig_info;
            sig_info
    | { pack_sig_info = None; pack_str = None; pack_name = name } ->
         raise (NotLoaded name)

   let sig_info pack_info arg =
      try synchronize_node pack_info sig_info_aux with
         NotLoaded _ ->
            auto_loading_str arg pack_info sig_info_aux

   let find pack_info arg name =
      auto_loading_str arg pack_info (function
         { pack_str = Some { pack_str_info = info } } ->
            fst (Cache.StrFilterCache.find info name)
       | { pack_str = None; pack_name = name } ->
            raise (NotLoaded name))

   let set pack_info arg item =
      auto_loading_str arg pack_info (function
         { pack_str = Some { pack_str_info = info } } ->
            Cache.StrFilterCache.set_command info (item, (0, 0))
       | { pack_str = None; pack_name = name } ->
            raise (NotLoaded name))

   (*
    * DAG access.
    *)
   let get_node { pack_dag = dag; pack_packages = packages } info =
      let rec search = function
         node :: t ->
            let info' = ImpDag.node_value dag node in
               if info'.pack_name = info.pack_name then
                  node
               else
                  search t
       | [] ->
            raise Not_found
      in
         search packages

   let compare pack1 pack2 =
      pack1.pack_name < pack2.pack_name

   let packages pack =
      synchronize_pack pack (function
         { pack_dag = dag; pack_packages = packages } ->
            Sort.list compare (List.map (ImpDag.node_value dag) packages))

   let roots pack =
      synchronize_pack pack (function
         { pack_dag = dag } ->
            List.map (ImpDag.node_value dag) (ImpDag.roots dag))

   let parents pack info =
      synchronize_pack pack (function
            { pack_dag = dag } ->
               let node = get_node pack info in
                  List.map (ImpDag.node_value dag) (ImpDag.node_out_edges dag node))

   let children pack info =
      synchronize_pack pack (function
         { pack_dag = dag } ->
            let node = get_node pack info in
               List.map (ImpDag.node_value dag) (ImpDag.node_in_edges dag node))

   (*
    * Access to cache.
    *)
   let mk_opname pack_info opname params bterms =
      synchronize_node pack_info (fun pack_info ->
            match pack_info.pack_str with
               Some { pack_str_info = info } ->
                  Cache.StrFilterCache.mk_opname info opname params bterms
             | None ->
                  raise (Failure (sprintf "Package_info/mk_opname: %s not initialized" pack_info.pack_name)))

   (*
    * Get a loaded theory.
    *)
   let get pack name =
      synchronize_pack pack (function
         pack ->
            ImpDag.node_value pack.pack_dag (get_package pack name))

   (*
    * Save a package.
    * This happens only if it is modified.
    *)
   let save pack_info =
      synchronize_node pack_info (function
         { pack_status = ReadOnly; pack_name = name } ->
            raise (Failure (sprintf "Package_info/save: package '%s' is read-only" name))
       | { pack_status = Unmodified } ->
            ()
       | { pack_status = Incomplete; pack_name = name } ->
            raise (Failure (sprintf "Package_info/save: package '%s' is incomplete" name))
       | { pack_status = Modified; pack_str = Some { pack_str_info = info; pack_parse = arg } } ->
            Cache.StrFilterCache.save info arg (OnlySuffixes ["prlb"])
       | { pack_status = Modified; pack_str = None } ->
            raise (Invalid_argument "Package_info/save"))

   let export arg pack_info =
      auto_loading_str arg pack_info (function
         { pack_status = ReadOnly; pack_name = name } ->
            raise (Failure (sprintf "Package_info/save: package '%s' is read-only" name))
       | { pack_status = Incomplete; pack_name = name } ->
            raise (Failure (sprintf "Package_info/save: package '%s' is incomplete" name))
       | { pack_str = Some { pack_str_info = info } } ->
            Cache.StrFilterCache.save info arg (OnlySuffixes ["prla"])
       | { pack_str = None; pack_name = name } ->
            raise (NotLoaded name))

   (*
    * Create an empty package.
    *)
   let create_package pack arg name =
      synchronize_pack pack (function
         pack ->
            let info =
              { pack_info = pack;
                 pack_status = Modified;
                 pack_sig_info = None;
                 pack_str = Some { pack_str_info =
                                      Cache.StrFilterCache.create_cache pack.pack_cache (**)
                                      name ImplementationType InterfaceType;
                                   pack_parse = arg
                            };
                 pack_infixes = [];
                 pack_name = name
               }
            in
               add_implementation info;
               info)

   (*
    * Look for a global_resource for an item
    *)
   let find_bookmark mod_name item_name =
      let mod_name = String.capitalize mod_name in
      try Mp_resource.find (mod_name, item_name)
      with Not_found -> begin
         eprintf "Warning: resources for %s.%s not found,\n\ttrying to use default resources for %s%t" mod_name item_name mod_name eflush;
         try
            Mp_resource.find (Mp_resource.theory_bookmark mod_name)
         with Not_found ->
            raise (RefineError("Package_info/new_proof", StringError("can not find any resources")))
      end

   let arg_resource pack_info arg name =
      auto_loading_str arg pack_info (fun pack -> find_bookmark pack.pack_name name)

   (*
    * A new proof cannot be saved.
    *)
   let new_proof pack_info arg name hyps goal =
      auto_loading_str arg pack_info (function
         { pack_name = mod_name } ->
            let loc = 0, 0 in
            let sentinal = Tactic_type.Tactic.sentinal_of_refiner_object mod_name name in
            let bookmark = find_bookmark mod_name name in
            let seq = Tactic_type.Tactic.create sentinal (mk_msequent goal hyps) bookmark in
            let proof = Proof.create seq in
            let ped = Proof_edit.ped_of_proof [] proof in
               ref (ProofEdit (arg, ped)))

   (*
    * Get the status of the proof.
    *)
   let status_of_proof proof =
      match !proof with
         ProofEdit (_, ped) ->
            Proof_edit.status_of_ped ped
       | ProofRaw (_, proof) ->
            Proof.status_of_io_proof proof

   let node_count_of_proof proof =
      match !proof with
         ProofEdit (_, ped) ->
            Proof_edit.node_count_of_ped ped
       | ProofRaw (_, proof) ->
            Proof.node_count_of_io_proof proof

   (*
    * Convert a proof on demand.
    *)
   let ped_of_proof pack_info arg proof goal =
      auto_loading_str arg pack_info (function
         { pack_name = name } ->
            begin
               match !proof with
                  ProofEdit (_, ped) ->
                     ped
                | ProofRaw (name', proof') ->
                     let parse, eval = arg in
                     let sentinal = Tactic_type.Tactic.sentinal_of_refiner_object name name' in
                     let bookmark = find_bookmark name name' in
                     let proof' = Proof.proof_of_io_proof [] sentinal bookmark parse eval proof' in
                     let ped = Proof_edit.ped_of_proof [] proof' in
                        Proof_edit.set_goal ped goal;
                        proof := ProofEdit (arg, ped);
                        ped
            end)

   let proof_of_ped proof arg ped =
      proof := ProofEdit (arg, ped);
      proof
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
