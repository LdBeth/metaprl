(*
 * This is the information about modules.
 * It is really derived from Filter_cache
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
open Lm_debug

open Lm_printf
open Lm_thread
open Lm_string_set

open File_base_type

open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Theory

open Filter_summary_type
open Filter_cache
open Filter_util

open Tactic_type
open Shell_sig

let debug_package_info =
   create_debug (**)
      { debug_name = "package_info";
        debug_description = "display package operations";
        debug_value = false
      }

(* unused
let debug_sentinal = load_debug "sentinal"
*)

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
            let proof = Proof.io_proof_of_proof true parse eval (Proof_edit.proof_of_ped ped) in
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

end

(*
 * Build the cache from the converter.
 *)
module Cache = MakeCaches (Convert)

(*
 * This is the global info.
 *)
type info =
   { pack_cache    : Cache.StrFilterCache.t;
     pack_groups   : (string, (string * LexStringSet.t)) Hashtbl.t;

     (*
      * For GC purposes, we keep handles to modified packages.
      * The loaded packages are kept in the weak array.
      *
      * Invariants:
      *    1. All packages in pack_modified have status PackModified.
      *    2. Any unmodified package can be garbage collected
      *       once all references to it are dropped.
      *)
     mutable pack_modified : package list;
     mutable pack_packages : package Weak.t
   }

and t = info State.entry

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

     mutable pack_status   : package_status;
     mutable pack_sig_info : Cache.StrFilterCache.sig_info option;
     mutable pack_str      : pack_str_info option;
     mutable pack_infixes  : Infix.Set.t;
   }

(*
 * Proof is either in raw form, or it is editable.
 *)
type proof = Convert.cooked

(************************************************************************
 * CONSTRUCTION                                                         *
 ************************************************************************)

(*
 * Load the list of theory descriptors.
 *)
let refresh pack_entry path =
   State.write pack_entry (fun pack ->
         let add_theory thy =
            let dsc, theories =
               try Hashtbl.find pack.pack_groups thy.thy_group with
                  Not_found ->
                     thy.thy_groupdesc, LexStringSet.empty
            in
               if thy.thy_groupdesc <> dsc then
                  raise (Failure (sprintf "Description mismatch:\n %s described %s as %s,\nbut %s describes it as %s" (**)
                                     (LexStringSet.choose theories) thy.thy_group dsc thy.thy_name thy.thy_groupdesc));
               Hashtbl.replace pack.pack_groups thy.thy_group (dsc, LexStringSet.add theories thy.thy_name)
         in
            List.iter add_theory (get_theories ()))

(*
 * Create the cache.
 *)
let create path =
   let pack =
      { pack_cache = Cache.StrFilterCache.create path;
        pack_groups = Hashtbl.create 17;
        pack_modified = [];
        pack_packages = Weak.create 32
      }
   in
   let pack = State.shared_val "Package_info.pack" pack in
      refresh pack path;
      pack

let clear_cache pack =
   State.write pack (fun pack -> Cache.StrFilterCache.clear pack.pack_cache)

(*
 * Lock the pack.
 *)
let synchronize_pack pack_entry f =
   State.write pack_entry f

let synchronize_node node f =
   State.write node.pack_info (fun _ ->
         f node)

(************************************************************************
 * REFINER ACCESS                                                       *
 ************************************************************************)

(*
 * Get the refiner.
 *)
let get_refiner pack =
   (get_theory pack.pack_name).thy_refiner

(************************************************************************
 * GARBAGE COLLECTION
 ************************************************************************)

(*
 * Set the status of the package.
 *)
let rec remove_pack pack1 l2 =
   match l2 with
      pack2 :: l2 ->
         if pack2.pack_name = pack1.pack_name then
            l2
         else
            pack2 :: remove_pack pack1 l2
    | [] ->
         raise (Invalid_argument "remove_pack")

let set_status pack status =
   State.write pack.pack_info (fun info ->
         match pack.pack_status, status with
            PackModified, PackUnmodified ->
               info.pack_modified <- remove_pack pack info.pack_modified;
               pack.pack_status <- status
          | PackUnmodified, PackModified ->
               info.pack_modified <- pack :: info.pack_modified;
               pack.pack_status <- status
          | PackUnmodified, PackUnmodified
          | PackModified, PackModified ->
               ())

(*
 * "Touch" the package, meaning update its writable status.
 *)
let touch pack =
   set_status pack PackModified

(*
 * Load a package if not already loaded.
 *)

(* Find a package in a weak array *)
let find_package weak name =
   let len = Weak.length weak in
   let rec find i =
      if i = len then
         raise (NotLoaded name)
      else
         match Weak.get weak i with
            Some pack ->
               if pack.pack_name = name then
                  pack
               else
                  find (succ i)
          | None ->
               find (succ i)
   in
      find 0

(* Remove a package if it exists *)
(* unused
let remove_package weak name =
   let len = Weak.length weak in
   let rec remove i =
      if i <> len then
         match Weak.get weak i with
            Some pack when pack.pack_name = name ->
               Weak.set weak i None
          | _ ->
               remove (i + 1)
   in
      remove 0
*)

(* Add a package to the weak array, but raise an exception if the array is full *)
let add_package info pack =
   let weak = info.pack_packages in
   let len = Weak.length weak in
   let rec add i =
      if i = len then
         raise (Failure "add_package")
      else
         match Weak.get weak i with
            Some pack' ->
               if pack'.pack_name = pack.pack_name then begin
                  if pack' != pack then
                     eprintf "Duplicate package %s@." pack'.pack_name
               end
               else
                  add (succ i)
          | None ->
               (* eprintf "Adding package at index %d (%d total modified packages)@." i (List.length info.pack_modified); *)
               Weak.set weak i (Some pack)
   in
      add 0

(* Add the package, and increase the array size if it is full *)
let add_package info pack =
   try add_package info pack with
      Failure _ ->
         let weak1 = info.pack_packages in
         let len = Weak.length weak1 in
         let weak2 = Weak.create (len * 2) in
            Weak.blit weak1 0 weak2 0 len;
            Weak.set weak2 len (Some pack);
            info.pack_packages <- weak2

(* Find or create the package *)
let get_package pack_entry name =
   State.write pack_entry (fun pack ->
         find_package pack.pack_packages name)

(*
 * Load a package.
 * We search for the description, and load it.
 *)
let load_aux arg pack_info =
   let { pack_info = pack_entry; pack_name = name; _ } = pack_info in
      State.write pack_entry (fun pack ->
            let { pack_cache = cache; _ } = pack in
               try
                  let info = Cache.StrFilterCache.load cache arg name ImplementationType AnySuffix in
                  let pack_str =
                     { pack_str_info = info;
                       pack_parse = arg
                     }
                  in
                     pack_info.pack_str <- Some pack_str;
                     pack_info.pack_infixes <- Cache.StrFilterCache.all_infixes info;
                     add_package pack pack_info;
                     Cache.StrFilterCache.set_mode info InteractiveSummary
               with
                  Not_found
                | Sys_error _ ->
                     raise (Failure (sprintf "Package_info.load: '%s' not found" name)))

(*
 * Make sure the str info is valid.
 *)
let auto_loading_str arg pack_info f =
   synchronize_node pack_info (function
      { pack_str = Some _; _ } ->
         f pack_info
    | { pack_str = None; _ } ->
         load_aux arg pack_info;
         f pack_info)

let auto_load_str arg pack_info =
   auto_loading_str arg pack_info (fun pack_info -> ())

(*
 * Load the package if it is not loaded already.
 *)
let load pack_entry arg name =
   synchronize_pack pack_entry (fun pack ->
         let pack_info =
            try get_package pack_entry name with
               NotLoaded _ ->
                  { pack_info = pack_entry;
                    pack_status = PackUnmodified;
                    pack_name = name;
                    pack_sig_info = None;
                    pack_str = None;
                    pack_infixes = Infix.Set.empty
                  }
         in
            auto_load_str arg pack_info;
            pack_info)

let get = get_package

(*
 * Package listings.
 *)
let compare pack1 pack2 =
   Stdlib.compare pack1.pack_name pack2.pack_name

let modified_packages pack =
   synchronize_pack pack (fun { pack_modified = modified; _ } ->
         List.sort compare modified)

let loaded_packages pack =
   synchronize_pack pack (fun { pack_packages = weak; _ } ->
         let len = Weak.length weak in
         let rec collect packages i =
            if i = len then
               packages
            else
               let packages =
                  match Weak.get weak i with
                     Some pack ->
                        pack :: packages
                   | None ->
                        packages
               in
                  collect packages (i + 1)
         in
            List.sort compare (collect [] 0))

(************************************************************************
 * DESTRUCTION                                                          *
 ************************************************************************)

(*
 * Get the name/status of the package.
 *)
let name pack = pack.pack_name
let status pack = pack.pack_status

(*
 * Get the filename for the package.
 *)
let filename pack_info arg =
   auto_loading_str arg pack_info (function
      { pack_info = pack_entry; pack_str = Some { pack_str_info = info; _ }; _ } ->
         State.write pack_entry (fun { pack_cache = cache; _ } ->
               Cache.StrFilterCache.filename cache info)
    | { pack_name = name; pack_str = None; _ } ->
         raise (NotLoaded name))

(*
 * Get the items in the module.
 *)
let info pack_info arg =
   auto_loading_str arg pack_info (function
      { pack_str = Some { pack_str_info = info; _ }; _ } ->
         Cache.StrFilterCache.info info
    | { pack_str = None; pack_name = name; _ } ->
         raise (NotLoaded name))

let sig_info_aux = function
   { pack_sig_info = Some info; _ } ->
      info
 | { pack_sig_info = None; pack_str = Some { pack_str_info = str_info; pack_parse = arg }; _ } as pack ->
      let sig_info = Cache.StrFilterCache.sig_info str_info arg InterfaceType in
         pack.pack_sig_info <- Some sig_info;
         sig_info
 | { pack_sig_info = None; pack_str = None; pack_name = name; _ } ->
      raise (NotLoaded name)

let sig_info pack_info arg =
   try synchronize_node pack_info sig_info_aux with
      NotLoaded _ ->
         auto_loading_str arg pack_info sig_info_aux

let find pack_info arg name =
   auto_loading_str arg pack_info (function
      { pack_str = Some { pack_str_info = info; _ }; _ } ->
         fst (Cache.StrFilterCache.find info name)
    | { pack_str = None; pack_name = name; _ } ->
         raise (NotLoaded name))

let set pack_info arg item =
   auto_loading_str arg pack_info (function
      { pack_str = Some { pack_str_info = info; _ }; _ } ->
         Cache.StrFilterCache.set_command info (item, dummy_loc)
    | { pack_str = None; pack_name = name; _ } ->
         raise (NotLoaded name))

(*
 * Access to cache.
 *)
let get_parsing_state pack_info =
   synchronize_node pack_info (fun pack_info ->
         match pack_info.pack_str with
            Some { pack_str_info = info; _ } ->
               Cache.StrFilterCache.get_parsing_state info
          | None ->
               raise (NotLoaded pack_info.pack_name))

(*
 * Grammar.
 *)
let get_start pack_info =
   synchronize_node pack_info (fun pack_info ->
          match pack_info.pack_str with
            Some { pack_str_info = info; _ } ->
               Cache.StrFilterCache.get_start info
          | None ->
               raise (NotLoaded pack_info.pack_name))

let get_infixes = function
   { pack_str = Some _; pack_infixes = infixes; _ } -> infixes
 | _ -> invalid_arg "Package_info.get_infixes: package not loaded"

(*
 * Get a loaded theory.
 *)
let groups pack =
   let f gr (dsc, _) res = (gr, dsc) :: res in
   let res = synchronize_pack pack (fun pack -> Hashtbl.fold f pack.pack_groups [])
   in List.sort Stdlib.compare res

let group_exists pack group =
   synchronize_pack pack (fun pack -> Hashtbl.mem pack.pack_groups group)

let group_packages pack group =
   synchronize_pack pack (fun pack ->
      let desc, thys = (Hashtbl.find pack.pack_groups group) in
         desc, LexStringSet.elements thys)

let group_of_module pack name =
   let f gr (_, set) res = if LexStringSet.mem set name then Some gr else res in
   let res = synchronize_pack pack (fun pack -> Hashtbl.fold f pack.pack_groups None)
   in match res with
         Some gr -> gr
       | None -> raise Not_found

(*
 * Three versions of saving.
 *)
let save_aux code arg pack_info =
   auto_loading_str arg pack_info (function
      { pack_str = Some { pack_str_info = info; _ }; _ } ->
         Cache.StrFilterCache.set_mode info InteractiveSummary;
         Cache.StrFilterCache.save info arg (OnlySuffixes [code]);
         set_status pack_info PackUnmodified
    | { pack_str = None; pack_name = name; _ } ->
         raise (NotLoaded name))

let export = save_aux "prla"
let save   = save_aux "prlb"
let backup = save_aux "cmoz"

(*
 * Revert a file the the .prlb backup
 *)
let revert pack_info =
   synchronize_node pack_info (function
         { pack_info = pack_entry; pack_str = Some str_info; _ } ->
            let { pack_str_info = info; pack_parse = parse_arg } = str_info in
               Cache.StrFilterCache.revert_proofs info parse_arg;
               set_status pack_info PackUnmodified
       | { pack_str = None; _ } ->
            eprintf "File is already reverted@.")

(*
 * Compeletly abandon changes.  You will lose all your work when
 * you do this.
 *)
let abandon pack_info =
   synchronize_node pack_info (fun pack ->
         (match pack.pack_str with
             Some { pack_str_info = info; _ } ->
                Cache.StrFilterCache.clear_info info;
                pack.pack_str <- None
           | None ->
                ());
         pack.pack_sig_info <- None;
         pack.pack_infixes <- Infix.Set.empty;
         set_status pack_info PackUnmodified)

(*
 * Create an empty package.
 *)
let create_package pack_entry arg name =
   synchronize_pack pack_entry (fun pack ->
         let info =
            { pack_info = pack_entry;
              pack_status = PackUnmodified;
              pack_sig_info = None;
              pack_str =
                 Some { pack_str_info = Cache.StrFilterCache.create_cache pack.pack_cache name ImplementationType;
                        pack_parse = arg
                 };
              pack_infixes = Infix.Set.empty;
              pack_name = name
            }
         in
            add_package pack info;
            set_status info PackModified;
            info)

(*
 * Look for a global_resource for an item
 *)
let find_bookmark mod_name item_name =
   try Mp_resource.find (mod_name, item_name) with
      Not_found ->
         eprintf "Warning: resources for %s.%s not found,\n\ttrying to use default resources for %s%t" (**)
            (String.capitalize mod_name) item_name mod_name eflush;
         try Mp_resource.find (Mp_resource.theory_bookmark mod_name) with
            Not_found ->
               raise (RefineError("Package_info/new_proof", StringError("can not find any resources")))

let arg_resource pack_info arg name =
   auto_loading_str arg pack_info (fun pack -> find_bookmark pack.pack_name name)

(*
 * A new proof cannot be saved.
 *)
let new_proof pack_info arg name hyps goal =
   auto_loading_str arg pack_info (function
      { pack_name = mod_name; _ } ->
         let sentinal = Tactic_type.Tactic.sentinal_of_refiner_object mod_name name in
         let bookmark = find_bookmark mod_name name in
         let seq = Tactic_type.Tactic.create sentinal (mk_msequent goal hyps) bookmark in
         let seq = Proof_initialize.initialize_arg seq in
         let proof = Proof.create seq in
         let ped = Proof_edit.ped_of_proof proof in
            ref (ProofEdit (arg, ped)))

(*
 * Get the status of the proof.
 *)
let status_of_proof proof =
   match !proof with
      ProofEdit (_, ped) ->
         Proof_edit.status_of_ped ped []
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
      { pack_name = name; _ } ->
         begin
            match !proof with
               ProofEdit (_, ped) ->
                  ped
             | ProofRaw (name', proof') ->
                  let parse, eval = arg in
                  let sentinal = Tactic_type.Tactic.sentinal_of_refiner_object name name' in
                  let bookmark = find_bookmark name name' in
                  let proof' = Proof.proof_of_io_proof [] sentinal bookmark parse eval proof' in
                  let ped = Proof_edit.ped_of_proof proof' in
                     Proof_edit.initialize_goal ped goal Proof_initialize.initialize_arg;
                     proof := ProofEdit (arg, ped);
                     ped
         end)

(* unused
let proof_of_ped proof arg ped =
   proof := ProofEdit (arg, ped);
   proof
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
