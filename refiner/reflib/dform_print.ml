(*
 * Add some features to the display form mechanism.
 * We want a default dform base for debugging purposes.
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

open Printf
open Mp_debug

open Dform

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Dform_print%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type dform_base = {
   mutable base_list : dform_info list;
   mutable base_includes : (int * dform_info list) list;
   mutable base_cached : Dform.dform_base option
}

(*
 * The mode base is just an association list.
 * We also keep a base that just includes the "all" forms, in
 * case a new mode is created.
 *)
type dform_mode_base =
   { all_base : dform_base;
     mode_bases : (string * dform_base) list
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty mode base.
 *)
let null_base = { base_list = []; base_includes = []; base_cached = None}
let null_mode_base = { all_base = null_base; mode_bases = [] }

let finish_base =
   let count = ref 0 in fun base ->
      if base.base_list != [] then
         base.base_includes <- (!count, base.base_list) :: base.base_includes;
         base.base_list <- [];
         incr count

(*
 * Get a particular mode base.
 *)
let get_mode_base mbase name =
   let base =
      if (name = "raw") then null_base else
      try List.assoc name mbase.mode_bases with
         Not_found ->
            mbase.all_base
   in match base.base_cached with
      Some base -> base
    | None ->
         let base' =
            finish_base base;
            create_dfbase (List_util.flat_map snd base.base_includes)
         in
            base.base_cached <- Some base';
            base'

let add_dform base info = {
   base_list = info :: base.base_list;
   base_includes = base.base_includes;
   base_cached = None
}

let join_dforms local_base parent_base =
   finish_base local_base;
   finish_base parent_base;
   let incl = local_base.base_includes in
   let parincl = List.filter (fun (x,_) -> not (List.mem_assoc x incl)) parent_base.base_includes in
   {
      base_list = [];
      base_includes = parincl @ incl;
      base_cached = None
   }

(*
 * Join all the bases.  Create a new mode for
 * all the modes in either base.
 *)
let join_mode_base base1 base2 =
   let { all_base = all1; mode_bases = bases1 } = !base1 in
   let { all_base = all2; mode_bases = bases2 } = base2 in
   let names = List_util.union (List.map fst bases1) (List.map fst bases2) in
   let get_base1 name = try List.assoc name bases1 with Not_found -> all1 in
   let get_base2 name = try List.assoc name bases2 with Not_found -> all2 in
   let join_mode_base name =
      name, join_dforms (get_base1 name) (get_base2 name)
   in
   let b =
      { all_base = join_dforms all1 all2;
        mode_bases = List.map join_mode_base names
      }
   in
   let b' =
      try List.assoc "prl" b.mode_bases with
         Not_found -> b.all_base
   in
      base1 := b

(*
 * A new form is added to a specific collection of modes.
 *)
let create_dform all include_mode mode_names b info =
   (* See if any new modes are created *)
   let { all_base = all_base; mode_bases = mode_bases } = !b in
   let rec compute_new_mode_bases mode_bases' = function
      "raw" :: _ ->
         raise (Invalid_argument("Dform_print.create_dform: raw mode can not have display forms"))
    | mode::modes ->
         let mode_bases'' =
            try List.assoc mode mode_bases'; mode_bases' with
               Not_found -> (mode, all_base)::mode_bases'
         in
            compute_new_mode_bases mode_bases'' modes
    | [] ->
         mode_bases'
   in
   let mode_bases' = compute_new_mode_bases mode_bases mode_names in

   (* Conditional addition *)
   let cond_add_base ((name, base) as entry) =
      if include_mode name then
         name, add_dform base info
      else
         entry
   in
   b:=
      (* Conditionally add to all the modes *)
      { all_base = if all then add_dform all_base info else all_base;
        mode_bases = List.map cond_add_base mode_bases'
      }

let create_dform_modes l =
   create_dform false (fun n -> List.mem n l) l

let create_dform_except_modes l =
   create_dform true (fun n -> not (List.mem n l)) l

let create_dform_all =
   create_dform true (fun _ -> true) []

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
