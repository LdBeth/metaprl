(*
 * Add some features to the display form mechanism.
 * We want a default dform base for debugging purposes.
 *
 *)

open Printf
open Nl_debug

open Dform

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Dform_print%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

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
let null_mode_base = { all_base = null_base; mode_bases = [] }

(*
 * Get a particular mode base.
 *)
let get_mode_base { all_base = all; mode_bases = bases } name =
   try List.assoc name bases with
      Not_found ->
         all

(*
 * Destruct the base.
 *)
let is_null_mode_base { all_base = all; mode_bases = bases } =
   is_null_dfbase all & bases = []

let equal_mode_bases
   { all_base = all1; mode_bases = bases1 }
   { all_base = all2; mode_bases = bases2 }
   =
   let labels1 = List.map fst bases1 in
   let labels2 = List.map fst bases2 in
   let equal_mode_base name =
      let base1 = List.assoc name bases1 in
      let base2 = List.assoc name bases2 in
         equal_dfbases base1 base2
   in
      equal_dfbases all1 all2 &
      (List_util.subtract labels1 labels2 = []) &
      (List_util.subtract labels2 labels1 = []) &
      List.for_all equal_mode_base labels1

let dest_mode_base { all_base = all; mode_bases = bases } =
   all, bases

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
let create_dform b modes info =
   (* See if any new modes are created *)
   let { all_base = all_base; mode_bases = mode_bases } = !b in
   let rec compute_new_mode_bases mode_bases' = function
      mode::modes ->
         let mode_bases'' =
            try List.assoc mode mode_bases'; mode_bases' with
               Not_found -> (mode, all_base)::mode_bases'
         in
            compute_new_mode_bases mode_bases'' modes
    | [] ->
         mode_bases'
   in
   let mode_bases' = compute_new_mode_bases mode_bases modes in

   (* Base addition *)
   let add_base (name, base) =
      name, add_dform base info
   in
   let cond_add_base ((name, base) as entry) =
      if List.mem name modes then
         name, add_dform base info
      else
         entry
   in
   let newbase =
      if modes = [] then
         (* Just add to all the modes *)
         { all_base = add_dform all_base info;
           mode_bases = List.map add_base mode_bases'
         }
      else
         (* Conditionalluy add to modes *)
         { all_base = all_base;
           mode_bases = List.map cond_add_base mode_bases'
         }
   in
      b := newbase

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
