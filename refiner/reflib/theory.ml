(*
 * Simple recorder for all the theories in the system.
 *
 *)

open Printf
open Debug

open Refiner.Refiner
open Refiner.Refiner.Refine
open Dform_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Theory%t" eflush

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_refiner : refiner;
     thy_dformer : dform_mode_base
   }

(*
 * Save all the theories on a list.
 *)
let base = ref ([] : theory list)

(*
 * Record a theory by pushing it onto the list.
 *)
let record_theory thy =
   Ref_util.push thy base

(*
 * Get all the theories.
 *)
let get_theories () =
   !base

(*
 * Get the parents of a theory.
 *)
let get_parents thy =
   let rec find_parent parent = function
      thy :: tl ->
         let { thy_refiner = refiner } = thy in
            if refiner == parent then
               thy
            else
               find_parent parent tl
    | [] ->
         raise Not_found
   in
   let rec search refiner =
      if is_null_refiner refiner then
         []
      else
         let item, refiner' = dest_refiner refiner in
            match item with
               RIParent parent ->
                  begin
                     try find_parent parent !base :: search refiner' with
                        Not_found ->
                           search refiner'
                  end
             | _ ->
                  search refiner'
   in
   let { thy_refiner = refiner } = thy in
      search refiner

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
