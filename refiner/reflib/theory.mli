(*
 * Simple mechanism for recording refiners and display forms.
 *
 *)

open Refiner.Refiner.Refine
open Dform_print

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_refiner : refiner;
     thy_dformer : dform_mode_base
   }

(* Save the theory *)
val record_theory : theory -> unit

(* Get back all the theories that have been recorded *)
val get_theories : unit -> theory list

(*
 * Get all the parent theories for a theory.
 * This is not guaranteed to be accurate.
 *)
val get_parents : theory -> theory list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
