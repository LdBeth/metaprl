(*
 * Add some features to the display form mechanism.
 * We want a default dform base for debugging purposes.
 *
 *)

open Rformat
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Dform

(*
 * We abstract the base a little to provide "modes" of
 * display forms.  Each mode has a string name.  The mode
 * "all" adds to all the modes at once.
 *)
type dform_mode_base

val null_mode_base : dform_mode_base

(*
 * Get a particular version of the base.
 *)
val get_mode_base : dform_mode_base -> string -> dform_base

(*
 * Join two bases.
 *)
val join_mode_base : dform_mode_base ref -> dform_mode_base -> unit

(*
 * Add a dform to the mode base, with a given mode.
 *)
val create_dform : dform_mode_base ref -> string list -> dform_info -> unit

(*
 * Destruction.
 *)
val is_null_mode_base : dform_mode_base -> bool
val equal_mode_bases : dform_mode_base -> dform_mode_base -> bool
val dest_mode_base : dform_mode_base -> dform_base * ((string * dform_base) list)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
