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
 * $Log$
 * Revision 1.1  1998/05/28 15:00:47  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.3  1998/05/27 15:13:44  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.2  1998/04/28 18:30:38  jyh
 * ls() works, adding display.
 *
 * Revision 1.1  1997/04/28 15:51:18  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.3  1996/05/21 02:13:42  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/04/11 13:29:24  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.1  1996/04/07 18:27:06  jyh
 * Intermediate checking while updating dform commands.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
