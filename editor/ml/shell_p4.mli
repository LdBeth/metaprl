(*
 * Define the additional grammar for the shell.
 *)

open Term
open Dform
open Opname

(*
 * Set the current display form base.
 *)
val set_df : dform_base option -> unit

(*
 * This is the opname function used when terms are built.
 *)
val set_mk_opname : (string list -> opname) option -> unit

(*
 * Printers.
 *)
val print_term : term -> unit

(*
 * $Log$
 * Revision 1.2  1998/04/28 18:29:57  jyh
 * ls() works, adding display.
 *
 * Revision 1.1  1998/04/23 20:04:17  jyh
 * Initial rebuilt editor.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
