(*
 * Define the additional grammar for the shell.
 *)

open Refiner.Refiner.Term
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
 * Get the tactic for the last refinement.
 *)
val get_tactic : unit -> string * MLast.expr

(*
 * $Log$
 * Revision 1.4  1998/06/01 13:52:32  jyh
 * Proving twice one is two.
 *
 * Revision 1.3  1998/05/28 13:46:01  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
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
