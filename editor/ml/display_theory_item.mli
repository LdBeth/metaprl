(*
 * Display an item in a theory.
 * This can be any of the types given in refine.csli.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:06  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.3  1996/05/21 02:25:09  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 16:59:39  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:05  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Rformat;;
open Refine;;
open Dform;;

val display_theory_item : dformBase -> buffer -> refinerItem -> unit;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
