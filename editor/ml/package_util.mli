(*
 * Some utilities on modules.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:21  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:28  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:34  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 17:00:03  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 *)

open Refine;;

val item_name : refinerItem -> string;;
val unknown_item_name : refinerItem -> string;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
