(*
 * Compute relationships between theories.
 *)

open Refine;;
open PackageInfo;;
open DformPrint;;

(*
 * This type is used to record the theory DAG.
 *)
type packageNode =
   { pnode_package : packageInfo;
     pnode_children : packageNode list
   };;

(*
 * Compute diagrams.
 *)
val compute_roots : packageInfo list -> packageNode list;;
val compute_leaves : packageInfo list -> packageNode list;;
val compute_definition : packageInfo list -> packageNode list;;

(*
 * $Log$
 * Revision 1.2  1998/04/15 12:39:31  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.1  1997/08/06 16:17:15  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:17  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:25  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:51  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:35  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
