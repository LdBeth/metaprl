(*
 * Display a particular theory.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:03  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:32:48  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:24:54  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:26  jyh
 * This is an intermediate form of the editor with packages
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:03:55  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Rformat;;
open Dform;;
open PackageInfo;;

val display_package : string -> buffer -> packageInfo -> unit;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
