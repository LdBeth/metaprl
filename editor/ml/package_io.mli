(*
 * File IO for package definitions.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:20  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:21  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:28  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:53  jyh
 * This is an intermediate form of the editor with packages
 * before debugging.  Will be removing theoryGraph files next.
 *
 *)

open Term;;
open Dform;;
open ProofEdit;;
open PackageInfo;;

include Itt_theory;;

val save : packageInfo -> unit;;
val save_as : packageInfo -> string -> unit;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
