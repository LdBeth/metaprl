(*
 * General purpose viewer.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:26  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.5  1996/10/23 15:17:52  jyh
 * First working version of dT tactic.
 *
 * Revision 1.4  1996/09/02 19:33:42  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:47  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:13  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:30  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Dform;;
open DformPrint;;
open ProofEdit;;
open PackageInfo;;

(*
 * This is raised when the object doesn't exist.
 *)
exception NoPackage of string list;;

(*
 * Commands for the top loop.
 *)
val print_term : dformModeBase -> term -> unit;;

(*
 * Navigation and display.
 *)
val cd : string -> string;;
val pwd : unit -> string;;
val view : string -> unit;;
val set_window_width : int -> unit;;

(*
 * Module commands.
 *)
val create_package : string -> unit;;
val install_package : packageInfo -> unit;;
val save : unit -> unit;;
val save_as : string -> unit;;
val save_all : unit -> unit;;

(*
 * Interactive commands to create objects.
 *)
val add_parent : string -> unit;;
val create_thm : string -> term -> unit;;
val move_up : unit -> unit;;
val move_down : int -> unit;;
val move_root : unit -> unit;;
val refine : string -> tactic -> unit;;
val undo : unit -> unit;;

(*
 * Debugging.
 *)
val pf : term ref;;
val z : tactic;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
