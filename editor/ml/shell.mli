(*
 * General purpose toploop.
 *)

include Tacticals
include Package_info
include Package_df
include Shell_rewrite

open Refiner.Refiner.Term

open Tacticals

(*
 * Some initialization function.
 *)
val init : unit -> unit

(*
 * Navigation and display.
 *)
val cd : string -> string
val pwd : unit -> string
val set_window_width : int -> unit

(*
 * Module commands.
 *)
val load : string -> unit
val create_pkg : string -> unit
val set_writeable : unit -> unit
val save : unit -> unit
val save_all : unit -> unit

(*
 * The possible objects in a package.
 *)
val create_rw : string -> unit
val create_axiom : string -> unit
val create_thm : string -> unit
val create_opname : string -> unit
val create_condition : string -> unit
val create_parent : string -> unit
val create_dform : string -> unit
val create_prec : string -> unit
val create_prec_rul : string -> string -> string -> unit
val create_resource : string -> unit
val create_infix : string -> unit
val create_ml : string -> unit

(*
 * View, close, check object.
 * An object is not installed until it is checked.
 *)
val view : string -> unit
val ls : unit -> unit

(*
 * Editing commands.
 *)
val set_goal : term -> unit
val set_redex : term -> unit
val set_contractum : term -> unit
val set_assumptions : term list -> unit
val set_params : term Filter_summary.param list -> unit
val check : unit -> unit
val expand : unit -> unit

(*
 * Proof editing.
 *)
val root : unit -> unit
val up : int -> unit
val down : int -> unit
val refine : tactic -> unit
val undo : unit -> unit
val fold : unit -> unit
val fold_all : unit -> unit

(************************************************************************
 * DEBUGGING                                                            *
 ************************************************************************)

(*
 * $Log$
 * Revision 1.11  1998/07/03 22:05:22  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.10  1998/07/02 18:34:42  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.9  1998/06/15 22:31:50  jyh
 * Added CZF.
 *
 * Revision 1.8  1998/06/12 13:45:14  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.7  1998/06/01 13:52:28  jyh
 * Proving twice one is two.
 *
 * Revision 1.6  1998/05/28 13:45:57  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.5  1998/04/28 18:29:54  jyh
 * ls() works, adding display.
 *
 * Revision 1.4  1998/04/23 20:04:04  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.3  1998/04/17 20:48:15  jyh
 * Updating refiner for extraction.
 *
 * Revision 1.2  1998/04/17 01:30:50  jyh
 * Editor is almost constructed.
 *
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
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
