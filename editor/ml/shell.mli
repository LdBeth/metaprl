(*
 * General purpose toploop.
 *)

open Term

(*
 * Navigation and display.
 *)
val cd : string -> string
val pwd : unit -> string
val set_window_width : int -> unit

(*
 * Module commands.
 *)
val create_pkg : string -> unit
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
val check : string -> MLast.str_item
val close : unit -> unit

(*
 * Editing commands.
 *)
val set_goal : term -> unit
val set_redex : term -> unit
val set_contractum : term -> unit
val set_assumptions : term list -> unit
val set_params : param list -> unit

(*
 * Proof editing.
 *)
val move_up : unit -> unit
val move_down : int -> unit
val move_root : unit -> unit
val refine : MLast.expr -> tactic -> string -> unit
val undo : unit -> unit

(*
 * $Log$
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
