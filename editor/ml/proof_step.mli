(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

open Term

include Tactic_type

(* Abstract type for steps *)
type t

(*
 * An out base compresses the steps for marshaling.
 * The in base collects them, and the handle maps
 * them to handles.
 *)
type handle
type out_base
type in_base

(* Constructor *)
val create :
       tactic_arg ->            (* Goal *)
       tactic_arg list ->       (* Subgoals *)
       string ->                (* Text in rule box *)
       Ast.expr ->              (* Parsed ML expression *)
       tactic ->                (* Keep the tactic too *)
       t
     
(* Destructors *)
val step_goal : t -> tactic_arg
val step_subgoals : t -> tactic_arg list
val step_text : t -> string
val step_ast : t -> Ast.expr
val step_tactic : t -> tactic

(* Saving proof steps *)
val create_out_base : unit -> out_base
val save_step : out_base -> t -> handle
val save_base : out_base -> out_channel -> unit

(*
 * Restoring proof steps.
 * The tactic expressions can be extracted separately
 * for compilation.
 *
 * restore_tactics can be compiled to a tactic array.
 *)
val restore_tactics : in_channel -> Ast.expr array
val restore_base : in_channel -> in_base
val restore_step : in_base -> tactic_resources -> tactic array -> handle -> t

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:25  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:39  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:43  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:11  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:27  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
