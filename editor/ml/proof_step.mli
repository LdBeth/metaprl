(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

open Term
open Filter_proof_type

include Tactic_type

(* Abstract type for steps *)
type t

(* Constructor *)
val create :
       tactic_arg ->            (* Goal *)
       tactic_arg list ->       (* Subgoals *)
       string ->                (* Text in rule box *)
       MLast.expr ->            (* Parsed ML expression *)
       t
     
(* Destructors *)
val step_goal : t -> tactic_arg
val step_subgoals : t -> tactic_arg list
val step_text : t -> string
val step_ast : t -> MLast.expr

(* IO *)
val io_step_of_step : t -> proof_step
val step_of_io_step : tactic_resources -> cache -> proof_step -> t

(*
 * $Log$
 * Revision 1.4  1998/04/17 01:30:48  jyh
 * Editor is almost constructed.
 *
 * Revision 1.3  1998/04/13 21:10:57  jyh
 * Added interactive proofs to filter.
 *
 * Revision 1.2  1998/04/09 19:07:29  jyh
 * Updating the editor.
 *
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
