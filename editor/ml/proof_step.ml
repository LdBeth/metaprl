(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

open Term
open Refine_sig

open Filter_proof_type

include Tactic_type

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * We keep all the info from the refiner,
 * plus the string representing the tactic.
 *)
type t =
   { step_goal : tactic_arg;
     step_subgoals : tactic_arg list;
     step_text : string;
     step_ast : MLast.expr
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Constructor.
 *)
let create goal subgoals text ast =
   { step_goal = goal;
     step_subgoals = subgoals;
     step_ast = ast;
     step_text = text
   }

(*
 * Destructors.
 *)
let step_goal { step_goal = goal } = goal
let step_subgoals { step_subgoals = goals } = goals
let step_text { step_text = text } = text
let step_ast { step_ast = ast } = ast

(************************************************************************
 * BASE OPERATIONS                                                      *
 ************************************************************************)

(*
 * Throw away extra information from the goal.
 *)
let aterm_tactic_arg_of_goal
    { tac_goal = goal;
      tac_hyps = hyps;
      tac_arg = { ref_label = label; ref_args = args; ref_fcache = fcache }
    } =
   { tac_goal = goal;
     tac_hyps = hyps;
     tac_arg = { aterm_label = label; aterm_args = args }
   }

let goal_of_aterm_tactic_arg resources fcache
    { tac_goal = goal;
      tac_hyps = hyps;
      tac_arg = { aterm_label = label; aterm_args = args }
    } =
   { tac_goal = goal;
     tac_hyps = hyps;
     tac_arg = { ref_label = label;
                 ref_args = args;
                 ref_fcache = fcache;
                 ref_rsrc = resources
               }
   }

(*
 * Throw away information.
 *)
let io_step_of_step 
    { step_goal = goal;
      step_subgoals = subgoals;
      step_text = text;
      step_ast = ast
    } =
   { Filter_proof_type.step_goal = aterm_tactic_arg_of_goal goal;
     Filter_proof_type.step_subgoals = List.map aterm_tactic_arg_of_goal subgoals;
     Filter_proof_type.step_text = text;
     Filter_proof_type.step_ast = ast
   }

(*
 * Add the resource information.
 *)
let step_of_io_step resources fcache
    { Filter_proof_type.step_goal = goal;
      Filter_proof_type.step_subgoals = subgoals;
      Filter_proof_type.step_text = text;
      Filter_proof_type.step_ast = ast
    } =
   { step_goal = goal_of_aterm_tactic_arg resources fcache goal;
     step_subgoals = List.map (goal_of_aterm_tactic_arg resources fcache) subgoals;
     step_text = text;
     step_ast = ast
   }

(*
 * $Log$
 * Revision 1.5  1998/04/21 20:57:57  jyh
 * Fixed typing problems introduced by refiner msequents.
 *
 * Revision 1.4  1998/04/17 01:30:47  jyh
 * Editor is almost constructed.
 *
 * Revision 1.3  1998/04/13 21:10:56  jyh
 * Added interactive proofs to filter.
 *
 * Revision 1.2  1998/04/09 19:07:27  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:24  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:37  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:42  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:10  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:25  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
