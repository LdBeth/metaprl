(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

include Tactic_type

open Printf
open Debug

open Term
open Opname
open Refine_sig
open Refine_util
open Refine_exn
open Refine

open Filter_proof_type

open Tactic_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Proof_step%t" eflush

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
     step_ast : MLast.expr;
     step_tactic : tactic
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Constructor.
 *)
let create goal subgoals text ast tac =
   { step_goal = goal;
     step_subgoals = subgoals;
     step_ast = ast;
     step_text = text;
     step_tactic = tac
   }

(*
 * Destructors.
 *)
let goal { step_goal = goal } = goal
let subgoals { step_subgoals = goals } = goals
let text { step_text = text } = text
let ast { step_ast = ast } = ast
let tactic { step_tactic = tac } = tac

(*
 * Make an error term.
 * Just a string.
 *)
let mk_error_subgoal { tac_hyps = hyps; tac_arg = arg } err =
   let s = string_of_refine_error err in
   let t = mk_string_term nil_opname s in
      { tac_goal = t;
        tac_hyps = hyps;
        tac_arg = arg
      }

(*
 * Apply the tactic and compute the extract.
 * Usually the subgoals will be exactly the same.
 * We use hash functions to make the search a little simpler.
 *
 * This function never fails.
 *)
let expand step =
   let { step_goal = goal;
         step_text = text;
         step_ast = ast;
         step_tactic = tac;
         step_subgoals = subgoals
       } = step
   in
      try
         let subgoals', _ = Refiner.refine tac goal in
            if List_util.for_all2 tactic_arg_alpha_equal subgoals' subgoals then
               step
            else
               { step_goal = goal;
                 step_text = text;
                 step_ast = ast;
                 step_tactic = tac;
                 step_subgoals = subgoals'
               }
      with
         RefineError err ->
            { step_goal = goal;
              step_text = text;
              step_ast = ast;
              step_tactic = tac;
              step_subgoals = [mk_error_subgoal goal err]
            }

let check step =
   let { step_goal = goal;
         step_tactic = tac;
         step_subgoals = subgoals
       } = step
   in
   let subgoals', ext = Refiner.refine tac goal in
      if List_util.for_all2 tactic_arg_alpha_equal subgoals' subgoals then
         ext
      else
         raise (RefineError (StringError "Proof_step.check: refinement mismatch"))

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
let step_of_io_step resources fcache tactics
    { Filter_proof_type.step_goal = goal;
      Filter_proof_type.step_subgoals = subgoals;
      Filter_proof_type.step_text = text;
      Filter_proof_type.step_ast = ast
    } =
   { step_goal = goal_of_aterm_tactic_arg resources fcache goal;
     step_subgoals = List.map (goal_of_aterm_tactic_arg resources fcache) subgoals;
     step_text = text;
     step_ast = ast;
     step_tactic = Hashtbl.find tactics text
   }

(*
 * $Log$
 * Revision 1.9  1998/04/24 02:41:33  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.8  1998/04/23 20:03:57  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.7  1998/04/22 22:44:20  jyh
 * *** empty log message ***
 *
 * Revision 1.6  1998/04/22 14:06:25  jyh
 * Implementing proof editor.
 *
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
