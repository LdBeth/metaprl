(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

include Io_proof_type
include Tactic_type
include Proof_type

open Printf
open Debug
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.RefineErrors
open Refiner.Refiner.Refine
open Opname
open Dform
open Rformat
open Refine_exn

open Io_proof_type
open Tactic_type
open Proof_type

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
let mk_error_subgoal db arg err =
   let buf = new_buffer () in
   let _ = format_refine_error db buf err in
   let t = mk_string_term nil_opname (print_to_string 80 buf) in
      set_concl arg t

(*
 * Apply the tactic and compute the extract.
 * Usually the subgoals will be exactly the same.
 * We use hash functions to make the search a little simpler.
 *
 * This function never fails.
 *)
let expand db step =
   let { step_goal = goal;
         step_text = text;
         step_ast = ast;
         step_tactic = tac;
         step_subgoals = subgoals
       } = step
   in
      try
         let subgoals', _ = refine tac goal in
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
              step_subgoals = [mk_error_subgoal db goal err]
            }

let check step =
   let { step_goal = goal;
         step_tactic = tac;
         step_subgoals = subgoals
       } = step
   in
   let subgoals', ext = refine tac goal in
      if List_util.for_all2 tactic_arg_alpha_equal subgoals' subgoals then
         ext
      else
         raise (RefineError ("Proof_step.check", StringError "refinement mismatch"))

(************************************************************************
 * BASE OPERATIONS                                                      *
 ************************************************************************)

(*
 * Throw away extra information from the goal.
 *)
let aterm_tactic_arg_of_goal arg =
   let { mseq_goal = goal; mseq_hyps = hyps } = Tactic_type.msequent arg in
      { aterm_goal = goal;
        aterm_hyps = hyps;
        aterm_label = Tactic_type.label arg;
        aterm_args = Tactic_type.attributes arg
      }

let goal_of_aterm_tactic_arg resources fcache
    { aterm_goal = goal;
      aterm_hyps = hyps;
      aterm_label = label;
      aterm_args = args
    } =
   Tactic_type.create label { mseq_goal = goal; mseq_hyps = hyps} fcache args resources

(*
 * Throw away information.
 *)
let io_step_of_step
    { step_goal = goal;
      step_subgoals = subgoals;
      step_text = text;
      step_ast = ast
    } =
   { Io_proof_type.step_goal = aterm_tactic_arg_of_goal goal;
     Io_proof_type.step_subgoals = List.map aterm_tactic_arg_of_goal subgoals;
     Io_proof_type.step_text = text;
     Io_proof_type.step_ast = ast
   }

(*
 * Add the resource information.
 *)
let step_of_io_step { ref_fcache = fcache; ref_rsrc = resources } tactics
    { Io_proof_type.step_goal = goal;
      Io_proof_type.step_subgoals = subgoals;
      Io_proof_type.step_text = text;
      Io_proof_type.step_ast = ast
    } =
   { step_goal = goal_of_aterm_tactic_arg resources fcache goal;
     step_subgoals = List.map (goal_of_aterm_tactic_arg resources fcache) subgoals;
     step_text = text;
     step_ast = ast;
     step_tactic = Hashtbl.find tactics text
   }

(*
 * $Log$
 * Revision 1.16  1998/07/01 04:36:28  nogin
 * Moved Refiner exceptions into a separate module RefineErrors
 *
 * Revision 1.15  1998/06/12 13:45:11  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.14  1998/06/09 20:51:17  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.13  1998/06/03 22:19:12  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.12  1998/06/01 13:52:23  jyh
 * Proving twice one is two.
 *
 * Revision 1.11  1998/05/28 13:45:53  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.10  1998/04/28 18:29:50  jyh
 * ls() works, adding display.
 *
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
