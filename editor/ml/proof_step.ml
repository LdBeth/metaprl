(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

include Io_proof_type
include Tacticals
include Proof_type

open Printf
open Debug
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Opname
open Dform
open Rformat
open Refine_exn

open Sequent
open Tacticals
open Tactic_type

open Io_proof_type
open Proof_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Proof_step%t" eflush

let debug_io_tactic =
   create_debug (**)
      { debug_name = "io_tactic";
        debug_description = "Display tactic lookups";
        debug_value = false
      }

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

(*
 * We use a memo function to convert to io_proofs.
 *)
type 'a denorm =
   { denorm : term -> 'a;
     proof_step_of_step : ('a denorm, t, 'a proof_step, 'a proof_step) Memo.t;
     aterm_of_tactic_arg : ('a denorm, tactic_arg, 'a aterm, 'a aterm) Memo.t;
     attributes_of_term_attributes : ('a denorm, term attributes, 'a attributes, 'a attributes) Memo.t
   }

(*
 * We use a memo function to convert to io_proofs.
 *)
type 'a norm =
   { norm : 'a -> term;
     global_args : term attributes;
     fcache : cache;
     sentinal : sentinal;
     tactics : (string, tactic) Hashtbl.t;
     step_of_proof_step : ('a norm, 'a proof_step, 'a proof_step, t) Memo.t;
     tactic_arg_of_aterm : ('a norm, 'a aterm, 'a aterm, tactic_arg) Memo.t;
     term_attributes_of_attributes : ('a norm, 'a attributes, 'a attributes, term attributes) Memo.t
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
let mk_error_subgoal db arg name err =
   let buf = new_buffer () in
   let _ = format_refine_error db buf name err in
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
         RefineError (name, err) ->
            { step_goal = goal;
              step_text = text;
              step_ast = ast;
              step_tactic = tac;
              step_subgoals = [mk_error_subgoal db goal name err]
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
 * IO PROOF CONVERSION                                                  *
 ************************************************************************)

(*
 * Comparison functions.
 *)
let compare_attributes list1 list2 =
   let compare (name1, att1) (name2, att2) =
      if name1 = name2 then
         match att1, att2 with
            TermArg t1, TermArg t2 ->
               t1 == t2
          | TypeArg t1, TypeArg t2 ->
               t1 == t2
          | IntArg i1, IntArg i2 ->
               i1 = i2
          | BoolArg b1, BoolArg b2 ->
               b1 = b2
          | SubstArg t1, SubstArg t2 ->
               t1 == t2
          | _ ->
               false
      else
         false
   in
      List_util.for_all2 compare list1 list2

let compare_aterm
    { aterm_goal = goal1;
      aterm_hyps = hyps1;
      aterm_label = label1;
      aterm_args = args1
    }
    { aterm_goal = goal2;
      aterm_hyps = hyps2;
      aterm_label = label2;
      aterm_args = args2
    } =
   (goal1 == goal2)
   & (List_util.compare_eq hyps1 hyps2)
   & (label1 = label2)
   & (List_util.compare_eq args1 args2)

let compare_step
    { Io_proof_type.step_goal = goal1;
      Io_proof_type.step_subgoals = subgoals1;
      Io_proof_type.step_text = text1
    }
    { Io_proof_type.step_goal = goal2;
      Io_proof_type.step_subgoals = subgoals2;
      Io_proof_type.step_text = text2
    } =
   (goal1 == goal2)
   & (text1 = text2)
   & (List_util.compare_eq subgoals1 subgoals2)

(*
 * Construction.
 *)
let rec make_attributes info = function
   [] ->
      []
 | (name, arg) :: tl ->
      let tl = make_attributes info tl in
         match arg with
            TermArg t ->
               (name, TermArg (info.denorm t)) :: tl
          | TypeArg t ->
               (name, TypeArg (info.denorm t)) :: tl
          | IntArg i ->
               (name, IntArg i) :: tl
          | BoolArg b ->
               (name, BoolArg b) :: tl
          | SubstArg t ->
               (name, SubstArg (info.denorm t)) :: tl
          | _ ->
               tl

let make_aterm info goal =
   let seq = Sequent.msequent goal in
   let t, hyps = dest_msequent seq in
   let label = Sequent.label goal in
   let args = Sequent.attributes goal in
      { Io_proof_type.aterm_goal = info.denorm t;
        Io_proof_type.aterm_hyps = List.map info.denorm hyps;
        Io_proof_type.aterm_label = label;
        Io_proof_type.aterm_args = Memo.apply info.attributes_of_term_attributes info args
      }

let make_proof_step info { step_goal = goal;
                           step_subgoals = subgoals;
                           step_text = text;
                           step_ast = ast
    } =
   { Io_proof_type.step_goal = Memo.apply info.aterm_of_tactic_arg info goal;
     Io_proof_type.step_subgoals =
        List.map (Memo.apply info.aterm_of_tactic_arg info) subgoals;
     Io_proof_type.step_ast = ast;
     Io_proof_type.step_text = text
   }

(*
 * Create the memo table.
 *)
let id _ x = x

let create_denorm denorm =
   { denorm = denorm;
     proof_step_of_step = Memo.create make_proof_step id compare_step;
     aterm_of_tactic_arg = Memo.create make_aterm id compare_aterm;
     attributes_of_term_attributes = Memo.create make_attributes id compare_attributes
   }

(*
 * Create the io step.
 *)
let io_step_of_step info step =
   Memo.apply info.proof_step_of_step info step

let aterm_of_tactic_arg info arg =
   Memo.apply info.aterm_of_tactic_arg info arg

let attributes_of_term_attributes info args =
   Memo.apply info.attributes_of_term_attributes info args

(*
 * Make the parts.
 *)
let make_step info { Io_proof_type.step_goal = goal;
                     Io_proof_type.step_subgoals = subgoals;
                     Io_proof_type.step_ast = ast;
                     Io_proof_type.step_text = text
    } =
   { step_goal = Memo.apply info.tactic_arg_of_aterm info goal;
     step_subgoals = List.map (Memo.apply info.tactic_arg_of_aterm info) subgoals;
     step_text = text;
     step_ast = ast;
     step_tactic =
        (if !debug_io_tactic then
            eprintf "Finding tactic '%s'%t" text eflush;
         let tac = Hashtbl.find info.tactics text in
            if !debug_io_tactic then
               eprintf "Found tactic%t" eflush;
            tac)
   }

let make_tactic_arg info { aterm_goal = goal;
                           aterm_hyps = hyps;
                           aterm_label = label;
                           aterm_args = args
    } =
   let { norm = norm; fcache = fcache; sentinal = sentinal } = info in
   let goal = norm goal in
   let hyps = List.map norm hyps in
   let args = Memo.apply info.term_attributes_of_attributes info args in
      Sequent.create sentinal label (mk_msequent goal hyps) fcache args

let rec make_term_attributes info = function
   [] ->
      info.global_args
 | (name, arg) :: tl ->
      let tl = make_term_attributes info tl in
         match arg with
            TermArg t ->
               (name, TermArg (info.norm t)) :: tl
          | TypeArg t ->
               (name, TypeArg (info.norm t)) :: tl
          | IntArg i ->
               (name, IntArg i) :: tl
          | BoolArg b ->
               (name, BoolArg b) :: tl
          | SubstArg t ->
               (name, SubstArg (info.norm t)) :: tl
          | _ ->
               tl

(*
 * Create the info.
 *)
let create_norm norm { ref_fcache = fcache; ref_args = args } tactics sentinal =
   { norm = norm;
     global_args = args;
     fcache = fcache;
     sentinal = sentinal;
     tactics = tactics;
     step_of_proof_step = Memo.create id make_step compare_step;
     tactic_arg_of_aterm = Memo.create id make_tactic_arg compare_aterm;
     term_attributes_of_attributes = Memo.create id make_term_attributes compare_attributes
   }

(*
 * Now do the conversion.
 *)
let step_of_io_step info step =
   Memo.apply info.step_of_proof_step info step

let tactic_arg_of_aterm info aterm =
   Memo.apply info.tactic_arg_of_aterm info aterm

let term_attributes_of_attributes info args =
   Memo.apply info.term_attributes_of_attributes info args

(*
 * $Log$
 * Revision 1.18  1998/07/03 22:05:19  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.17  1998/07/02 18:34:37  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
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
