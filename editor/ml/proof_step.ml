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
 * TACTIC CREATION                                                      *
 ************************************************************************)

(*
 * We create tactics through the toploop,
 * but it is delayed until the tactic is first evaluated.
 *)
type delayed_tactic =
   Delay of MLast.expr
 | Tactic of tactic

(*
 * Ref cell for returning the tactic value.
 *)
let inline_tactic = ref None

let install_tactic tac =
   inline_tactic := Some tac

(*
 * Evaluate a tactic through the toploop.
 *)
let eval_tactic_exn = RefineError ("eval_tactic", StringError "evaluation failed")

let eval_tactic expr =
   let loc = 0, 0 in
   let expr = (<:expr< $uid: "Proof_step"$ . $lid: "install_tactic"$ $expr$ >>) in
   let item = (<:str_item< $exp: expr$ >>) in
   let pt_item = Ast2pt.str_item item [] in
       inline_tactic := None;
       try
          if Toploop.execute_phrase false (Parsetree.Ptop_def pt_item) then
             match !inline_tactic with
                Some tac ->
                   tac
              | None ->
                   raise eval_tactic_exn
          else
             raise eval_tactic_exn
       with
          Typecore.Error (_, err) ->
             Typecore.report_error err;
             eflush stdout;
             raise eval_tactic_exn

(*
 * Build a delayed-evaluation tactic.
 *)
let create_tactic expr =
   let cell = ref (Delay expr) in
   let tac p =
      match !cell with
         Tactic tac ->
            tac p
       | Delay expr ->
            let tac = eval_tactic expr in
               cell := Tactic tac;
               tac p
   in
      tac

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
     step_tactic = create_tactic ast
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
let create_norm norm { ref_fcache = fcache; ref_args = args } sentinal =
   { norm = norm;
     global_args = args;
     fcache = fcache;
     sentinal = sentinal;
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
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
