(*
 * The proof editor constructs a proof interactively.
 * We provide a notion of a "current" address into the
 * proof, which is the point in the proof that is displayed
 * on the screen.
 *
 *
 * At the base level, this data structure just adds undo capability
 * to proofs, and in doing so, the operations become imperative.
 *
 * Also add display capability.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
include Summary

open Printf
open Mp_debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Rformat
open Dform
open Dform_print
open Simple_print

open Tactic_type
open Tactic_type.TacticType
open Tactic_type.Sequent
open Tactic_type.Tacticals

open Summary

open Display_term

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Proof_edit%t"

let debug_edit =
   create_debug (**)
      { debug_name = "edit";
        debug_description = "show editing commands";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The is the state of the current proof.
 *    ped_undo: current undo stack, modified by undo operations
 *    ped_stack: global undo stack, not modified by undo operations
 *
 * Current proof is at top of undo stack.
 *)
type ped_proof = Proof.proof

type ped =
   { mutable ped_params : term Filter_type.param list;
     mutable ped_undo : ped_proof list;
     mutable ped_stack : ped_proof list
   }

(*
 * Info for proof-type objects.
 *)
type edit_info =
   { edit_goal : tactic_arg;
     edit_expr : string;
     edit_subgoals : tactic_arg list;
     edit_extras : tactic_arg list
   }

(*
 * Possible commands.
 *)
type proof_command =
   ProofRefine of string * MLast.expr * tactic
 | ProofUnfold
 | ProofUndo
 | ProofRedo
 | ProofNop
 | ProofKreitz
 | ProofUp of int
 | ProofDown of int
 | ProofRoot
 | ProofAddr of int list
 | ProofRotate of int
 | ProofCopy of string
 | ProofPaste of string
 | ProofCp of int list * int list
 | ProofExpand of dform_base
 | ProofMakeAssum
 | ProofClean
 | ProofSquash

(************************************************************************
 * OPERATIONS                                                           *
 ************************************************************************)

(*
 * Constructors.
 *)
let ped_of_proof params proof =
   let stack = [proof] in
      { ped_params = params;
        ped_undo = stack;
        ped_stack = stack
      }

let create params t =
   ped_of_proof params (Proof.create t)

let set_params ped params =
   ped.ped_params <- params

(*
 * Push a new proof into the ped.
 *)
let push_proof ped proof =
   let stack = proof :: ped.ped_stack in
      ped.ped_undo <- stack;
      ped.ped_stack <- stack

(*
 * This is the function we pass to the proof module to track updates.
 *)
let update_fun ped proof =
   push_proof ped proof;
   let post () =
      List.hd ped.ped_undo
   in
      Proof.post post

(*
 * Replace the current proof.
 *)
let set_proof ped proof =
   ped.ped_undo <- proof :: List.tl ped.ped_undo

(*
 * Destructors.
 *)
let proof_of_ped { ped_undo = undo } =
   List.hd undo

let status_of_ped ped =
   Proof.status (proof_of_ped ped)

let node_count_of_ped ped =
   Proof.node_count (proof_of_ped ped)

let goal_of_ped ped =
   Proof.goal (proof_of_ped ped)

let item_of_ped ped =
   Proof.info (proof_of_ped ped)

let rotate_ped ped i =
   let { Proof.step_goal = goals } = Proof.info (proof_of_ped ped) in
   let len = List.length goals in
      if i < 1 || i > len then
         raise (RefineError ("rotate_ped", StringIntError ("argument is out of range", i)));
      push_proof ped (List.nth goals i)

let edit_info_of_ped ped =
   let { Proof.step_goal = goal;
         Proof.step_expr = expr;
         Proof.step_subgoals = subgoals;
         Proof.step_extras = extras
       } = item_of_ped ped
   in
   let expr =
      match expr with
         Proof.ExprGoal ->
            "<goal>"
       | Proof.ExprIdentity ->
            "<identity>"
       | Proof.ExprUnjustified ->
            "<unjustified>"
       | Proof.ExprExtract arg ->
            "<extract>"
       | Proof.ExprCompose ->
            "<compose>"
       | Proof.ExprWrapped arg ->
            "<wrapped>"
       | Proof.ExprRule (text, _) ->
            text
   in
   let goal = List.hd goal in
   let subgoals = List.map List.hd subgoals in
      { edit_goal = Proof.goal goal;
        edit_expr = expr;
        edit_subgoals = List.map Proof.goal subgoals;
        edit_extras = List.map Proof.goal extras
      }

(*
 * Set the goal term.
 *)
let set_goal ped mseq =
   let proof = proof_of_ped ped in
      push_proof ped (Proof.set_goal (update_fun ped) proof mseq)

(*
 * Move down the undo stack.
 *)
let undo_ped ped =
   let { ped_undo = undo } = ped in
      match undo with
         _ :: ((_ :: _) as proofs) ->
            ped.ped_undo <- proofs
       | _ ->
            raise (RefineError ("undo_ped", StringError "undo stack is empty"))

let redo_ped ped =
   let { ped_undo = undo; ped_stack = stack } = ped in
   let undo_length = List.length undo in
   let stack_length = List.length stack in
      if undo_length = stack_length then
         raise (RefineError ("redo_ped", StringError "all steps are already redone"));
      push_proof ped (List.nth stack (pred (stack_length - undo_length)))

(*
 * Reset the undo stack.
 *)
let nop_ped ped =
   ped.ped_undo <- ped.ped_stack

(*
 * Move to the `root' goal.
 *)
let root_ped ped =
   set_proof ped (Proof.root (proof_of_ped ped))

(*
 * Move to the parent goal.
 *)
let up_ped ped i =
   if i > 0 then
      let rec parent proof i =
         if i = 0 then
            proof
         else
            parent (Proof.parent proof) (pred i)
      in
         set_proof ped (parent (proof_of_ped ped) i)

(*
 * Move to a child.
 *)
let down_ped ped i =
   set_proof ped (Proof.child (proof_of_ped ped) i)

let addr_ped ped addr =
   set_proof ped (Proof.index (Proof.root (proof_of_ped ped)) addr)

(*
 * Unfold the extract at the current node.
 *)
let unfold_ped ped =
   push_proof ped (Proof.unfold (update_fun ped) (proof_of_ped ped))

(*
 * Refinement, and undo lists.
 * A finite number of undo's are allowed.
 * After a refine_ped or nop_ped, the undo stack gets reset.
 * The nop_ped does nothing but reset the undo stack.
 *)
let refine_ped ped text ast tac =
   let proof = proof_of_ped ped in
   let proof = Proof.refine (update_fun ped) proof text ast tac in
      push_proof ped proof

(*
 * Fold the current subgoals into a new proof node.
 *)
let kreitz_ped ped =
   push_proof ped (Proof.kreitz (update_fun ped) (proof_of_ped ped))

(*
 * Check a proof.
 *)
let check_ped ped =
   raise (Failure "Proof_edit.check_ped: not implemented")

(*
 * When the proof is expanded, we make a dulicate.
 * Expansion never fails, but it may change the status of the proof.
 *)
let expand_ped dforms ped =
   push_proof ped (Proof.expand (update_fun ped) dforms (proof_of_ped ped))

(*
 * We keep a global copy/paste buffer.
 *)
let copy_buffer = ref []
let copy_lock = Mutex.create ()

let copy_ped ped s =
   Mutex.lock copy_lock;
   let proof = proof_of_ped ped in
      begin
         try copy_buffer := List_util.assoc_replace !copy_buffer s proof with
            Not_found ->
               copy_buffer := (s, proof) :: !copy_buffer
      end;
      Mutex.unlock copy_lock

let paste_ped ped s =
   let proof2 =
      try List.assoc s !copy_buffer with
         Not_found ->
            raise (RefineError ("paste", StringStringError ("no proof in buffer", s)))
   in
   let proof1 = proof_of_ped ped in
   let proof = Proof.paste (update_fun ped) proof1 proof2 in
      push_proof ped proof

let cp_ped ped from_addr to_addr =
   let proof = proof_of_ped ped in
   let proof = Proof.copy (update_fun ped) proof from_addr to_addr in
      push_proof ped proof

let make_assum_ped ped =
   push_proof ped (Proof.make_assum (update_fun ped) (proof_of_ped ped))

let clean_ped ped =
   push_proof ped (Proof.clean (update_fun ped) (proof_of_ped ped))

let squash_ped ped =
   push_proof ped (Proof.squash (update_fun ped) (proof_of_ped ped))

(*
 * Command interpretation.
 *)
let interpret ped = function
   ProofRefine (text, expr, tac) ->
      refine_ped ped text expr tac
 | ProofUnfold ->
      unfold_ped ped
 | ProofUndo ->
      undo_ped ped
 | ProofRedo ->
      redo_ped ped
 | ProofNop ->
      nop_ped ped
 | ProofKreitz ->
      kreitz_ped ped
 | ProofUp i ->
      up_ped ped i
 | ProofDown i ->
      down_ped ped i
 | ProofRoot ->
      root_ped ped
 | ProofAddr addr ->
      addr_ped ped addr
 | ProofRotate i ->
      rotate_ped ped i
 | ProofCopy s ->
      copy_ped ped s
 | ProofPaste s ->
      paste_ped ped s
 | ProofCp (from_addr, to_addr) ->
      cp_ped ped from_addr to_addr
 | ProofExpand dforms ->
      expand_ped dforms ped
 | ProofMakeAssum ->
      make_assum_ped ped
 | ProofClean ->
      clean_ped ped
 | ProofSquash ->
      squash_ped ped

(************************************************************************
 * HTML DISPLAY                                                         *
 ************************************************************************)

(*
 * A window is either a text window or an HTML window.
 *)
type proof_window =
   { pw_port : Mux_channel.session;
     pw_base : dform_mode_base;
     pw_goal : Display_term.t;
     pw_rule : Display_term.t;
     pw_subgoals : Display_term.t;
     pw_menu : Display_term.t
   }

type text_window =
   { df_base : dform_mode_base;
     df_mode : string;
     df_width : int
   }

type window =
   ProofWindow of proof_window
 | TextWindow of text_window
 | TexWindow of text_window

type incomplete_ped =
   Primitive of tactic_arg
 | Incomplete of tactic_arg
 | Derived of tactic_arg * MLast.expr

(*
 * Create a new window.
 *)
let create_text_window base mode =
   TextWindow { df_base = base;
                df_mode = mode;
                df_width = 80
   }

let create_tex_window base =
   TexWindow { df_base = base;
               df_mode = "tex";
               df_width = 80
   }

let create_proof_window port dfbase =
   let { proof_goal = pw_goal;
         proof_rule = pw_rule;
         proof_subgoals = pw_subgoals
       } = Display_term.create_proof port dfbase
   in
   let pw_menu = Display_term.create_menu port dfbase in
   let window =
      { pw_port = port;
        pw_base = dfbase;
        pw_goal = pw_goal;
        pw_rule = pw_rule;
        pw_subgoals = pw_subgoals;
        pw_menu = pw_menu
      }
   in
      ProofWindow window

(*
 * Fork the current window.
 *)
let new_window = function
   ProofWindow { pw_port = port; pw_base = base } ->
      create_proof_window port base
 | (TextWindow _ | TexWindow _) as window ->
      window

(************************************************************************
 * CONVERSION TO TERMS                                                  *
 ************************************************************************)

(*
 * Turn the status into a char.
 *)
let term_of_proof_status = function
   Proof.StatusBad ->
      status_bad_term
 | Proof.StatusIncomplete ->
      status_asserted_term
 | Proof.StatusPartial ->
      status_partial_term
 | Proof.StatusComplete ->
      status_complete_term

let term_of_proof_status_list status =
   mk_status_term (List.map term_of_proof_status status)

(*
 * Label of the goal.
 *)
let term_of_tactic_arg status goal =
   let label = Sequent.label goal in
   let goal, assums = dest_msequent (Sequent.msequent goal) in
   let status = term_of_proof_status_list status in
   let label = mk_goal_label_term label in
      mk_goal_term status label assums goal

let term_of_proof_arg proof =
   term_of_tactic_arg (Proof.path_status proof) (Proof.goal proof)

(*
 * Turn an arglist into a string.
 *)
let term_of_arg = function
   TermArg t ->
      Summary.mk_term_arg_term t
 | TypeArg t ->
      Summary.mk_type_arg_term t
 | IntArg i ->
      Summary.mk_int_arg_term i
 | BoolArg b ->
      Summary.mk_bool_arg_term b
 | StringArg s ->
      Summary.mk_string_arg_term s
 | SubstArg t ->
      Summary.mk_subst_arg_term t
 | TermListArg tl ->
      Summary.mk_term_list_arg_term tl

let term_of_arglist args =
   Summary.mk_arglist_term (List.map term_of_arg (Tactic.expand_arglist args))

(*
 * Display a proof with an inference.
 *)
let term_of_proof proof =
   if !debug_edit then
      eprintf "Proof_edit.term_of_proof: begin%t" eflush;
   let { Proof.step_goal = goal;
         Proof.step_expr = expr;
         Proof.step_subgoals = subgoals;
         Proof.step_extras = extras
       } = Proof.info proof
   in
   let main = term_of_proof_arg proof in
   let goal = mk_goal_list_term (List.map term_of_proof_arg goal) in
   let subgoals = List.map (fun l -> mk_goal_list_term (List.map term_of_proof_arg l)) subgoals in
   let extras = List.map term_of_proof_arg extras in
   let text =
      match expr with
         Proof.ExprGoal ->
            mk_rule_box_string_term "<goal>"
       | Proof.ExprIdentity ->
            mk_rule_box_string_term "<identity>"
       | Proof.ExprUnjustified ->
            mk_rule_box_string_term "<unjustified>"
       | Proof.ExprExtract args ->
            mk_rule_box_term (term_of_arglist args)
       | Proof.ExprCompose ->
            mk_rule_box_string_term "<compose>"
       | Proof.ExprWrapped args ->
            mk_rule_box_term (term_of_arglist args)
       | Proof.ExprRule (text, _) ->
            mk_rule_box_string_term text
   in
   let subgoals = 
      (* HACK!!! *)
      let l = List.length subgoals in
      if l < 20 then mk_subgoals_term subgoals extras
      else mk_xlist_term [ mk_string_arg_term "\n\n<<"; mk_int_arg_term l; mk_string_arg_term " subgoals (output suppressed)>>"]
   in
   let x = mk_proof_term main goal text subgoals in
      if !debug_edit then
         eprintf "Proof_edit.term_of_proof: done%t" eflush;
      x

(*
 * Show the incomplete proof.
 *)
let term_of_incomplete proof =
   let goal, text =
      match proof with
         Primitive goal ->
            if !debug_edit then
               eprintf "Proof_edit.term_of_incomplete: Primitive%t" eflush;
            let goal = term_of_tactic_arg [Proof.StatusComplete] goal in
            let text = mk_rule_box_string_term "<Primitive>" in
               goal, text
       | Incomplete goal ->
            if !debug_edit then
               eprintf "Proof_edit.term_of_incomplete: Incomplete%t" eflush;
            let goal = term_of_tactic_arg [Proof.StatusIncomplete] goal in
            let text = mk_rule_box_string_term "<Incomplete>" in
               goal, text
       | Derived (goal, expr) ->
            if !debug_edit then
               eprintf "Proof_edit.term_of_incomplete: Derived%t" eflush;
            let goal = term_of_tactic_arg [Proof.StatusComplete] goal in
            let text = mk_rule_box_string_term "<Derived>" in
               goal, text
   in
      mk_proof_term goal (mk_goal_list_term [goal]) text xnil_term

(*
 * Display the current proof.
 *    0. Display the status
 *    1. Display the goal
 *    2. Display the rule
 *    3. Display the subgoals
 *)
let format_aux window proof =
   match window with
      TextWindow { df_width = width; df_base = dfbase; df_mode = mode } ->
         let df = get_mode_base dfbase mode in
         let buf = Rformat.new_buffer () in
            Dform.format_term df buf proof;
            Rformat.format_newline buf;
            Rformat.print_to_channel width buf stdout;
            flush stdout
    | TexWindow { df_width = width; df_base = dfbase; df_mode = mode } ->
         let df = get_mode_base dfbase mode in
         let buf = Rformat.new_buffer () in
            Dform.format_term df buf proof;
            Rformat.format_newline buf;
            Rformat.print_to_tex width buf stdout;
            flush stdout
    | ProofWindow { pw_goal = pw_goal;
                    pw_rule = pw_rule;
                    pw_subgoals = pw_subgoals
      } ->
         let main, goal, text, subgoals = dest_proof proof in
            if !debug_edit then
               eprintf "Proof_edit.format_aux: set_goal%t" eflush;
            Display_term.set pw_goal main;
            if !debug_edit then
               eprintf "Proof_edit.format_aux: set_rule%t" eflush;
            Display_term.set pw_rule text;
            if !debug_edit then
               eprintf "Proof_edit.format_aux: set_subgoals%t" eflush;
            Display_term.set pw_subgoals subgoals

let format window ped =
   format_aux window (term_of_proof (proof_of_ped ped))

let format_incomplete window proof =
   format_aux window (term_of_incomplete proof)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
