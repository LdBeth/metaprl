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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
extends Summary

open Lm_debug
open Lm_thread

open Opname
open Refiner.Refiner
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Dform

open Tactic_type
open Tactic_type.TacticType
open Tactic_type.Tacticals

open Summary

open Shell_sig

let eprintf = Lm_printf.eprintf
let eflush = Lm_printf.eflush

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

let debug_show_all_subgoals =
   create_debug (**)
      { debug_name = "show_all_subgoals";
        debug_description = "show the full suggoals list, even when it is very long";
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
   { mutable ped_undo : ped_proof list;
     mutable ped_stack : ped_proof list
   }

type incomplete_ped =
   Primitive of tactic_arg
 | Incomplete of tactic_arg
 | Derived of tactic_arg * MLast.expr

(************************************************************************
 * CONVERSION TO TERMS                                                  *
 ************************************************************************)

(*
 * Turn the status into a char.
 *)
let term_of_proof_status = function
   Proof.StatusBad ->
      <<status_bad>>
 | Proof.StatusIncomplete ->
      <<status_asserted>>
 | Proof.StatusPartial ->
      <<status_partial>>
 | Proof.StatusComplete ->
      <<status_complete>>

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
 | TermListArg tl ->
      Summary.mk_term_list_arg_term tl

let term_of_arglist args =
   Summary.mk_arglist_term (List.map term_of_arg (Tactic.expand_arglist args))

let rec rule_term_of_text = function
   Proof.ExprGoal ->
      mk_rule_box_string_term "<goal>"
 | Proof.ExprIdentity ->
      mk_rule_box_string_term "<identity>"
 | Proof.ExprUnjustified ->
      mk_rule_box_string_term "<unjustified>"
 | Proof.ExprExtract args
 | Proof.ExprWrapped args ->
      mk_rule_box_term (term_of_arglist args)
 | Proof.ExprCompose expr ->
      append_rule_box (rule_term_of_text expr) "<then...>"
 | Proof.ExprRule (text, _) ->
      mk_rule_box_string_term text
(*
 * Display a proof with an inference.
 *)
let term_of_proof proof =
   if !debug_edit then
      begin
         let buf = Lm_rformat.new_buffer () in
         let () = Proof.format_proof Dform.null_base buf proof in
         let prf = Lm_rformat_text.print_text_string 80 buf in
            eprintf "Proof_edit.term_of_proof: begin:\n%s%t" prf eflush
      end;

   let { Proof.step_goal = goal;
         Proof.step_status = status;
         Proof.step_expr = expr;
         Proof.step_subgoals = subgoals;
         Proof.step_extras = extras
       } = Proof.info proof
   in
   let main = term_of_proof_arg proof in
   let goal = mk_goal_list_term (List.map term_of_proof_arg goal) in
   let subgoals = List.map (fun l -> mk_goal_list_term (List.map term_of_proof_arg l)) subgoals in
   let extras = List.map term_of_proof_arg extras in
   let subgoals =
      (* HACK!!! *)
      let l = List.length subgoals in
         if l < 20 || !debug_show_all_subgoals then
            mk_subgoals_term subgoals extras
         else
            mk_xlist_term [mk_subgoals_term (Lm_list_util.firstn 5 subgoals) [];
                           mk_string_arg_term "\n\n   ...   \n\n<<";
                           mk_int_arg_term l;
                           mk_string_arg_term " subgoals (output suppressed -- turn the \"show_all_subgoals\" debug variable on to see the full list)>>"]
   in
   let x = mk_proof_term main goal (term_of_proof_status status) (rule_term_of_text expr) subgoals in
      if !debug_edit then
         eprintf "Proof_edit.term_of_proof: done%t" eflush;
      x

(*
 * Show the incomplete proof.
 *)
let term_of_incomplete proof =
   let goal, status, text =
      match proof with
         Primitive goal ->
            if !debug_edit then
               eprintf "Proof_edit.term_of_incomplete: Primitive%t" eflush;
            let goal = term_of_tactic_arg [Proof.StatusComplete] goal in
            let text = mk_rule_box_string_term "<Primitive>" in
               goal, Proof.StatusComplete, text
       | Incomplete goal ->
            if !debug_edit then
               eprintf "Proof_edit.term_of_incomplete: Incomplete%t" eflush;
            let goal = term_of_tactic_arg [Proof.StatusIncomplete] goal in
            let text = mk_rule_box_string_term "<Incomplete>" in
               goal, Proof.StatusIncomplete, text
       | Derived (goal, expr) ->
            if !debug_edit then
               eprintf "Proof_edit.term_of_incomplete: Derived%t" eflush;
            let goal = term_of_tactic_arg [Proof.StatusComplete] goal in
            let text = mk_rule_box_string_term "<Derived>" in
               goal, Proof.StatusComplete, text
   in
      mk_proof_term goal (mk_goal_list_term [goal]) (term_of_proof_status status) text xnil_term

(*
 * Display the error.
 *)
let print_exn get_dfm f x =
   let dfm = get_dfm () in
      Filter_exn.print_exn (get_mode_base dfm.df_base dfm.df_mode) None f x

(************************************************************************
 * OPERATIONS                                                           *
 ************************************************************************)

(*
 * Constructors.
 *)
let ped_of_proof proof =
   let stack = [proof] in
      {
        ped_undo = stack;
        ped_stack = stack
      }

let create t =
   ped_of_proof (Proof.create t)

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

let ped_status = function
   Filter_summary_type.Primitive _ ->
      ObjPrimitive
 | Filter_summary_type.Derived _ ->
      ObjDerived
 | Filter_summary_type.Incomplete ->
      ObjIncomplete(0,0)
 | Filter_summary_type.Interactive ped ->
      begin match status_of_ped ped with
         Proof.StatusBad ->
            ObjBad
       | Proof.StatusIncomplete
       | Proof.StatusPartial ->
            let (c1,c2)=node_count_of_ped ped in ObjIncomplete(c1,c2)
       | Proof.StatusComplete ->
            let (c1,c2)=node_count_of_ped ped in ObjComplete(c1,c2)
      end

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

let rec str_expr = function
   Proof.ExprGoal ->
      "<goal>"
 | Proof.ExprIdentity ->
      "<identity>"
 | Proof.ExprUnjustified ->
      "<unjustified>"
 | Proof.ExprExtract arg ->
      "<extract>"
 | Proof.ExprCompose expr ->
      (str_expr expr) ^ " <then...>"
 | Proof.ExprWrapped arg ->
      "<wrapped>"
 | Proof.ExprRule (text, _) ->
      text

let edit_info_of_ped ped =
   let { Proof.step_goal = goal;
         Proof.step_expr = expr;
         Proof.step_subgoals = subgoals;
         Proof.step_extras = extras
       } = item_of_ped ped
   in
   let goal = List.hd goal in
   let subgoals = List.map List.hd subgoals in
      { edit_goal = Proof.goal goal;
        edit_expr = str_expr expr;
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
 * We keep a global copy/paste buffer.
 *)
let copy_entry =
   let default = ref [] in
   let fork l = ref !l in
      State.private_val "Proof_edit.copy" default fork

let copy_ped ped s =
   State.write copy_entry (fun copy_buffer ->
         let proof = proof_of_ped ped in
            begin
               try copy_buffer := Lm_list_util.assoc_replace !copy_buffer s proof with
                  Not_found ->
                     copy_buffer := (s, proof) :: !copy_buffer
            end)

let paste_ped ped s =
   let proof2 =
      State.read copy_entry (fun copy_buffer ->
            try List.assoc s !copy_buffer with
               Not_found ->
                  raise (RefineError ("paste", StringStringError ("no proof in buffer", s))))
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

(************************************************************************
 * Windowed operations.
 *)

(*
 * When the proof is expanded, we make a dulicate.
 * Expansion never fails, but it may change the status of the proof.
 *)
let expand_ped window ped =
   push_proof ped (Proof.expand (update_fun ped) (print_exn window) (proof_of_ped ped))

(*
 * Check a proof.
 *)
let refiner_extract_of_ped window ped =
   if status_of_ped ped <> Proof.StatusComplete then
      expand_ped window ped;
   Proof.refiner_extract_of_proof (proof_of_ped ped)

let check_ped window refiner opname ped =
   if status_of_ped ped <> Proof.StatusComplete then
      expand_ped window ped;
   match status_of_ped ped with
      Proof.StatusBad
    | Proof.StatusIncomplete
    | Proof.StatusPartial ->
         let c1, c2 = node_count_of_ped ped in
            RefIncomplete (c1, c2)
    | Proof.StatusComplete ->
         let proof = proof_of_ped ped in
         let c1, c2 = node_count_of_ped ped in
            try
               RefComplete (c1, c2, Refine.compute_dependencies refiner opname)
            with
               Refine_sig.Incomplete opname ->
                  RefUngrounded (c1, c2, opname)
             | Not_found ->
                  raise (RefineError ("Proof_edit.check_ped", StringStringError("could not find", string_of_opname opname)))

(*
 * Command interpretation.
 *)
let interpret window ped item =
   match item with
      ProofRefine (text, expr, tac) ->
         refine_ped ped text expr tac
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
    | ProofExpand ->
         expand_ped window ped
    | ProofMakeAssum ->
         make_assum_ped ped
    | ProofClean ->
         clean_ped ped
    | ProofSquash ->
         squash_ped ped

(************************************************************************
 * Display.
 *)

let display_term_aux newline dfm term =
   let df = get_mode_base dfm.df_base dfm.df_mode in
   let buf = Lm_rformat.new_buffer () in
   match dfm.df_type with
      DisplayText ->
         Dform.format_term df buf term;
         if newline then Lm_rformat.format_newline buf;
         let width = Mp_term.term_width Pervasives.stdout dfm.df_width in
         Lm_rformat_text.print_text_channel (Mp_term.term_width Pervasives.stdout dfm.df_width) buf stdout;
         flush stdout
    | DisplayTex ->
         let out = Shell_tex.open_file () in
            Dform.format_term df buf term;
            if newline then Lm_rformat.format_newline buf;
            Lm_rformat_tex.print_tex_channel dfm.df_width buf out;
            Shell_tex.close_file out
    | DisplayBrowser ->
         let df = save_slot_terms df in
            Dform.format_term df buf term;
            if newline then Lm_rformat.format_newline buf;
            Session.set_main buf (get_slot_terms df)

let display_term = display_term_aux false
let display_term_newline = display_term_aux true

let format get_dfm ped =
   display_term_newline (get_dfm()) (term_of_proof (proof_of_ped ped))

let format_incomplete get_dfm proof =
   display_term_newline (get_dfm()) (term_of_incomplete proof)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
