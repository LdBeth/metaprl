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
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 MetaPRL Group, Cornell University and Caltech
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
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Dform

open Tactic_boot_sig
open Tactic_type
open Tactic_type.Tactic

open Summary

open Shell_sig

let eprintf = Lm_printf.eprintf
let eflush = Lm_printf.eflush

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
 *)
type ped_proof = Proof.proof

type ped =
   { mutable ped_proof : ped_proof;
     mutable ped_undo : (ped_proof * Proof.address) list;
     mutable ped_redo : (ped_proof * Proof.address) list;
   }

type incomplete_ped =
   Primitive of tactic_arg
 | Incomplete of tactic_arg
 | Derived of tactic_arg * MLast.expr

(************************************************************************
 * CONVERSION TO TERMS                                                  *
 ************************************************************************)

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

let term_of_proof_arg addr pf =
   term_of_tactic_arg (Proof.path_status pf addr) (Proof.goal pf addr)

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
 | AddrArg a ->
      Summary.mk_addr_arg_term (dest_address a)

let term_of_arglist args =
   Summary.mk_arglist_term (List.map term_of_arg (Tactic.expand_arglist args))

let rec rule_term_of_text = function
   Proof.ExprGoal ->
      mk_rule_box_string_term "<goal>"
 | Proof.ExprIdentity ->
      mk_rule_box_string_term "<identity>"
 | Proof.ExprAnnotate ->
      mk_rule_box_string_term "<annotate>"
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
let term_of_proof pf addr =
   if !debug_edit then
      begin
         let buf = Lm_rformat.new_buffer () in
         let () = Proof.format_proof Dform.null_base buf pf in
         let prf = Lm_rformat_text.print_text_string 80 buf in
            eprintf "Proof_edit.term_of_proof: begin:\n%s%t" prf eflush
      end;

   let { Proof.step_goal = goal;
         Proof.step_status = status;
         Proof.step_expr = expr;
         Proof.step_subgoals = subgoals;
         Proof.step_extras = extras
       } = Proof.info pf addr
   in
   let main = term_of_proof_arg addr pf in
   let goal = mk_goal_list_term (List.map (term_of_proof_arg []) goal) in
   let subgoals = List.map (fun l -> mk_goal_list_term (List.map (term_of_proof_arg []) l)) subgoals in
   let extras = List.map (term_of_proof_arg []) extras in
   let subgoals =
      (* HACK!!! *)
      let l = List.length subgoals in
         if l < 20 || !debug_show_all_subgoals then
            mk_subgoals_term subgoals extras
         else
            mk_xlist_term [mk_subgoals_term (Lm_list_util.firstn 5 subgoals) [];
                           mk_string_arg_term "\n\n   ...   \n\n<<";
                           mk_int_arg_term l;
                           mk_string_arg_term " subgoals (output suppressed -- to see the full list run:  set_debug \"show_all_subgoals\" true )>>"]
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

(************************************************************************
 * OPERATIONS                                                           *
 ************************************************************************)

(*
 * Constructors.
 *)
let ped_of_proof proof =
   { ped_undo = [];
     ped_redo = [];
     ped_proof = proof;
   }

let create t =
   ped_of_proof (Proof.create t)

(*
 * Push a new proof into the ped.
 *)
let push_proof ped pf addr =
   if ped.ped_proof != pf then begin
      ped.ped_undo <- (ped.ped_proof, addr) :: ped.ped_undo;
      ped.ped_redo <- [];
      ped.ped_proof <- pf;
   end;
   true

(*
 * This is the function we pass to the proof module to track updates.
 *)
let update_fun ped addr pf =
   ignore (push_proof ped pf addr);
   let post () =
      ped.ped_proof
   in
      Proof.post post addr

(*
 * Replace the current proof.
 *)
(* unused
let set_proof ped proof =
   ped.ped_proof <- proof
 *)

(*
 * Destructors.
 *)
let proof_of_ped ped =
   ped.ped_proof

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
      begin match status_of_ped ped [] with
         Proof.StatusBad ->
            ObjBad
       | Proof.StatusIncomplete
       | Proof.StatusPartial ->
            let (c1,c2)=node_count_of_ped ped in ObjIncomplete(c1,c2)
       | Proof.StatusComplete ->
            let (c1,c2)=node_count_of_ped ped in ObjComplete(c1,c2)
      end

let rec str_expr = function
   Proof.ExprGoal ->
      "<goal>"
 | Proof.ExprIdentity ->
      "<identity>"
 | Proof.ExprAnnotate ->
      "<annotate>"
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

let edit_info_of_ped ped addr =
   let { Proof.step_goal = goal;
         Proof.step_expr = expr;
         Proof.step_subgoals = subgoals;
         Proof.step_extras = extras;
         _
       } = Proof.info (proof_of_ped ped) addr
   in
   let goal = List.hd goal in
   let subgoals = List.map List.hd subgoals in
   let get_goal node = Proof.goal node [] in
      { edit_goal = get_goal goal;
        edit_expr = str_expr expr;
        edit_subgoals = List.map get_goal subgoals;
        edit_extras = List.map get_goal extras
      }

(*
 * Set the goal term.
 *)
let set_goal ped mseq =
   let proof = proof_of_ped ped in
      ignore (push_proof ped (Proof.set_goal (update_fun ped []) proof [] mseq) [])

let initialize_goal ped mseq init =
   let proof = proof_of_ped ped in
      ignore (push_proof ped (Proof.initialize_goal (update_fun ped []) proof [] mseq init) [])

(*
 * Move down the undo stack.
 *)
let undo_ped ped addr =
   match ped.ped_undo with
      (proof, addr') :: proofs ->
         ped.ped_redo <- (ped.ped_proof, addr) :: ped.ped_redo;
         ped.ped_undo <- proofs;
         ped.ped_proof <- proof;
         addr'
    | [] ->
         raise (RefineError ("undo_ped", StringError "undo stack is empty"))

let redo_ped ped addr =
   match ped.ped_redo with
      (proof, addr') :: proofs ->
         ped.ped_undo <- (ped.ped_proof, addr) :: ped.ped_undo;
         ped.ped_redo <- proofs;
         ped.ped_proof <- proof;
         addr'
    | [] ->
         raise (RefineError ("redo_ped", StringError "all steps are already redone"))

let check_addr_ped ped addr =
   ignore (Proof.index (proof_of_ped ped) addr)

(*
 * We keep a global copy/paste buffer.
 *)
let copy_entry =
   let default = ref [] in
   let fork l = ref !l in
      State.private_val "Proof_edit.copy" default fork

let paste_is_enabled ped s =
   State.read copy_entry (fun copy_buffer ->
         List.mem_assoc s !copy_buffer)

let is_enabled_ped ped addr = function
   MethodRefine ->
      true
 | MethodApplyAll ->
      false
 | MethodPaste name ->
      paste_is_enabled ped name
 | MethodUndo ->
      ped.ped_undo <> []
 | MethodRedo ->
      ped.ped_redo <> []
 | MethodExpand ->
      begin match status_of_ped ped addr with
         Proof.StatusBad
       | Proof.StatusIncomplete ->
            true
       | Proof.StatusPartial
       | Proof.StatusComplete ->
            false
      end

(************************************************************************
 * Windowed operations.
 *)

(*
 * When the proof is expanded, we make a dulicate.
 * Expansion never fails, but it may change the status of the proof.
 *)
let handle_exn get_dfm =
   let dfm = (get_dfm ()).df_base in
      if Refine_exn.backtrace then
         fun f -> Some (f ())
      else
         fun f ->
            try Some (f ())
            with exn ->
               let buf = Lm_rformat.new_buffer () in
                  Filter_exn.format_exn dfm buf exn;
                  Lm_rformat.format_newline buf;
                  Lm_rprintf.output_rbuffer Lm_rprintf.stderr buf;
                  flush stderr;
                  None

let expand_ped window ped addr =
   push_proof ped (Proof.expand (update_fun ped addr) (handle_exn window) (proof_of_ped ped) addr) addr

(*
 * Check a proof.
 *)
let refiner_extract_of_ped window ped =
   let modified = (status_of_ped ped [] <> Proof.StatusComplete) && (expand_ped window ped []) in
   let extract = Proof.refiner_extract_of_proof (proof_of_ped ped) in
      modified, extract

let check_ped window refiner opname ped =
   let modified = (status_of_ped ped [] <> Proof.StatusComplete) && (expand_ped window ped []) in
   let status =
      match status_of_ped ped [] with
         Proof.StatusBad
       | Proof.StatusIncomplete
       | Proof.StatusPartial ->
            let c1, c2 = node_count_of_ped ped in
               RefIncomplete (c1, c2)
       | Proof.StatusComplete ->
            let c1, c2 = node_count_of_ped ped in
               try
                  RefComplete (c1, c2, Refine.compute_dependencies refiner opname)
               with
                  Refine_sig.Incomplete opname ->
                     RefUngrounded (c1, c2, opname)
                | Not_found ->
                     raise (RefineError ("Proof_edit.check_ped", StringStringError("could not find", string_of_opname opname)))
   in modified, status

(*
 * Command interpretation.
 *)
let interpret window ped addr = function
   ProofRefine (text, expr, tac) ->
      let proof = Proof.refine (update_fun ped addr) (proof_of_ped ped) addr text expr tac in
         push_proof ped proof addr
 | ProofKreitz ->
      push_proof ped (Proof.kreitz (update_fun ped addr) (proof_of_ped ped) addr) addr
 | ProofCopy s ->
      State.write copy_entry (fun copy_buffer ->
            let proof = Proof.index (proof_of_ped ped) addr in
               begin
                  try copy_buffer := Lm_list_util.assoc_replace !copy_buffer s proof with
                     Not_found ->
                        copy_buffer := (s, proof) :: !copy_buffer
               end);
      false
 | ProofPaste s ->
      let proof2 =
         State.read copy_entry (fun copy_buffer ->
               try List.assoc s !copy_buffer with
                  Not_found ->
                     raise (RefineError ("paste", StringStringError ("no proof in buffer", s))))
      in
      let proof1 = proof_of_ped ped in
      let proof = Proof.paste (update_fun ped addr) proof1 addr proof2 in
         push_proof ped proof addr;
 | ProofCp (from_addr, to_addr) ->
      let proof = proof_of_ped ped in
      let proof = Proof.copy (update_fun ped addr) proof from_addr to_addr in
         push_proof ped proof addr
 | ProofExpand ->
      expand_ped window ped addr
 | ProofMakeAssum ->
      push_proof ped (Proof.make_assum (update_fun ped addr) (proof_of_ped ped) addr) addr
 | ProofClean ->
      push_proof ped (Proof.clean (update_fun ped addr) (proof_of_ped ped) addr) addr
 | ProofSquash ->
      push_proof ped (Proof.squash (update_fun ped addr) (proof_of_ped ped) addr) addr

(************************************************************************
 * Display.
 *)

let display_term_aux newline dfm term =
   let buf = Lm_rformat.new_buffer () in
   match dfm.df_type with
      DisplayText ->
         Dform.format_term dfm.df_base buf term;
         if newline then Lm_rformat.format_newline buf;
         Lm_rformat_text.print_text_channel (Lm_termsize.term_width Stdlib.stdout dfm.df_width) buf stdout;
         flush stdout
    | DisplayTex ->
         let out = Shell_tex.open_file () in
            Dform.format_term dfm.df_base buf term;
            if newline then Lm_rformat.format_newline buf;
            Lm_rformat_tex.print_tex_channel dfm.df_width buf out;
            Shell_tex.close_file out
    | DisplayBrowser ->
         let df = save_slot_terms dfm.df_base in
            Dform.format_term df buf term;
            if newline then Lm_rformat.format_newline buf;
            Session.set_main buf (get_slot_terms df)

let display_term = display_term_aux false
let display_term_newline = display_term_aux true

let format get_dfm ped addr =
   display_term_newline (get_dfm()) (term_of_proof (proof_of_ped ped) addr)

let format_incomplete get_dfm proof =
   display_term_newline (get_dfm()) (term_of_incomplete proof)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
