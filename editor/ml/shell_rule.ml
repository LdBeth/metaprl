(*
 * Create an ediable rewrite object.
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

include Shell_sig
include Package_info

open Printf
open Mp_debug

open Rformat
open Dform
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Opname

open Tactic_type
open Tactic_type.Tacticals

open Shell_sig
open Package_sig
open Package_info

open Filter_type
open Filter_summary
open Filter_cache
open Filter_ocaml

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell_rule%t" eflush

let debug_refine = load_debug "refine"

(*
 * This is the actual rewrite object.
 *)
type info =
   { mutable rule_params : term param list;
     mutable rule_assums : term list;
     mutable rule_goal : term;
     mutable rule_proof : Package.proof proof_type;
     mutable rule_ped : Proof_edit.ped proof_type;
     mutable rule_resources : MLast.expr resource_def
   }

(*
 * Make a rewrite goal from the assumptions,
 * and the rewrite.
 *)
let mk_rule_goal sentinal arg assums goal =
   let { ref_label = label; ref_args = args } = arg in
      Tactic.create sentinal label (mk_msequent goal assums) args

let mk_ped sentinal arg params assums goal =
   Proof_edit.create params (mk_rule_goal sentinal arg assums goal)

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Commenting function.
 *)
let comment _ _ t =
   t

(*
 * Build an item from the object.
 *)
let item_of_obj pack name
    { rule_params = params;
      rule_assums = assums;
      rule_proof = proof;
      rule_ped = ped;
      rule_goal = goal;
      rule_resources = res
    } =
   if params = [] & assums = [] then
      Filter_type.Axiom (**)
         { Filter_type.axiom_name = name;
           Filter_type.axiom_stmt = goal;
           Filter_type.axiom_proof = proof;
           Filter_type.axiom_resources = res
         }
   else
      Filter_type.Rule (**)
         { Filter_type.rule_name = name;
           Filter_type.rule_params = params;
           Filter_type.rule_stmt = zip_mimplies (assums @ [goal]);
           Filter_type.rule_proof = proof;
           Filter_type.rule_resources = res
         }

(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term (make_opname ["unit"]) []

let rec edit pack parse_arg sentinal arg name window obj =
   let edit_copy () =
      let { rule_params = params;
            rule_assums = assums;
            rule_proof = proof;
            rule_ped = ped;
            rule_goal = goal;
            rule_resources = res
          } = obj
      in
      let obj =
         { rule_params = params;
           rule_assums = assums;
           rule_proof = proof;
           rule_ped = ped;
           rule_goal = goal;
           rule_resources = res
         }
      in
         edit pack parse_arg sentinal arg name (Proof_edit.new_window window) obj
   in
   let update_ped () =
      obj.rule_ped <- Primitive unit_term
   in
   let save_ped () =
      let item = item_of_obj pack name obj in
         Package.set pack parse_arg item
   in
   let edit_display _ =
      (* Convert to a term *)
      let { rule_assums = assums;
            rule_goal = goal;
            rule_ped = ped
          } = obj
      in
         match ped with
            Primitive t ->
               let goal = mk_rule_goal sentinal arg assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Primitive goal)
          | Derived expr ->
               let goal = mk_rule_goal sentinal arg assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Derived (goal, expr))
          | Incomplete ->
               let goal = mk_rule_goal sentinal arg assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Incomplete goal)
          | Interactive ped ->
               Proof_edit.format window ped
   in
   let edit_set_goal t =
      obj.rule_goal <- t;
      update_ped ()
   in
   let edit_set_redex t =
      raise (RefineError ("Shell_rule.set_redex", StringError "can't set redex in rules"))
   in
   let edit_set_contractum t =
      raise (RefineError ("Shell_rule.set_contractum", StringError "can't set contractum in rules"))
   in
   let edit_set_assumptions tl =
      obj.rule_assums <- tl;
      update_ped ()
   in
   let edit_set_params pl =
      obj.rule_params <- pl;
      update_ped ()
   in
   let edit_save () =
      save_ped ()
(*
      match obj.rule_ped with
         Interactive ped ->
            save_ped ()
       | Primitive _
       | Derived _
       | Incomplete ->
            ()
*)
   in
   let edit_check () =
      match obj.rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check noninteractive proofs"))
       | Incomplete ->
            raise (RefineError ("Shell_rule.check", StringError "proof is incomplete"))
       | Interactive ped ->
            try Proof_edit.check_ped ped with
               RefineError (name', err) ->
                  raise (RefineError (name, GoalError (name', err)))
   in
   let get_ped obj =
      match obj.rule_ped with
         Primitive _
       | Derived _
       | Incomplete ->
            raise (RefineError ("Shell_rule.get_ped", StringError "proof is not interactive"))
       | Interactive ped ->
            ped
   in
   let edit_expand df =
      Proof_edit.expand_ped df (get_ped obj)
   in
   let edit_root () =
      Proof_edit.root_ped (get_ped obj)
   in
   let edit_up i =
      Proof_edit.up_ped (get_ped obj) i
   in
   let edit_down i =
      Proof_edit.down_ped (get_ped obj) i
   in
   let edit_undo () =
      Proof_edit.undo_ped (get_ped obj)
   in
   let edit_redo () =
      Proof_edit.redo_ped (get_ped obj)
   in
   let edit_nop () =
      Proof_edit.nop_ped (get_ped obj)
   in
   let get_ped () =
      match obj.rule_ped with
         Primitive _
       | Derived _
       | Incomplete ->
            (* Convert to a ped *)
            let { rule_params = params;
                  rule_assums = assums;
                  rule_goal = goal
                } = obj
            in
            let proof = Package.new_proof pack parse_arg name assums goal in
            let ped = Package.ped_of_proof pack parse_arg proof (mk_msequent goal assums) in
               obj.rule_proof <- Interactive proof;
               obj.rule_ped <- Interactive ped;
               Proof_edit.set_params ped params;
               save_ped ();
               ped
       | Interactive ped ->
            ped
   in
   let edit_info () =
      Proof_edit.edit_info_of_ped (get_ped ())
   in
   let edit_addr addr =
      Proof_edit.addr_ped (get_ped ()) addr
   in
   let edit_refine text ast tac =
      if !debug_refine then
         eprintf "Shell_rule.edit_refine: starting refinement%t" eflush;
      Proof_edit.refine_ped (get_ped ()) text ast tac;
      if !debug_refine then
         eprintf "Shell_rule.edit_refine: refinement done%t" eflush
   in
   let edit_interpret command =
      Proof_edit.interpret (get_ped ()) command
   in
      { edit_display = edit_display;
        edit_copy = edit_copy;
        edit_set_goal = edit_set_goal;
        edit_set_redex = edit_set_redex;
        edit_set_contractum = edit_set_contractum;
        edit_set_assumptions = edit_set_assumptions;
        edit_set_params = edit_set_params;
        edit_save = edit_save;
        edit_check = edit_check;
        edit_expand = edit_expand;
        edit_root = edit_root;
        edit_up = edit_up;
        edit_down = edit_down;
        edit_addr = edit_addr;
        edit_info = edit_info;
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_redo = edit_redo;
        edit_interpret = edit_interpret
      }

let create_window = function
   DisplayText (base, mode) ->
      Proof_edit.create_text_window base mode
 | DisplayGraphical (port, base) ->
      Proof_edit.create_proof_window port base

let create pack parse_arg window name =
   let rl =
      { Filter_type.axiom_name = name;
        Filter_type.axiom_stmt = unit_term;
        Filter_type.axiom_proof = Incomplete;
        Filter_type.axiom_resources = []
      }
   in
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = unit_term;
        rule_proof = Incomplete;
        rule_ped = Incomplete;
        rule_resources = []
      }
   in
   let sentinal = Package.sentinal pack in
   let arg = Package.argument pack parse_arg name in
      edit pack parse_arg sentinal arg name (create_window window) obj

let ped_of_proof pack parse_arg goal = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Incomplete ->
      Incomplete
 | Interactive proof ->
      Interactive (Package.ped_of_proof pack parse_arg proof goal)

let view_axiom pack parse_arg window
    { Filter_type.axiom_name = name;
      Filter_type.axiom_stmt = goal;
      Filter_type.axiom_proof = proof;
      Filter_type.axiom_resources = res
    } =
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = goal;
        rule_proof = proof;
        rule_ped = ped_of_proof pack parse_arg (mk_msequent goal []) proof;
        rule_resources = res
      }
   in
   let sentinal = Package.sentinal_object pack name in
   let arg = Package.argument pack parse_arg name in
      edit pack parse_arg sentinal arg name (create_window window) obj

let view_rule pack parse_arg window
    { Filter_type.rule_name = name;
      Filter_type.rule_params = params;
      Filter_type.rule_stmt = stmt;
      Filter_type.rule_proof = proof;
      Filter_type.rule_resources = res
    } =
   let assums, goal = unzip_mfunction stmt in
   let assums = List.map (fun (_, _, t) -> t) assums in
   let obj =
      { rule_assums = assums;
        rule_params = params;
        rule_goal = goal;
        rule_proof = proof;
        rule_ped = ped_of_proof pack parse_arg (mk_msequent goal assums) proof;
        rule_resources = res
      }
   in
   let sentinal = Package.sentinal_object pack name in
   let arg = Package.argument pack parse_arg name in
      edit pack parse_arg sentinal arg name (create_window window) obj

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
