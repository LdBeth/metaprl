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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Lm_debug
open Lm_printf

open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Opname

open Tactic_type

open Shell_sig

open Filter_type
open Filter_util
open Filter_summary_type

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Shell_rule%t"

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

let debug_refine = load_debug "refine"

type goal =
   GRule of term
 | GRewrite of term*term

(*
 * This is the actual rule/rewrite object.
 *)
type info =
   { mutable rule_params    : term param list;
     mutable rule_assums    : term list;
     mutable rule_goal      : goal;
     mutable rule_proof     : Package_info.proof proof_type;
     mutable rule_ped       : Proof_edit.ped proof_type;
     mutable rule_resources : (MLast.expr, term) resource_def;
     mutable rule_name      : string
   }

(*
 * Make a rule/rewrite goal from the assumptions, and the info goal.
 *)
let mk_rw_goal assums redex contractum =
   let rw = mk_rewrite_hack (mk_xrewrite_term redex contractum) in
   let assums = List.map mk_rewrite_hack assums in
      if !debug_shell then begin
         eprintf "Shell_rule.mk_rw_goal: [... --> ] %a <--> %a%t"
            print_term redex print_term contractum eflush;
      end;
      mk_msequent rw assums

let mk_bare_goal assums = function
   GRule goal ->
      Tactic.create Tactic.null_sentinal (mk_msequent goal assums) (Mp_resource.find Mp_resource.top_bookmark)
 | GRewrite (redex, contractum) ->
      Tactic.create Tactic.null_sentinal (mk_rw_goal assums redex contractum) (Mp_resource.find Mp_resource.top_bookmark)

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
   match goal with
      GRule goal ->
         Filter_type.Rule {
            Filter_type.rule_name = name;
            Filter_type.rule_params = params;
            Filter_type.rule_stmt = zip_mimplies assums goal;
            Filter_type.rule_proof = proof;
            Filter_type.rule_resources = res
         }
    | GRewrite (redex, contractum) when params = [] & assums = [] ->
         Filter_type.Rewrite {
            rw_name = name;
            rw_redex = redex;
            rw_contractum = contractum;
            rw_proof = proof;
            rw_resources = res
         }
    | GRewrite (redex, contractum) ->
         Filter_type.CondRewrite {
            crw_name = name;
            crw_params = params;
            crw_args = assums;
            crw_redex = redex;
            crw_contractum = contractum;
            crw_proof = proof;
            crw_resources = res
         }

(*
 * The object has a package in scope.
 *)
let unit_term = mk_simple_term nil_opname []

let mk_addr addr =
   try List.map int_of_string addr with
      Failure _ ->
         raise (Failure "Invalid proof node address (contains non-numerical elements)")

let rec edit pack parse_arg name window obj =
   let edit_copy () =
      edit pack parse_arg name window { obj with rule_name = obj.rule_name }
   in
   let update_ped () =
      obj.rule_ped <- Primitive unit_term
   in
   let save_ped () =
      let item = item_of_obj pack name obj in
         Package_info.set pack parse_arg item
   in
   let edit_display addr _ =
      (* Convert to a term *)
      let addr = mk_addr addr in
      let { rule_assums = assums; rule_goal = goal } = obj in
         match obj.rule_ped with
            Primitive t ->
               let goal = mk_bare_goal assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Primitive goal)
          | Derived expr ->
               let goal = mk_bare_goal assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Derived (goal, expr))
          | Incomplete ->
               let goal = mk_bare_goal assums goal in
                  Proof_edit.format_incomplete window (Proof_edit.Incomplete goal)
          | Interactive ped ->
               Proof_edit.format window ped addr
   in
   let edit_get_terms () =
      match obj.rule_goal with
         GRule goal ->
            goal :: obj.rule_assums
       | GRewrite (redex, contractum) ->
            redex :: contractum :: obj.rule_assums
   in
   let edit_set_goal t =
      obj.rule_goal <- GRule t;
      update_ped ()
   in
   let edit_set_redex t =
      match obj.rule_goal with
         GRule _ ->
            raise (RefineError ("Shell_rule.set_redex", StringError "can't set redex in rules"))
       | GRewrite (_, contractum) ->
            obj.rule_goal <- GRewrite (t, contractum);
            update_ped ()
   in
   let edit_set_contractum t =
      match obj.rule_goal with
         GRule _ ->
            raise (RefineError ("Shell_rule.set_contractum", StringError "can't set contractum in rules"))
       | GRewrite (redex, _) ->
            obj.rule_goal <- GRewrite (redex, t);
            update_ped ()
   in
   let edit_set_assumptions tl =
      obj.rule_assums <- tl;
      update_ped ()
   in
   let edit_set_params pl =
      obj.rule_params <- pl;
      update_ped ()
   in
   let edit_get_extract () =
      match obj.rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check primitive rules"))
       | Derived _ ->
            raise (RefineError ("Shell_rule.check", StringError "can't check noninteractive proofs"))
       | Incomplete ->
            raise (RefineError ("Shell_rule.check", StringError "proof is incomplete"))
       | Interactive ped ->
            try Proof_edit.refiner_extract_of_ped window ped with
               RefineError (name', err) ->
                  raise (RefineError (name, StringWrapError (name', err)))
   in
   let edit_check () =
      match obj.rule_ped with
         Primitive _ ->
            RefPrimitive
       | Derived _
       | Incomplete ->
            RefIncomplete (0, 0)
       | Interactive ped ->
            Proof_edit.check_ped window (Package_info.get_refiner pack) (make_opname [obj.rule_name; Package_info.name pack]) ped
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
   let edit_undo addr =
      List.map string_of_int (Proof_edit.undo_ped (get_ped obj) (mk_addr addr))
   in
   let edit_redo addr =
      List.map string_of_int (Proof_edit.redo_ped (get_ped obj) (mk_addr addr))
   in
   let edit_get_contents addr =
      if addr <> [] then
         raise (Invalid_argument "Shell_rule.edit_get_contents: only implemented for the root of the proof");
      let goal =
         match obj.rule_goal with
            GRule goal -> MetaTheorem goal
          | GRewrite (redex, contractum) -> MetaIff(MetaTheorem redex, MetaTheorem contractum)
      in
         obj.rule_name,
         Proof_edit.ped_status obj.rule_ped,
         List.fold_right (fun x y -> MetaImplies(MetaTheorem x,y)) obj.rule_assums goal,
         obj.rule_params
   in
   let get_ped () =
      match obj.rule_ped with
         Primitive _ ->
            raise (RefineError ("Shell_rule.get_ped", StringError "Editing of primitive rules is not currently allowed"))
       | Derived _
       | Incomplete ->
            let goal, assums =
               match obj.rule_goal with
                  GRule goal ->
                     goal, obj.rule_assums
                | GRewrite (redex, contractum) ->
                     dest_msequent (mk_rw_goal obj.rule_assums redex contractum)
            in
            let proof = Package_info.new_proof pack parse_arg name assums goal in
            let ped = Package_info.ped_of_proof pack parse_arg proof (mk_msequent goal assums) in
               obj.rule_proof <- Interactive proof;
               obj.rule_ped <- Interactive ped;
               save_ped ();
               ped
       | Interactive ped ->
            ped
   in
   let edit_info addr =
      Proof_edit.edit_info_of_ped (get_ped ()) (mk_addr addr)
   in
   let edit_check_addr addr =
      Proof_edit.check_addr_ped (get_ped ()) (mk_addr addr)
   in
   let edit_interpret addr command =
      Proof_edit.interpret window (get_ped ()) (mk_addr addr) command
   in
   let edit_find addr i =
      List.map string_of_int (Proof.find_subgoal (Proof_edit.proof_of_ped (get_ped ())) (mk_addr addr) i)
   in
      { edit_display = edit_display;
        edit_get_contents = edit_get_contents;
        edit_get_terms = edit_get_terms;
        edit_copy = edit_copy;
        edit_set_goal = edit_set_goal;
        edit_set_redex = edit_set_redex;
        edit_set_contractum = edit_set_contractum;
        edit_set_assumptions = edit_set_assumptions;
        edit_set_params = edit_set_params;
        edit_get_extract = edit_get_extract;
        edit_save = save_ped;
        edit_check = edit_check;
        edit_check_addr = edit_check_addr;
        edit_info = edit_info;
        edit_undo = edit_undo;
        edit_redo = edit_redo;
        edit_interpret = edit_interpret;
        edit_find = edit_find;
      }

let create pack parse_arg window name =
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = GRewrite (unit_term, unit_term);
        rule_proof = Incomplete;
        rule_ped = Incomplete;
        rule_resources = no_resources;
        rule_name = name
      }
   in
      edit pack parse_arg name window obj

let ped_of_proof pack parse_arg goal = function
   Primitive proof ->
      Primitive proof
 | Derived expr ->
      Derived expr
 | Incomplete ->
      Incomplete
 | Interactive proof ->
      Interactive (Package_info.ped_of_proof pack parse_arg proof goal)

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
        rule_goal = GRule goal;
        rule_proof = proof;
        rule_ped = ped_of_proof pack parse_arg (mk_msequent goal assums) proof;
        rule_resources = res;
        rule_name = name
      }
   in
      edit pack parse_arg name window obj

let view_rw pack parse_arg window
    { rw_name = name;
      rw_redex = redex;
      rw_contractum = contractum;
      rw_proof = proof;
      rw_resources = res;
    } =
   let obj =
      { rule_assums = [];
        rule_params = [];
        rule_goal = GRewrite (redex, contractum);
        rule_proof = proof;
        rule_ped = ped_of_proof pack parse_arg (mk_rw_goal [] redex contractum) proof;
        rule_resources = res;
        rule_name = name;
      }
   in
      edit pack parse_arg name window obj

let view_crw pack parse_arg window
    { crw_name = name;
      crw_params = params;
      crw_args = args;
      crw_redex = redex;
      crw_contractum = contractum;
      crw_proof = proof;
      crw_resources = res;
    } =
   let obj =
      { rule_assums = args;
        rule_params = params;
        rule_goal = GRewrite (redex, contractum);
        rule_proof = proof;
        rule_ped = ped_of_proof pack parse_arg (mk_rw_goal args redex contractum) proof;
        rule_resources = res;
        rule_name = name;
      }
   in
      edit pack parse_arg name window obj

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
