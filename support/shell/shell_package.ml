(*
 * Create an ediable rewrite object.
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
extends Summary

open Refiner.Refiner.RefineError

open Filter_type
open Filter_shape
open Filter_summary_type

open Tactic_type

open Summary
open Shell_sig
open Shell_util

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Standard FilterOCaml module.
 *)
open Filter_summary
module FilterOCaml = Filter_ocaml.FilterOCaml (Refiner.Refiner)
module FilterSummaryTerm = FilterSummaryTerm (Refiner.Refiner)

open FilterOCaml
open FilterSummaryTerm

let identity x       = x
let term_of_expr     = term_of_expr []
let term_of_str_item = term_of_str_item []

(* unused
let convert_intf =
   let null_term    = mk_xstring_term "..." in
      { term_f      = identity;
        meta_term_f = term_of_meta_term;
        proof_f     = (fun _ _ -> null_term);
        resource_f  = FilterOCaml.term_of_resource_sig resource_op;
        ctyp_f      = term_of_type;
        expr_f      = term_of_expr;
        item_f      = term_of_sig_item
      }
*)

let convert_impl =
   let convert_proof _ = function
      Primitive t ->
         <:con<status_primitive{$t$}>>
    | Derived expr ->
         <<status_asserted>>
    | Incomplete ->
         <<status_partial>>
    | Interactive proof ->
         let status = term_of_proof_status (Package_info.status_of_proof proof) in
         let rcount, ncount = Package_info.node_count_of_proof proof in
            <:con<status_interactive[$int:rcount$, $int:ncount$]{$status$}>>
   in
      { term_f      = identity;
        meta_term_f = term_of_meta_term;
        proof_f     = convert_proof;
        resource_f  = FilterOCaml.term_of_resource_str resource_op;
        ctyp_f      = term_of_type;
        expr_f      = term_of_expr;
        item_f      = term_of_str_item
      }

(*
 * Display the entire package.
 *)
(* unused
let term_of_interface pack filter parse_arg =
   let tl = term_list convert_intf (Filter_summary.filter filter (Package_info.sig_info pack parse_arg)) in
      mk_interface_term tl
 *)

(*
 * Display the entire package.
 *)
let term_of_implementation pack filter parse_arg =
   let tl = term_list convert_impl (Filter_summary.filter filter (Package_info.info pack parse_arg)) in
      mk_implementation_term tl

(*
 * Filter the entries for ls.
 *)
let is_any_item _ =
   true

let is_rewrite_item = function
   Rewrite _
 | CondRewrite _
 | MLRewrite _ ->
      true
 | DefineTerm (shapeclass, _, _) ->
      is_shape_normal shapeclass
 | _ ->
      false

let is_rule_item = function
   Rule _
 | MLAxiom  _ ->
      true
 | _ ->
      false

let is_parent_item = function
   Parent _ ->
      true
 | _ ->
      false

let is_formal_item = function
   Rewrite _
 | CondRewrite _
 | MLRewrite _
 | Rule _
 | MLAxiom  _
 | DeclareTypeRewrite _ ->
      true
 | DeclareTypeClass (shapeclass, _, _, _)
 | DeclareType (shapeclass, _, _)
 | DefineTerm (shapeclass, _, _) ->
      is_shape_normal shapeclass
 | DeclareTerm (shapeclass, ty_term) ->
      is_shape_normal shapeclass && not (Perv.is_dform_type ty_term)
 | Parent _
 | SummaryItem _
 | Improve _
 | Resource _
 | InputForm _
 | Comment _
 | MagicBlock _
 | ToploopItem _
 | MLGramUpd _
 | Prec _
 | DForm _
 | Module _
 | Id _
 | PrecRel _
 | PRLGrammar _ ->
      false

let is_informal_item item =
   not (is_formal_item item)

let is_display_item = function
   MLGramUpd _
 | Prec _
 | DForm _
 | InputForm _ ->
      true
 | DeclareTerm (shapeclass, ty_term) ->
      is_shape_iform shapeclass || Perv.is_dform_type ty_term
 | DeclareTypeClass (shapeclass, _, _, _)
 | DeclareType (shapeclass, _, _)
 | DefineTerm (shapeclass, _, _) ->
      is_shape_iform shapeclass
 | _ ->
      false

let is_documentation = function
   Comment _ -> true
 | _ -> false

let is_unjustified_item = function
   Rewrite { rw_proof = proof; _ }
 | CondRewrite { crw_proof = proof; _ }
 | Rule { rule_proof = proof; _ } ->
      begin match proof with
         Primitive _
       | Derived _ ->
            false
       | Incomplete ->
            true
       | Interactive proof ->
            match Package_info.status_of_proof proof with
               Proof.StatusComplete ->
                  false
             | _ ->
                  true
      end
 | _ ->
      false

(*
 * Conjoin all the predicates.
 *)
let compile_ls_predicate pred (item, _) =
   List.exists (fun pred -> pred item) pred

let mk_ls_filter options =
   let predicate =
      LsOptionSet.fold (fun predicate option ->
            match option with
               LsAll ->
                  is_any_item :: predicate
             | LsRewrites ->
                  is_rewrite_item :: predicate
             | LsRules ->
                  is_rule_item :: predicate
             | LsUnjustified ->
                  is_unjustified_item :: predicate
             | LsParent ->
                  is_parent_item :: predicate
             | LsFormal ->
                  is_formal_item :: predicate
             | LsDisplay ->
                  is_display_item :: predicate
             | LsInformal ->
                  is_informal_item :: predicate
             | LsDocumentation ->
                  is_documentation :: predicate
             | LsFileAll
             | LsFileModifiers
             | LsHandles
             | LsLineNumbers
             | LsExternalEditor ->
                  predicate) [] options
   in
      compile_ls_predicate predicate

(************************************************************************
 * SHELL INTERFACE                                                      *
 ************************************************************************)

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_package", StringError s))

let raise_edit_error_fun s _ = raise_edit_error s

let edit_check_addr = function
   [] -> ()
 | _ -> raise (Invalid_argument "Shell_package.edit_check_addr")

(*
 * Build the shell interface.
 *)
let rec edit pack_info parse_arg get_dfm =
   let edit_display addr options =
      edit_check_addr addr;
      Proof_edit.display_term_newline (get_dfm ()) (term_of_implementation pack_info (mk_ls_filter options) parse_arg)
   in
   let edit_copy () =
      edit pack_info parse_arg get_dfm
   in
   let edit_save () =
      Package_info.save parse_arg pack_info
   in

   (*
    * This function always returns false.
    * However, it is wise to keep it because
    * we may add more methods.
    *)
   let edit_is_enabled _ = function
      MethodRefine
    | MethodPaste _
    | MethodUndo
    | MethodRedo
    | MethodExpand ->
         false
    | MethodApplyAll ->
         true
   in
   let not_a_rule _ =
      raise_edit_error "this is not a rule or rewrite"
   in
      { edit_display = edit_display;
        edit_get_contents = raise_edit_error_fun "can only retrieve contents of an individual item, not of a package";
        edit_get_terms = not_a_rule;
        edit_copy = edit_copy;
        edit_set_goal = not_a_rule;
        edit_set_redex = not_a_rule;
        edit_set_contractum = not_a_rule;
        edit_set_assumptions = not_a_rule;
        edit_set_params = not_a_rule;
        edit_get_extract = not_a_rule;
        edit_save = edit_save;
        edit_check = raise_edit_error_fun "check the entire package? Use check_all.";
        edit_check_addr = edit_check_addr;
        edit_info = raise_edit_error_fun "no info for the package";
        edit_undo = not_a_rule;
        edit_redo = not_a_rule;
        edit_interpret = raise_edit_error_fun "this is not a proof";
        edit_find = not_a_rule;
        edit_is_enabled = edit_is_enabled
      }

let create = edit
let view = edit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
