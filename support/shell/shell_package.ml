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
extends Shell_sig
extends Shell_util
extends Package_info
extends Summary

open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

open Dform

open Filter_type
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

let convert_impl =
   let convert_proof _ = function
      Primitive t ->
         <:con<status_primitive{$t$}>>
    | Derived expr ->
         <<status_asserted>>
    | Incomplete ->
         <<status_partial>>
    | Interactive proof ->
         let status =
            match Package_info.status_of_proof proof with
               Proof.StatusBad ->
                  <<status_bad>>
             | Proof.StatusPartial ->
                  <<status_partial>>
             | Proof.StatusIncomplete ->
                  <<status_asserted>>
             | Proof.StatusComplete ->
                  <<status_complete>>
         in
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
let term_of_interface pack filter parse_arg =
   let tl = term_list convert_intf (Filter_summary.filter filter (Package_info.sig_info pack parse_arg)) in
      mk_interface_term tl

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
 | MLRewrite _
 | Definition _ ->
      true
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
 | Definition _
 | Opname _ ->
      true
 | Parent _
 | SummaryItem _
 | Improve _
 | Resource _
 | InputForm _
 | Comment _
 | MagicBlock _
 | ToploopItem _
 | GramUpd _
 | Prec _
 | DForm _
 | Module _
 | Id _
 | PrecRel _ ->
      false

let is_informal_item item =
   not (is_formal_item item)

let is_display_item = function
   Opname _
 | GramUpd _
 | Prec _
 | DForm _
 | InputForm _ ->
      true
 | _ ->
      false

let is_documentation = function
   Comment _ -> true
 | _ -> false

let is_unjustified_item = function
   Rewrite { rw_proof = proof }
 | CondRewrite { crw_proof = proof }
 | Rule { rule_proof = proof } ->
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

let edit_addr = function
   [] -> ()
 | _ -> raise (Invalid_argument "Shell_package.edit_addr")

(*
 * Build the shell interface.
 *)
let rec edit pack_info parse_arg get_dfm =
   let edit_display options =
      Proof_edit.display_term (get_dfm ()) (term_of_implementation pack_info (mk_ls_filter options) parse_arg)
   in
   let edit_copy () =
      edit pack_info parse_arg get_dfm
   in
   let edit_save () =
      Package_info.save parse_arg pack_info
   in
   let edit_fs_cwd () =
      "."
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
        edit_addr = edit_addr;
        edit_int_addr = edit_addr;
        edit_info = raise_edit_error_fun "no info for the package";
        edit_undo = not_a_rule;
        edit_redo = not_a_rule;
        edit_interpret = raise_edit_error_fun "this is not a proof";
        edit_find = not_a_rule;
        edit_fs_cwd = edit_fs_cwd
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
