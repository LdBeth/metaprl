(*
 * Define a resource for select arguments.
 * XXX: JYH: we want this resource to be scoped within a theory.
 * That will take a little extra work.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
extends Mptop
extends Proof_initialize

open Lm_printf

open Opname
open Tactic_boot_sig
open Refiner.Refiner.Refine
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.RefineError
open Mp_resource
open Top_resource
open Option_sig

open Tactic_type
open Tactic_type.Tacticals

(*
 * A rule is labeled with positive and negative arguments.
 *)
type rule_labels = OpnameSet.t

(*
 * The options are collected in a list.
 * For efficiency, squash the entries in the list when
 * the resource is extracted.
 *)
let add_data options (t, info) =
   (opname_of_term t, info) :: options

let extract_data options =
   let _, options =
      List.fold_left (fun (found, options) info ->
            match info with
               (opname, OptionIgnore) ->
                  OpnameSet.add found opname, options
             | (opname, _) ->
                  if OpnameSet.mem found opname then
                     found, options
                  else
                     let found = OpnameSet.add found opname in
                     let options = info :: options in
                        found, options) (OpnameSet.empty, []) options
   in
      List.rev options

let resource_info =
   Functional (**)
      { fp_empty    = [];
        fp_add      = add_data;
        fp_retr     = extract_data
      }

let resource (term * option_info, option_table) select =
   resource_info

(************************************************************************
 * Initialize a tactic_arg by loading the initial options from the
 * resource.
 *)
let initialize_arg p =
   let info = Sequent.get_resource_arg p get_select_resource in
      Sequent.set_option_args p info

let resource proof_initialize += initialize_arg

let get_options = Sequent.get_option_args

(************************************************************************
 * Check whether the options on the rule/rewrite are allowed.
 *
 * XXX: JYH: this could be more efficient.
 *)
let rec test_rule_labels options labels =
   match options with
      (opname, OptionAllow) :: options ->
         OpnameSet.mem labels opname || test_rule_labels options labels
    | (opname, OptionExclude) :: options ->
         if OpnameSet.mem labels opname then
            false
         else
            test_rule_labels options labels
    | (_, OptionIgnore) :: options ->
         test_rule_labels options labels
    | [] ->
         true

let rec rule_labels_are_allowed options labels =
   OpnameSet.is_empty labels || options = [] || test_rule_labels options labels

let rule_labels_are_allowed_arg p labels =
   rule_labels_are_allowed (get_options p) labels

(************************************************************************
 * Utilities.
 *)
let rule_labels_empty = OpnameSet.empty

let rule_labels_of_terms labels =
   List.fold_left (fun opnames t ->
         OpnameSet.add opnames (opname_of_term t)) OpnameSet.empty labels

let rule_labels_of_opt_terms labels =
   let labels =
      match labels with
         Some labels -> labels
       | None -> []
   in
      rule_labels_of_terms labels

let rule_labels_not_allowed loc labels =
   match labels with
      None ->
         ()
    | Some _ ->
         Stdpp.raise_with_loc loc (RefineError ("option check", StringError "rule labels are not allowed, or the annotation processor has not been updated"))

(************************************************************************
 * Tacticals for option handling.
 *)
let pp_print_option_info buf info =
   pp_print_string buf (string_of_option info)

let addOptionInfoT t info =
   Tacticals.addOptionT (opname_of_term t) info

let addOptionT t s =
   addOptionInfoT t (option_of_string s)

let allowOptionT t =
   addOptionInfoT t OptionAllow

let excludeOptionT t =
   addOptionInfoT t OptionExclude

let withOptionInfoT t info tac =
   Tacticals.withOptionT (opname_of_term t) info tac

let withOptionT t s =
   withOptionInfoT t (option_of_string s)

let withAllowOptionT t =
   withOptionInfoT t OptionAllow

let withExcludeOptionT t =
   withOptionInfoT t OptionExclude

let removeOptionT t =
   Tacticals.removeOptionT (opname_of_term t)

let withoutOptionT t tac =
   Tacticals.withoutOptionT (opname_of_term t) tac

let printOptionT t =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = get_options p in
         eprintf "Option: %s = " (string_of_opname opname);
         (try eprintf "%a@." pp_print_option_info (List.assoc opname options) with
             Not_found ->
                eprintf "<unbound>@.");
         idT)

(************************************************************************
 * Get the complete set of options for the current proof.
 * We *assume* that the tactics here are the ones that manipulate options.
 *)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
