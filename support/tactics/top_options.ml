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

type option_command =
   OptionAccept
 | OptionReject
 | OptionClear

type select_entry = term * option_command

type rule_labels_info =
   { rule_when    : OpnameSet.t;
     rule_unless  : OpnameSet.t
   }

type rule_labels = rule_labels_info option

(*
 * Extract the option.
 *)
let add_data options (t, info) =
   match info with
      OptionAccept ->
         OpnameTable.add options (opname_of_term t) OptionAllow
    | OptionReject ->
         OpnameTable.add options (opname_of_term t) OptionExclude
    | OptionClear ->
         OpnameTable.empty

let resource_info =
   Functional (**)
      { fp_empty    = OpnameTable.empty;
        fp_add      = add_data;
        fp_retr     = (fun x -> x)
      }

let resource (select_entry, option_table) select =
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
let rule_labels_are_allowed options labels =
   match labels with
      None ->
         true
    | Some labels ->
         let { rule_when = labels_when;
               rule_unless = labels_unless
             } = labels
         in
            OpnameSet.for_all (fun key ->
                  OpnameTable.mem options key && OpnameTable.find options key = OptionAllow) labels_when
            && OpnameSet.for_all (fun key ->
                  not (OpnameTable.mem options key) || OpnameTable.find options key = OptionAllow) labels_unless


let rule_labels_are_allowed_arg p labels =
   rule_labels_are_allowed (get_options p) labels

(************************************************************************
 * Utilities.
 *)
let rule_labels_empty = None

let rule_labels_of_terms tl_select tl_label =
   match tl_select, tl_label with
      [], [] ->
         None
    | _ ->
         let info =
            { rule_when   = List.fold_left (fun options t -> OpnameSet.add options (opname_of_term t)) OpnameSet.empty tl_select;
              rule_unless = List.fold_left (fun options t -> OpnameSet.add options (opname_of_term t)) OpnameSet.empty tl_label
            }
         in
            Some info

let rule_labels_of_opt_terms tl_select tl_label =
   let tl_select =
      match tl_select with
         Some labels -> labels
       | None -> []
   in
   let tl_label =
      match tl_label with
         Some labels -> labels
       | None -> []
   in
      rule_labels_of_terms tl_select tl_label

let rule_labels_not_allowed loc tl_select tl_label =
   match tl_select, tl_label with
      None, None ->
         ()
    | Some _, _
    | _, Some _ ->
         Stdpp.raise_with_loc loc (RefineError ("option check", StringError "rule labels are not allowed, or the annotation processor has not been updated"))

(************************************************************************
 * Tacticals for option handling.
 *)
let pp_print_option_info buf info =
   let s =
      match info with
         OptionAllow -> "allow"
       | OptionExclude -> "exclude"
   in
      pp_print_string buf s

let option_info_of_string = function
   "allow" -> OptionAllow
 | "exclude" -> OptionExclude
 | s -> raise (RefineError ("option_info_of_string", StringError (Printf.sprintf "illegal option '%s': valid options are 'allow' and 'exclude'" (String.escaped s))))

let addOptionInfoT t info =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = get_options p in
      let options = OpnameTable.add options opname info in
         Tacticals.addOptionsT options)

let addOptionT t s =
   addOptionInfoT t (option_info_of_string s)

let allowOptionT t =
   addOptionInfoT t OptionAllow

let excludeOptionT t =
   addOptionInfoT t OptionExclude

let removeOptionT t =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = get_options p in
      let options = OpnameTable.remove options opname in
         Tacticals.addOptionsT options)

let withOptionInfoT t info tac =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = get_options p in
      let options = OpnameTable.add options opname info in
         Tacticals.withOptionsT options tac)

let withOptionT t s =
   withOptionInfoT t (option_info_of_string s)

let withAllowOptionT t =
   withOptionInfoT t OptionAllow

let withExcludeOptionT t =
   withOptionInfoT t OptionExclude

let withoutOptionT t tac =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = get_options p in
      let options = OpnameTable.remove options opname in
         Tacticals.withOptionsT options tac)

let printOptionT t =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = get_options p in
         eprintf "Option: %s = " (string_of_opname opname);
         (try eprintf "%a@." pp_print_option_info (OpnameTable.find options opname) with
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
