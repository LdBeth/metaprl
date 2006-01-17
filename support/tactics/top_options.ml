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

open Lm_printf

open Opname
open Tactic_boot_sig
open Refiner.Refiner.Refine
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.RefineError
open Mp_resource
open Top_resource

open Tactic_type
open Tactic_type.Tacticals

type select_entry = term * option_info

(*
 * Extract the option.
 *)
let add_data options (t, info) =
   OpnameTable.add options (opname_of_term t) info

let resource_info =
   Functional (**)
      { fp_is_local = true;
        fp_empty    = OpnameTable.empty;
        fp_add      = add_data;
        fp_retr     = (fun x -> x)
      }

let resource (select_entry, option_table) select =
   resource_info

(************************************************************************
 * Get the options.  For efficiency, we assume that if the tactic_arg
 * options are non-empty, the resource does not need to be consulted.
 *)
declare options

let options_opname = opname_of_term << options >>

let get_options p =
   let info = Sequent.get_option_args p in
      if OpnameTable.is_empty info then
         Sequent.get_resource_arg p get_select_resource
      else
         info

(*
 * Force the table to be nonempty.
 *)
let add_options_tag info =
   OpnameTable.add info options_opname OptionAllow

(************************************************************************
 * Check whether the options on the rule/rewrite are allowed.
 *)
let options_are_allowed options keys =
   OpnameSet.for_all (fun key ->
         not (OpnameTable.mem options key) || OpnameTable.find options key = OptionAllow) keys

let options_are_allowed_arg p keys =
   options_are_allowed (get_options p) keys

(************************************************************************
 * Utilities.
 *)
let opset_of_terms tl =
   List.fold_left (fun options t -> OpnameSet.add options (opname_of_term t)) OpnameSet.empty tl

let opset_of_opt_terms = function
   None ->
      None
 | Some tl ->
      Some (opset_of_terms tl)

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
      let options = add_options_tag (get_options p) in
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
      let options = add_options_tag (get_options p) in
      let options = OpnameTable.remove options opname in
         Tacticals.addOptionsT options)

let withOptionInfoT t info tac =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = add_options_tag (get_options p) in
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
      let options = add_options_tag (get_options p) in
      let options = OpnameTable.remove options opname in
         Tacticals.withOptionsT options tac)

let printOptionT t =
   funT (fun p -> (**)
      let opname = opname_of_term t in
      let options = Tacticals.get_option_args p in
         eprintf "Option: %s =" (string_of_opname opname);
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
