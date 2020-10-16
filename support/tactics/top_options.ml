(*
 * Define a resource for select arguments.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
extends Mptop
extends Proof_initialize

open Lm_printf

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Options_boot
open Mp_resource

open Tactic_type
open Tactic_type.Tacticals

(*
 * Option printing.
 *)
let pp_print_option_info buf info =
   pp_print_string buf (string_of_option info)

(*
 * The options are collected in a list.
 * For efficiency, squash the entries in the list when
 * the resource is extracted.
 *)
let add_data options (t, info) =
   add_option options t info

let extract_data options = options

let resource_info =
   Functional (**)
      { fp_empty    = options_empty;
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

(************************************************************************
 * Utilities.
 *)
let get_options = Sequent.get_option_args

let rule_labels_are_allowed_arg p labels =
   rule_labels_are_allowed (get_options p) labels

(************************************************************************
 * Tacticals for option handling.
 *)
let addOptionInfoT = Tacticals.addOptionT
let addOptionT t s = addOptionInfoT t (option_of_string s)
let allowOptionT t = addOptionInfoT t OptionAllow
let excludeOptionT t = addOptionInfoT t OptionExclude
let withOptionInfoT = Tacticals.withOptionT
let withOptionT t s = withOptionInfoT t (option_of_string s)
let withAllowOptionT t = withOptionInfoT t OptionAllow
let withExcludeOptionT t = withOptionInfoT t OptionExclude
let removeOptionT = Tacticals.removeOptionT
let withoutOptionT = Tacticals.withoutOptionT

let printOptionT t =
   funT (fun p -> (**)
      let options = list_options (get_options p) in
         eprintf "Option: %a = " print_term t;
         (try eprintf "%a@." pp_print_option_info (List.assoc t options) with
             Not_found ->
                eprintf "<unbound>@.");
         idT)

let printOptionsT =
   funT (fun p -> (**)
      let options = list_options (get_options p) in
         eprintf "Total options: %d@." (List.length options);
         List.iter (fun (t, info) ->
               eprintf "Option %a -> %a@." print_term t pp_print_option_info info) options;
         idT)

let withOptionInfoC = Conversionals.withOptionC
let withOptionC t s = withOptionInfoC t (option_of_string s)
let withoutOptionC = Conversionals.withoutOptionC

let select_crw = Perv.select_crw

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
