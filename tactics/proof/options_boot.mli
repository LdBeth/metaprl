(*
 * Option types.
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
open Opname
open Refiner.Refiner.TermType

(*
 * Flags associated with options.
 *)
type option_info =
   OptionAllow
 | OptionExclude
 | OptionIgnore

val string_of_option : option_info -> string
val option_of_string : string -> option_info
val is_option_string : string -> bool

(*
 * Options decision table
 *)
type option_table

val options_empty : option_table
val add_option : option_table -> opname -> option_info -> option_table
val list_options : option_table -> (opname * option_info) list
val options_eq : option_table -> option_table -> bool

(*
 * Labels for a rule.
 *)
type rule_labels

(*
 * Check whether an options set is allowed.
 *)
val rule_labels_are_allowed : option_table -> rule_labels -> bool

(*
 * Utilities.
 *)
val rule_labels_empty        : rule_labels
val rule_labels_of_terms     : term list -> rule_labels
val rule_labels_of_opt_terms : term list option -> rule_labels

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
