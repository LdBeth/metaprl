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

open Opname
open Tactic_boot_sig
open Refiner.Refiner.Refine
open Mp_resource
open Top_resource

open Tactic_type.Tactic

type select_entry = term * option_info

resource (select_entry, option_table) select

(*
 * Labels for a rule.
 *)
type rule_labels

(*
 * Check whether an options set is allowed.
 *)
val get_options : tactic_arg -> option_table
val rule_labels_are_allowed : option_table -> rule_labels -> bool
val rule_labels_are_allowed_arg : tactic_arg -> rule_labels -> bool

(*
 * Utilities.
 *)
val rule_labels_empty        : rule_labels
val rule_labels_of_terms     : term list -> term list -> rule_labels
val rule_labels_of_opt_terms : term list option -> term list option -> rule_labels
val rule_labels_not_allowed  : MLast.loc -> term list option -> term list option -> unit

(*
 * Tactics.
 *)
topval addOptionT : term -> string -> tactic
topval allowOptionT : term -> tactic
topval excludeOptionT : term -> tactic
topval printOptionT : term -> tactic

topval withOptionT : term -> string -> tactic -> tactic
topval withAllowOptionT : term -> tactic -> tactic
topval withExcludeOptionT : term -> tactic -> tactic

topval withoutOptionT : term -> tactic -> tactic
topval removeOptionT : term -> tactic

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
