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

open Opname
open Tactic_boot_sig
open Refiner.Refiner.Refine
open Mp_resource
open Top_resource
open Options_boot

open Tactic_type.Tactic

resource (term * option_info, option_table) select

(*
 * Tactics.
 *)
topval addOptionT : term -> string -> tactic
topval allowOptionT : term -> tactic
topval excludeOptionT : term -> tactic

val withOptionInfoT : term -> option_info -> tactic -> tactic
topval withOptionT : term -> string -> tactic -> tactic
topval withAllowOptionT : term -> tactic -> tactic
topval withExcludeOptionT : term -> tactic -> tactic

topval withoutOptionT : term -> tactic -> tactic
topval removeOptionT : term -> tactic

(*
 * Printing.
 *)
topval printOptionT : term -> tactic
topval printOptionsT : tactic

(*
 * Conversions.
 *)
val withOptionInfoC : term -> option_info -> conv -> conv
topval withOptionC : term -> string -> conv -> conv
topval withoutOptionC : term -> conv -> conv

(*
 * Utilities
 *)
val get_options : tactic_arg -> option_table
val rule_labels_are_allowed_arg : tactic_arg -> rule_labels -> bool

(*
 * Specific "canonical" labels
 *)
topval select_crw : term

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
