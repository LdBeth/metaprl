(*
 * Forward-chaining tactic.
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
open Refiner.Refiner.Refine

open Tactic_type
open Tactic_type.Tactic

open Mp_resource

(*
 * Precedences.
 *)
type forward_prec

val forward_trivial_prec : forward_prec
val forward_normal_prec  : forward_prec
val forward_max_prec     : forward_prec

(*
 * Operations on precedences.
 * The create operation takes a list of precedences that
 * are smaller, and another list that are larger.
 *)
val create_forward_prec : forward_prec list -> forward_prec list -> forward_prec

(*
 * Arguments to forward-chaining.
 *)
type forward_option =
   ForwardArgsOption of (tactic_arg -> term -> term list) * term option
 | ForwardPrec of forward_prec

(*
 * Rules for forward chaning.
 * WARNING: the rules you add should not delete, permute, or otherwise
 * modify the existing hyp list.  The new hyps must be placed at the end
 * of the hyp list.
 *)
type forward_info =
   { forward_prec : forward_prec;
     forward_tac  : int -> tactic
   }

resource (term * forward_info, (forward_info -> bool) -> int -> tactic) forward

val process_forward_resource_annotation :
   (Tactic.pre_tactic * forward_option list, term * forward_info) annotation_processor

topval forwardT : int -> tactic
topval forwardChainBoundT : int -> tactic
topval forwardChainT : tactic

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
