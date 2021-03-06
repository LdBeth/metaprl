(*
 * These are the basic rewriting operations.
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
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and California
 * Institute of Technology
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Alexei Kopylov <kopylov@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
extends Mptop

open Term_addr_sig
open Refiner.Refiner.Refine
open Top_resource
open Options_boot

open Tactic_type.Tactic

(*
 * Toploop values.
 *)

topval rw : conv -> int -> tactic
topval rwc : conv -> int -> int -> tactic
topval rwAll : conv -> tactic
topval rwcAll : conv -> int -> tactic
topval rwAllAll : conv -> tactic

topval rwh : conv -> int -> tactic
topval rwch : conv -> int -> int -> tactic
topval rwhAll : conv -> tactic
topval rwchAll : conv -> int -> tactic
topval rwhAllAll : conv -> tactic

topval rwa : conv list -> int -> tactic
topval rwca : conv list -> int -> int -> tactic
topval rwaAll : conv list -> tactic
topval rwcaAll : conv list -> int -> tactic
topval rwaAllAll : conv list -> tactic

topval prefix_thenC : conv -> conv -> conv
topval prefix_orelseC : conv -> conv -> conv
topval addrC : addr_item list -> conv -> conv
topval idC : conv
topval foldC : term -> conv -> conv
topval makeFoldC : term -> conv -> conv
topval cutC : term -> conv
topval failC : conv
topval failWithC : string -> conv
topval tryC : conv -> conv
topval someSubC : conv -> conv
topval allSubC : conv -> conv
topval higherC : conv -> conv
topval lowerC : conv -> conv
topval sweepUpC : conv -> conv
topval sweepDnC : conv -> conv
val findThenC : (term -> bool) -> conv -> conv
topval firstC : conv list -> conv
topval applyAllC : conv list -> conv
topval untilFailC : conv -> conv
topval repeatC : conv -> conv
topval repeatForC : int -> conv -> conv
topval ifEqualC : term -> conv -> conv -> conv
topval replaceUsingC : term -> conv -> conv
topval progressC : conv -> conv
topval allSubThenC : conv -> conv -> conv
topval prefix_thenTC : conv -> tactic -> conv
topval forceC : string -> conv -> conv

infix thenC
infix orelseC
infix thenTC

(************************************************************************
 * REDUCTION RESOURCE                                                   *
 ************************************************************************)

type reduce_conv
type reduce_info = rule_labels * conv
type reduce_entry = term * reduce_info

resource (reduce_entry, reduce_conv) reduce

val process_reduce_resource_rw_annotation : reduce_entry rw_annotation_processor

val wrap_reduce : ?labels:term list -> conv -> reduce_info
val wrap_reduce_crw : ?labels:term list -> conv -> reduce_info

topval reduceTopC : conv
topval reduceC : conv
topval reduceT : tactic
val reduceHypsT : tactic

topval simpleReduceTopC : conv
topval simpleReduceC : conv
topval simpleReduceT : tactic

(************************************************************************
 * DEBUGGING
 ************************************************************************)

val apply_rewrite : tactic_arg -> conv -> term -> term

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
