(*
 * The D tactic performs a case selection on the conclusion opname.
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
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and
 * California Institute of Technology
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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
extends Auto_tactic

open Opname
open Refiner.Refiner.Refine

open Tactic_type
open Tactic_type.Tactic

open Mp_resource
open Top_resource
open Options_boot
open Auto_tactic

type intro_option =
   SelectOption of int
 | IntroArgsOption of (tactic_arg -> term -> term list) * term option
 | AutoMustComplete
 | CondMustComplete of (tactic_arg -> bool)

type elim_option =
   ThinOption of (int -> tactic)  (* Thin the eliminated hyp, unless overridden *)
 | ThinFirst of (int -> tactic)   (* Try thinning before eliminating; thinning will be added to AutoNormal *)
 | ElimArgsOption of (tactic_arg -> term -> term list) * term option
 | AutoOK (* It's OK to use this in autoT on "normal" level *)

type intro_item
type elim_item  = rule_labels * bool * (int -> tactic)

resource (term * elim_item, int -> tactic) elim
resource (term * intro_item, tactic) intro

val process_elim_resource_annotation :
   ?options: elim_option list ->
   (term * elim_item) annotation_processor

val process_intro_resource_annotation :
   ?options: intro_option list ->
   (term * intro_item) annotation_processor

val wrap_intro :
   ?labels: term list ->   (* rule selection labels *)
   ?name:string ->         (* rule/tactic name *)
   ?select:int ->          (* require a matching selT argument *)
   ?auto:auto_type ->      (* use a rule in a specific phase of autoT; default: AutoNormal *)
   ?fall_through:bool ->   (* whether to keep trying other matches if this one fails; default: true *)
   tactic -> intro_item

val wrap_elim : ?labels: term list -> (int -> tactic) -> elim_item
val wrap_elim_auto_ok : ?labels: term list -> (int -> tactic) -> elim_item
val intro_must_select : intro_item

(*
 * The inherited d tactic.
 *)
val d_prec : auto_prec
val d_elim_prec : auto_prec
val d_in_auto : tactic_arg -> bool (* true when in auto, but not the "Complete" part of it *)
val d_outside_auto : tactic -> tactic (* temporary drop any "I am in autoT" information *)

topval dT : int -> tactic

(*
 * Run dT 0 so many times.
 *)
topval dForT : int -> tactic

val intro_typeinf : term -> intro_option
val simp_intro_typeinf : term -> intro_option
val elim_typeinf : term -> elim_option
val intro_typeinf_plusone : term -> intro_option
val elim_typeinf_plusone : term -> elim_option
val elim_univ_arg : elim_option
val intro_univ_arg : intro_option

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
