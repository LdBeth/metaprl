(*
 * The D tactic performs a case selection on the conclusion opname.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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

open Refiner.Refiner.Refine

open Tactic_type
open Tactic_type.Tactic

open Mp_resource
open Auto_tactic

type intro_option =
   SelectOption of int
 | IntroArgsOption of (tactic_arg -> term -> term list) * term option
 | AutoMustComplete
 | CondMustComplete of (tactic_arg -> bool)

type elim_option =
   ThinOption of (int -> tactic)  (* Thin the eliminated hyp, unless overridden *)
 | ElimArgsOption of (tactic_arg -> term -> term list) * term option

type intro_item = string * int option * bool * tactic

resource (term * (int -> tactic), int -> tactic) elim
resource (term * intro_item, tactic) intro

val process_elim_resource_annotation :
   (Tactic.pre_tactic * elim_option list, term * (int -> tactic)) annotation_processor
val process_intro_resource_annotation :
   (Tactic.pre_tactic * intro_option list, term * intro_item) annotation_processor

val wrap_intro : tactic -> intro_item

(*
 * The inherited d tactic.
 *)
val d_prec : auto_prec

topval dT : int -> tactic

val in_auto : tactic_arg -> bool (* Returns true what we are in weakAutoT, but not strongAutoT *)

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
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
