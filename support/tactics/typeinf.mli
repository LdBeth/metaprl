(*
 * Before anything, we start the type inference resource.
 * This is mostly an incomplete type inference algorithm, but
 * it is used to perform basic inference.
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
 * Modified by: Alexey Nogin <nogin@cs.cornell.edu>
 *)

open String_set
open Refiner.Refiner.Term

open Unify_mm

open Tactic_boot_sig

open Tactic_type
open Tactic_type.Sequent
open Tactic_type.Tacticals

(*
 * The types of the main type inference functions,
 * typeinf_subst_fun and typeinf_func
 * are described in filter/boot/tactic_boot_sig.mlz
 *)

(*
 * This resource is used to analyze the sequent to gather type info.
 *)
type typeinf_subst_info = term * typeinf_subst_fun

resource (typeinf_subst_info, typeinf_subst_fun) typeinf_subst

(*
 * Modular components also get a recursive instance of
 * the inference algorithm.
 *)
type typeinf_comp = typeinf_func -> typeinf_func

(*
 * This is the resource addition.
 *)
type typeinf_resource_info = term * typeinf_comp

(*
 * The resource itself.
 *)
resource (typeinf_resource_info, typeinf_func) typeinf

(*
 * Utilities.
 *)
val infer_type : tactic_arg -> term -> term
val infer_type_args : tactic_arg -> term -> term list
val infer_type_2args : tactic_arg -> term -> term list (* need two args; first can be inferred *)

(* finalizes the type using all the eqautions collected,
 * returns the new eqs and opt_eqs *)
val typeinf_final : StringSet.t -> eqnlist -> opt_eqs_type -> term_subst -> term -> eqnlist * opt_eqs_type * term_subst * term

(* creates a "fresh" variable name *)
val vnewname : StringSet.t -> term_subst -> string -> string

val infer_const : term -> typeinf_comp
val infer_map : (term -> term) -> typeinf_comp

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
