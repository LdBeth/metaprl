(*
 * Before anything, we start the type inference resource.
 * This is mostly an incomplete type inference algorithm, but
 * it is used to perform basic inference.
 *
 * The structure of this code is similar to the Simp_typeinf
 * module, but this implementation includes support for a broader
 * set of heuristics.
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

open Lm_symbol
open Refiner.Refiner.Term

open Unify_mm

open Tactic_type.Tacticals

(*
 * A function that analyzes the sequent to gather type info.
 * It gets a clause from the current sequent or its assumptions.
 *)
type typeinf_subst_fun = term_subst -> (var option * term) -> term_subst

(*
 * A type inference is performed in a type context,
 * which maps variables to type.
 *
 * An inference function takes as arguments :
 * 1) consts - a set of variables that should be treated as
 * constants when we use unification to figure things out.
 * 2) decls - an associative list of variable names and
 * the types these variables were declared with.
 * 3) eqs - a list of equations we have on our type variables
 * 4) opt_eqs - a list of equations we can use to figure the variables out,
 * but these equations do not have to be satisfied.
 * 5) defs -  list of the defaults that we should use instead of variables
 * in case there is no other information we can use to figure them out
 * 6) t - a term whoose type we want to infer
 *
 * An inference function returns:
 * Updated eqs, updated opt_eqs, updated defs and a type
 * (that can contain variables)
 *)
type opt_eqs_type = (term * term) list
type typeinf_func = SymbolSet.t -> term_subst -> eqnlist -> opt_eqs_type -> term_subst -> term -> eqnlist * opt_eqs_type * term_subst * term

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
val typeinf_final : SymbolSet.t -> eqnlist -> opt_eqs_type -> term_subst -> term -> eqnlist * opt_eqs_type * term_subst * term

(* creates a "fresh" variable name *)
val vnewname : SymbolSet.t -> term_subst -> var -> var

val infer_const : term -> typeinf_comp
val infer_map : (term -> term) -> typeinf_comp

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
