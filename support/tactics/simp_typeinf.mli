(*
 * Before anything, we start the type inference resource.
 * This is supposed to be a strict type inference algorithm,
 * for languages where type inference is well-defined,
 * for example in ML-like languages.
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
 * Copyright (C) 1998,2003 Mojave Group, Caltech, Cornell University
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
open Refiner.Refiner.TermType

open Unify_mm

open Tactic_type.Tactic

(*
 * Some generic renaming of type variables.
 *)
type ty_var = var
type ty_var_set = SymbolSet.t
type tenv = ty_var_set
type venv = term SymbolTable.t

(*
 * A type inference is performed in a type context,
 * which maps variables to type.
 *
 * An inference function takes as arguments :
 * 1) consts - a set of variables that should be treated as
 * constants when we use unification to figure things out.
 * 2) tenv - set of all bound type variables
 * 3) venv - a table of variable names and
 * the types these variables were declared with.
 * 4) eqs - a list of equations we have on our type variables
 * 5) t - a term whoose type we want to infer
 *
 * An inference function returns:
 * 1) A new term constructed by the type inference function;
 * this term can be arbitrary, but it is often a new term
 * that represents the input term with type annotations added.
 * 2) Updated eqs,
 * 3) a type for the term (that can contain new type variables)
 *)
type simp_typeinf_func = ty_var_set -> tenv -> venv -> eqnlist -> term -> term * eqnlist * term

(*
 * Modular components also get a recursive instance of
 * the inference algorithm.
 *)
type simp_typeinf_comp = simp_typeinf_func -> simp_typeinf_func

(*
 * This is the resource addition.
 *)
type simp_typeinf_resource_info = term * simp_typeinf_comp

(*
 * The resource itself.
 *)
resource (simp_typeinf_resource_info, simp_typeinf_func) simp_typeinf

(*
 * Utilities.
 * infer_type returns two values:
 *    1. a new term, constructed during type inference
 *    2. the type of the argument
 *)
val simp_infer_type : tactic_arg -> term -> term * term
val simp_infer_type_args : tactic_arg -> term -> term list

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
