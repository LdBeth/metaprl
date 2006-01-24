(*
 * Substitution, alpha equality, simple matching.
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
 * Copyright (C) 1998-2005 MetaPRL Group
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_symbol
open Term_sig

module type TermSubstSig =
sig
   module SubstTypes : Term_sig.TermSig
   open SubstTypes

   type term_subst = (var * term) list

   (************************************************************************
    * Operations                                                           *
    ************************************************************************)

   (* Term operations. *)

   (*
    * subst: simultaneous subst of terms for vars.
    * if var list has repeated variables, the first match is used
    *
    * Bound variables will be renamed if necessary to prevent capturing
    * When renaming in bound terms with duplicate variables:
    *   x,x.t[x] would become new(x),new(x).t[new(x)]
    *
    * If a same variable is mentioned several times in var list,
    * the first occurence is used.
    *)
   val subst : term -> var list -> term list -> term
   val subst1 : term -> var -> term -> term
   val apply_subst : term_subst -> term -> term

   (*
    * dest_bterm_and_rename is the same as dest_bterm, except it will also
    * do alpha-renaming to avoid reusing the specified variables
    *)
   val dest_bterm_and_rename : SymbolSet.t -> bound_term -> bound_term'

   (*
    * var_subst: subst of var for a term.
    *)
   val var_subst : term -> term -> var -> term
   val equal_params : param -> param -> bool
   val opparam_eq : param op_param -> param op_param -> bool
   val equal_operators : operator -> operator -> bool

   (*
    * In all alpha_equal* functions:
    * for bound terms with duplicate variables: x,x.t =alpha new_var,x.t
    *)
   val alpha_equal : term -> term -> bool

   (*
    * alpha_equal_vars: alpha equality on destructed bound terms
    * If one of the "var list"s has duplicate entries, the first entry is used.
    *)
   val alpha_equal_vars : term -> var list -> term -> var list -> bool

   (*
    * alpha_equal_fun f t1 vs t2 os =
    * t2 is alpha equal to t1 where for each free occurence of v_i
    * some term t is substituted, such that (f t o_i) is true
    *)
   val alpha_equal_fun :
      ( term -> 'a -> bool ) ->
      term -> var list ->
      term -> 'a list ->
      bool

   (*
    * standardize t =
    * generates a term alpha-equal to t, but all binding vars
    * have been renamed.
    *)
   val standardize : term -> term

   (*
    * Get the list of free variables.
    *)
   val is_closed_term : term -> bool
   val is_var_free : var -> term -> bool
   val is_some_var_free : var list -> term -> bool
   val is_some_var_free_list : var list -> term list -> bool
   val free_vars_list : term -> var list
   val free_vars_set : term -> SymbolSet.t
   val free_vars_terms : term list -> SymbolSet.t

   (*
    * Matching is like unification but variables in
    * the first term match terms in the second,
    * but not vice-versa.
    *)
   val match_terms : term_subst -> term -> term -> term_subst
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
