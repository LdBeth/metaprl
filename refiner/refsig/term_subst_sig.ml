(*
 * Substitution, alpha equality, unification.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open String_set

module type TermSubstSig =
sig
   type term
   type param
   type bound_term
   type bound_term'

   (*
    * Substitution, matching, unification.
    *)
   type term_subst = (string * term) list

   (************************************************************************
    * Operations                                                           *
    ************************************************************************)

   (* Term operations. *)

   (*
    * subst: simultaneous subst of terms for vars.
    * if string list has repeated variables, the first match is used
    *
    * Bound variables will be renamed if necessary to prevent capturing
    * When renaming in bound terms with duplicate variables:
    *   x,x.t[x] would become new(x),new(x).t[new(x)]
    *
    * If a same variable is mentioned several times in string list,
    * the first occurence is used.
    *)
   val subst : term -> string list -> term list -> term
   val subst1 : term -> string -> term -> term
   val apply_subst : term -> term_subst -> term

   (*
    * dest_bterm_and_rename is the same as dest_bterm, except it will also
    * do alpha-renaming to avoid reusing the specified variables
    *
    * TODO: need to also make sure that all bvars in rerurned bound_term'
    * are distinct
    *)
   val dest_bterm_and_rename : bound_term -> String_set.StringSet.t -> bound_term'

   (*
    * var_subst: subst of var for a term.
    *)
   val var_subst : term -> term -> string -> term
   val equal_params : param -> param -> bool

   (*
    * In all alpha_equal* functions:
    * for bound terms with duplicate variables: x,x.t =alpha new_var,x.t
    *)
   val alpha_equal : term -> term -> bool

   (*
    * alpha_equal_vars: alpha equality on destructed bound terms
    * If one of the "string list"s has duplicate entries, the first entry is used.
    *)
   val alpha_equal_vars : term -> string list -> term -> string list -> bool

   (*
    * alpha_equal_fun f t1 vs t2 os =
    * t2 is alpha equal to t1 where for each free occurence of v_i
    * some term t is substituted, such that (f t o_i) is true
    *)
   val alpha_equal_fun :
      ( term -> 'a -> bool ) ->
      term -> string list ->
      term -> 'a list ->
      bool

   (*
    * Get the list of free variables.
    *)
   val is_var_free : string -> term -> bool
   val is_some_var_free : string list -> term -> bool
   val is_some_var_free_list : string list -> term list -> bool
   val free_vars_list : term -> string list
   val free_vars_set : term -> StringSet.t
   val free_vars_terms : term list -> string list
   val context_vars : term -> string list
   val binding_vars : term -> string list

   (*
    * Matching is like unification but variables in
    * the first term match terms in the second,
    * but not vice-versa.
    *)
   val match_terms : term_subst -> term -> term -> term_subst

(***********************)
   (*
    * generalization: see if the first term generalizes the second term.
    * Return the alpha conversion if so, otherwise raise
    * Failure "generalization"
    *
    * generalizes: boolean equivalent of the proceeding
    *)
   val generalizes : term -> term -> bool
   val generalization : (string * string) list -> term -> term -> (string * string) list
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
