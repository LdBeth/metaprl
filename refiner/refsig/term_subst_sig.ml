(*
 * Substitution, alpha equality, unification.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

module type TermSubstSig =
sig
   type term
   type param

   (*
    * Unification uses its own substition type.
    *)
   type unify_subst

   (*
    * Substitution, matching, unification.
    *)
   type term_subst = (string * term) list

   (************************************************************************
    * Operations                                                           *
    ************************************************************************)

   (*
    * Term operations.
    * subst: simultaneous subst of terms for vars.
    * var_subst: subst of var for a term.
    *)
   val subst : term -> term list -> string list -> term
   val var_subst : term -> term -> string -> term
   val equal_params : param -> param -> bool
   val alpha_equal : term -> term -> bool
   val alpha_equal_vars : (term * string list) -> (term * string list) -> bool
   val alpha_equal_match : (term * string list) ->
          (term * string list * string list * term list) ->
          bool

   (*
    * Get the list of free variables.
    *)
   val is_free_var : string -> term -> bool
   val is_free_var_list : string list -> term list -> bool
   val free_vars : term -> string list
   val free_vars_terms : term list -> string list
   val context_vars : term -> string list
   val binding_vars : term -> string list

   (*
    * Matching is like unification but variables in
    * the first term match terms in the second,
    * but not vice-versa.
    *)
   val unify_empty : unify_subst
   val subst_of_unify_subst : unify_subst -> term_subst
   val unify_subst_of_subst : term_subst -> unify_subst
   val add_unify_subst : string -> term -> unify_subst -> unify_subst
   val new_unify_var : unify_subst -> string -> string
   val unify : unify_subst -> String_set.StringSet.t -> term -> term -> unify_subst
   val match_terms : term_subst -> term -> term -> term_subst

   (*
    * generalization: see if the first term generalizes the second term.
    * Return the alpha conversion if so, otherwise fail with
    * Invalid_argument "generalization"
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
