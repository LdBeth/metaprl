(*
 * Substitution, alpha equality, unification.
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
