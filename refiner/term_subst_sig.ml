(*
 * Substitution, alpha equality, unification.
 *)

module type TermSubstSig =
sig
   type term
   type param
   
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
   val free_vars : term -> string list
   val free_vars_terms : term list -> string list
   val context_vars : term -> string list
   val binding_vars : term -> string list

   val unify : term_subst -> term -> term -> term_subst

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
 * $Log$
 * Revision 1.1  1998/05/27 15:14:57  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
