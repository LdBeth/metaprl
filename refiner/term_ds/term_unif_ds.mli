(*
 *    MM-unification deals with the first order unification problems of the form
 *               T1_i = T2_i  , i=1,2,...
 *    for terms with bindings from Term_ds.TermType.term. The problems are members
 *    of eqnlist type. The unification is treated as a transformation of
 *    an arbitrary unification problem into an equivalent  problem in the
 *    "solved" form:
 *           x1=F1(x2,...,xm)
 *           x2=F2(x3,...,xm)
 *           ...
 *    If it is impossible then the exceptions Cycle or Clash are raised.
 *    The conversion of a unification problem into its mgu always
 *    implies  the transformation into the "solved" form (N*log N) and
 *    the calculation of the product of substitutions which may be more
 *    expensive (m^2). The internal representation gives some speed-up
 *    (5-10 times faster) but failes to reduce the order.
 *       Extract the mgu only when you need it!
 *       Use unify_eqnl_eqnl for iterative calls.
 *       The unifiable* functions are much faster!
 *       In the negative case all the functions run in the same time!
 *       Use  eqnlist2ttlist if you need the unification problem as is.
 *)

open Refine_error_sig
open Term_ds_sig

open Term_ds
open String_set

module TermSubstMm
(Term : TermDsSig
        with type level_exp_var = TermType.level_exp_var
        with type level_exp = TermType.level_exp
        with type param = TermType.param
        with type operator = TermType.operator
        with type term = TermType.term
        with type term_core = TermType.term_core
        with type bound_term = TermType.bound_term
        with type esequent = TermType.esequent
        with type seq_hyps = TermType.seq_hyps
        with type seq_goals = TermType.seq_goals
        with type hypothesis = TermType.hypothesis

        with type level_exp_var' = TermType.level_exp_var'
        with type level_exp' = TermType.level_exp'
        with type object_id = TermType.object_id
        with type param' = TermType.param'
        with type operator' = TermType.operator'
        with type term' = TermType.term'
        with type bound_term' = TermType.bound_term'

        with type term_subst = TermType.term_subst)
(RefineError : RefineErrorSig
               with type level_exp = TermType.level_exp
               with type param = TermType.param
               with type term = TermType.term
               with type bound_term = TermType.bound_term) :
sig
   type term = Term_ds.TermType.term
   and term_subst = Term_ds.TermType.term_subst

   type eqnlist = (term*term) list

   val eqnlist_empty : eqnlist
   val eqnlist_append_eqn : eqnlist -> term -> term -> eqnlist
   val eqnlist_append_var_eqn : string -> term -> eqnlist -> eqnlist
   val eqnlist_append_eqns : eqnlist -> (term*term) list -> eqnlist
   val eqnlist2ttlist : eqnlist -> (term*term) list

   val new_eqns_var : eqnlist -> string -> string

   val unifiable : Term.term -> Term.term -> StringSet.t -> bool
   val unifiable_eqnl : eqnlist -> StringSet.t -> bool

   val unify : Term.term -> Term.term -> StringSet.t -> term_subst
   val unify_eqnl : eqnlist -> StringSet.t -> term_subst
   val unify_eqnl_eqnl : eqnlist -> StringSet.t -> eqnlist
end









