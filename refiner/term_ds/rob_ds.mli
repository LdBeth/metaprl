(* The unification of terms with bindings based on the ideas of
 * Robinson's  unification algorithm and lasy convertion into dag representation*)


open Refine_error_sig
open Term_ds_sig

open Term_ds
open String_set

module TermSubstRob
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

   type node= nodeknown ref
   and nodeknown={mutable nodeterm:term;
                  mutable children:childrentype;
                  mutable bindings:caseofb}
   and childrentype=ChildrenDelayed
                  | ChildrenKnown of node list
   and caseofb =  BindingPoint of bnode list list
                | BoundOc of (node*int*int)
                | BindingNone
                | BindingDelayed of brunch_bindings_delayed
   and bnode = dummy ref
   and dummy = Dummy
   and sacktype = (string, node) Hashtbl.t
(*
*  bbd:brunch_bindings_delayed will store the delayed bindings in
*  the current brunch of the tree -- in order to fill in the field
*  caseofb = (BoundOc ???) on the leafs later
*)
   and brunch_bindings_delayed = (string*(node*int*int)) list
(*
*
* ***********************************      ***********************************
* *  nodeterm<--> op1               *      *  nodeterm<--> op2               *
* *                *                *      *                *                *
* *       *****************         *      *       *****************         *
* *       *               *         *      *       *               *         *
* * ***********     *************** *      * ***********     *************** *
* * *children *     *  bindings = * *      * *children *     *  bindings = * *
* * *   .     *     * BindingPoint* *      * *   .     *     * BindingPoint* *
* * *         *     *     ...     * *      * *   .     *     *     ...     * *
* * *   .     *     *             * *      * *   .     *     *             * *
* * *         *     *   (.):bnode * *      * *         *     *   (.):bnode * *
* * *bindings:*     *************** *      * *bindings:*     *************** *
* * *BoundOc  *                     *      * *BoundOc  *                     *
* * * ->op1,..*                     *      * * ->op2,..*                     *
* * ***********                     *      * ***********                     *
* ***********************************      ***********************************
*
* On top:
*
*   *                                                                      *
*   *                     ********************************************     *
* ***********************************      **********************************┐
* *  nodeterm<--> op1     *         *      *  nodeterm<--> op2       *       *
* *                *      *         *      *                *        *       *
* *       *****************         *      *       ****************  *       *
* * ***********                     *      * ***********     *************** *
* * *children *                     *      * *children *     *  bindings = * *
* * *   .     *     *************** *      * *   .     *     * BindingPoint* *
* * *   .     *     *  bindings = * *      * *   .     *     *     ...     * *
* * *   .     *     * BindingPoint* *      * *   .     *     *             * *
* * *         *     *     ...     * *      * *         *     *   (.):bnode * *
* * *bindings:*     *             * *      * *bindings:*     *************** *
* * *BoundOc  *     *   (.):bnode * *      * *BoundOc  *                     *
* * *   ->op1 *     *************** *      * *   ->op2 *                     *
* * ***********      п опадае       *      * ***********                     *
* ***********************************      ***********************************
*
* After the unification of all subterms:
*   *
*   ************************************************************************
*                                                                          *
*                         ********************************************     *
* ***********************************      ***********************************
* *  nodeterm<--> op1     *         *      *  nodeterm<--> op2       *       *
* *                *      *         *      *                *        *       *
* *       *****************         *      *       ***************** *       *
* * ***********                     *      * ***********     *************** *
* * *children *                     *      * *children *     *  bindings = * *
* * *   .     *     *************** *      * *   .     *     * BindingPoint* *
* * *   .     *     *  bindings = * *      * *   .     *     *     ...     * *
* * *   .     *     * BindingPoint* *      * *   .     *     *             * *
* * *         *     *     ...     * *      * *         *     *   (.):bnode * *
* * *bindings:*     *             * *      * *bindings:*     *************** *
* * *BoundOc  *     *   (.):bnode * *      * *BoundOc  *                     *
* * * ->op1,..*     *************** *      * * ->op2,..*                     *
* * ***********      п              *      * ***********                     *
* ***********************************      ***********************************
*        ^ is unreachable now
*
*)

(*  A variant with  brunch_bindings_delayed list stored in BindingDelayed
*   val calc_node : node -> unit
*   We suppose the invariant:
*      bindings!=BindingDelayed _ => children!=ChildrenDelayed
*)
   val calc_node : node -> unit
   val nodeoper : node -> sacktype -> Term.operator
end




