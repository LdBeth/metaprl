(* This is an EXPERIMENTAL module that is not currently being used *)

(* The unification of terms with bindings based on the ideas of
 * Robinson's  unification algorithm and lasy convertion into dag representation*)


open Refine_error_sig
open Term_ds_sig

open Term_ds
open Lm_string_set

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

(*
*   Nodes with children relation (field children) give a tree.
*   Sack contains links which make a DAG from it. Sack also contains
*   looping links for variable nodes which means that this variable
*   is not eliminated yet; these loops shouldn't be followed (in DAG).
*)
   type node= nodeknown ref
   and nodeknown={mutable nodeterm:term;
                  mutable children:childrentype;
                  mutable bindings:caseofb;
                  mutable bound:bool}
   and childrentype=ChildrenDelayed
                  | ChildrenKnown of node list
   and caseofb =  BindingPoint of bnode list list
                | BoundOc of (node*int*int)
                | Constant
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
*    *                                                                   *
*    *                                                                   *
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
* ***********************************      ***********************************
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

(*  calc_node is one step of conversion from term to DAG.
*   Almost free when the node is already calculated!
*   We suppose the invariants:
*      1) bindings!=BindingDelayed _ => children!=ChildrenDelayed;
*      2) sack does not contain links to a node with
*         bindings = BindingDelayed _ .
*)

   val term2node : term -> node
   val initsack : sacktype

   val calc_node : node -> sacktype -> StringSet.t -> unit
   val follow_links : node -> sacktype -> StringSet.t -> node

   val is_bvar_n : node -> bool
   val is_const_n : node -> bool

   val is_fvar_n : node -> bool
   val links_fvar_n : node -> sacktype -> StringSet.t -> bool
   val fvarstr_n :  node -> sacktype -> StringSet.t -> string

   val is_fsymb_n : node -> bool
   val links_fsymb_n : node -> sacktype -> StringSet.t -> bool
   val fsymboper_n : node -> sacktype -> StringSet.t -> TermType.operator
   val succs : node -> sacktype -> StringSet.t -> node list
   val substfree : string -> node -> sacktype -> StringSet.t -> bool

(* unify_n checks the unifiability and returnes the mgu as a side effect
*  by updating the sack: given initially a sack representing an idempotent
*  substitution \sigma it updates the sack into one representing (mgu)*\sigma,
*  also idempotent.
*  When nodes are not unifiable the sack will be broken !!!
*)
   val unify_n : node -> node -> sacktype -> StringSet.t ->bool
   val unifiable_rob : term -> term -> sacktype -> StringSet.t ->bool
   val  unifytosack :  term -> term -> sacktype -> StringSet.t ->sacktype


end












