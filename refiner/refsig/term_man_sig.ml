(*
 * Operations on "manifest" terms.  These are
 * terms that have a pervasive definition.
 *
 * Sequents are manifest only for efficiency.
 * The refiner does not use them, but they are
 * included because many logics use sequents.
 *)

module type TermManSig =
sig
   type term
   type operator
   type level_exp
   type address
   
   type hypothesis =
      Hypothesis of string * term
    | Context of string * term list
   
   type esequent =
      { sequent_hyps : hypothesis list;
        sequent_goals : term list
      }

   (************************************************************************
    * Simplified operations on manifest terms                              *
    ************************************************************************)

   (* Level expression operations *)
   val mk_const_level_exp : int -> level_exp
   val mk_var_level_exp : string -> level_exp
   val max_level_exp : level_exp -> level_exp -> level_exp
   val incr_level_exp : level_exp -> level_exp

   val level_cumulativity : level_exp -> level_exp -> bool

   (*
    * Sequents.
    * This should be visible only to sequents, but oh well.
    *)
   val is_hyp_term : term -> bool
   val dest_hyp : term -> string * term * term
   val mk_hyp_term : string -> term -> term -> term

   val is_concl_term : term -> bool
   val dest_concl : term -> term * term
   val mk_concl_term : term -> term -> term
   val null_concl : term

   val is_sequent_term : term -> bool
   val dest_sequent : term -> term list
   val explode_sequent : term -> esequent
   val mk_sequent_term : term list -> term

   val nth_hyp : term -> int -> string * term
   val nth_concl : term -> int -> term
   val num_hyps : term -> int
   val declared_vars : term -> string list
   val get_decl_number : term -> string -> int
   val is_free_seq_var : int -> string -> term -> bool

   val nth_hyp_addr : term -> int -> address
   val nth_concl_addr : term -> int -> address
   val nth_clause_addr : term -> int -> address
   val nth_clause_addrs : term -> int array -> address array
   val replace_concl : term -> term -> term
   val replace_goal : term -> term -> term          (* One concl *)

   val is_xrewrite_term : term -> bool
   val mk_xrewrite_term : term -> term -> term
   val dest_xrewrite : term -> term * term

   (*
    * Primitive lists.
    *)
   val is_xnil_term : term -> bool
   val xnil_term : term

   val is_xcons_term : term -> bool
   val mk_xcons_term : term -> term -> term
   val dest_xcons : term -> term * term

   val is_xlist_term : term -> bool
   val dest_xlist : term -> term list
   val mk_xlist_term : term list -> term

   (*
    * Primitive strings.
    *)
   val is_xstring_term : term -> bool
   val mk_xstring_term : string -> term
   val dest_xstring : term -> string

   (*
    * Primitive abstractions.
    *)
   val mk_xlambda_term : string -> term -> term

   (*
    * Construct a redex out of some vars, params, and other terms.
    *)
   val construct_redex : string array -> term list -> term list -> term
end

(*
 * $Log$
 * Revision 1.5  1998/06/09 20:52:26  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.4  1998/06/03 22:19:28  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.3  1998/06/03 15:23:25  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.2  1998/06/01 13:55:13  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:01:45  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:29  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
