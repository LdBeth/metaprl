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
   type esequent

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
   val is_sequent_term : term -> bool
   val mk_sequent_term : esequent -> term
   val explode_sequent : term -> esequent
   val args_of_sequent : term -> term

   val nth_hyp : term -> int -> string * term
   val nth_concl : term -> int -> term
   val num_hyps : term -> int
   val declared_vars : term -> string list
   val get_decl_number : term -> string -> int
   val is_free_seq_var : int -> string -> term -> bool

   (*
    * The nth_*_addr functions are used to
    * compute addreses for parts of a sequent.
    * The indexing starts from 1.  Clause 0
    * refers to the conclusion.
    *
    * The range functions provide addresses
    * that are used for contexts in sequent
    * rewriting.
    *)
   val nth_hyp_addr : term -> int -> address
   val nth_concl_addr : term -> int -> address
   val nth_clause_addr : term -> int -> address
   val hyp_range_addr : term -> int -> address
   val concl_range_addr : term -> int -> address
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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
