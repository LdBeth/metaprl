(*
 *
 *)

module type TermManSig =
sig
   type term
   type operator
   type level_exp

   (************************************************************************
    * Simplified operations on manifest terms                              *
    ************************************************************************)
   
   (* Level expression operations *)
   val mk_const_level_exp : int -> level_exp
   val mk_var_level_exp : string -> level_exp
   val incr_level_exp : level_exp -> level_exp
   val max_level_exp : level_exp -> level_exp -> level_exp
   
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
   val goal_of_sequent : term -> term
   val mk_sequent_term : term list -> term
   
   val nth_hyp : term -> int -> string * term
   val nth_concl : term -> int -> term
   val num_hyps : term -> int
   val declared_vars : term -> string list
   val declarations : term -> (string * term) list
   val get_decl_number : term -> string -> int
   val is_free_seq_var : int -> string -> term -> bool
   
   val concl_addr : term -> int * int
   val replace_concl : term -> term -> term
   val replace_goal : term -> term -> term          (* One subgoal *)
   
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
