(*
 * ``Special'' terms to be used in reduction rules
 *
 * "canon_var" plays the same role as "var" in reduction rules but
 * the correspondig subterm should be evaluated before the reduction
 * ("call by value" instead of "call by name")
 *
 * subst (v1,v2,v3,...,vm.T;t1;t2;t3;...;tn)
 * it is an error if m!=n
 * if n=m then subst(...) is T with v1 substituted to t2, v2 - to t2, etc.
 *
 *)

module type TermEvalSig =
sig
   type term

   val is_canon_var_term : term -> bool
   val dest_canon_var : term -> string
   val mk_canon_var_term : string -> term

   val is_subst_term : term -> bool
   val dest_subst : term -> term * (string list * term list)
   val mk_subst_term : term -> (string * term) list -> term
   val make_subst_term : term -> string list -> term list -> term
   val make_1subst_term : term -> string -> term -> term
   val make_2subst_term : term -> string -> string -> term -> term -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
