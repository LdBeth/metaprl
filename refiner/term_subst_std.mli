(*
 * Substituion, alpha equality, and unification.
 *)

open Term_std
open Term_subst_sig

module TermSubst : TermSubstSig
                   with type term = Term.term
                   with type param = Term.param

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:15:01  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
