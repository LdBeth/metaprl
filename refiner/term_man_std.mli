(*
 * Manifest terms.
 *)

open Term_man_sig
open Term_std

module TermMan : TermManSig
                 with type term = Term.term
                 with type operator = Term.operator
                 with type level_exp = Term.level_exp

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:14:33  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
