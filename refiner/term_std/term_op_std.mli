(*
 * Operations on standard terms.
 *)

open Term_op_sig
open Term_std

module TermOp : TermOpSig
                with type term = Term.term
                with type operator = Term.operator
                with type level_exp = Term.level_exp

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:02:36  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:44  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
