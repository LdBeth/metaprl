(*
 * Alexey's utils on special terms.
 *)

open Term_std
open Term_eval_sig

module TermEval : TermEvalSig
                  with type term = Term.term

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:02:27  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:28  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
