(*
 * Addressed operations on terms.
 *)

open Term_std
open Term_addr_sig

module TermAddr : TermAddrSig
                  with type term = Term.term

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:02:24  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:21  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
