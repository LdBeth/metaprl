(*
 * Addressed operations on terms.
 *)

open Term_sig
open Term_op_sig
open Term_addr_sig

module TermAddr (**)
   (Term : TermSig)
   (TermOp : TermOpSig
    with type term = Term.term)
: TermAddrSig
  with type term = Term.term

(*
 * $Log$
 * Revision 1.1  1998/06/03 15:23:42  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.1  1998/05/28 15:01:59  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/28 02:53:06  nogin
 * Splitted Term_ds and Term_ds_simple modules into a smaller modules
 * for use in the functorized refiner
 *
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
