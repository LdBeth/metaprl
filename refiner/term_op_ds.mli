(*
 * Operations on standard terms.
 *)

open Term_op_sig
open Term_ds

module TermOp : TermOpSig
                with type term = Term.term
                with type operator = Term.operator
                with type level_exp = Term.level_exp

(*
 * $Log$
 * Revision 1.1  1998/05/28 02:53:10  nogin
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
