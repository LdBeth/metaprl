(*
 * Manifest terms.
 *)

open Term_man_sig
open Term_ds

module TermMan : TermManSig
                 with type term = Term.term
                 with type operator = Term.operator
                 with type level_exp = Term.level_exp

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:02:08  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/28 02:53:09  nogin
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
