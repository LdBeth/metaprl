(*
 * Substituion, alpha equality, and unification.
 *)

open Term_ds
open Term_subst_sig

module TermSubst : TermSubstSig
                   with type term = Term.term
                   with type param = Term.param

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:02:18  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/28 02:53:12  nogin
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
