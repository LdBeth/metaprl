(*
 * Addressed operations on terms.
 *)

open Term_ds
open Term_addr_sig

module TermAddr : TermAddrSig
                  with type term = Term.term

(*
 * $Log$
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
