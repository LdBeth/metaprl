(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.
 *)

open Term_simple_sig
open Term_shape_sig

module TermShape (Term : TermSimpleSig)
: TermShapeSig
  with type term = Term.term

(*
 * $Log$
 * Revision 1.1  1998/06/03 15:23:49  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.1  1998/05/28 15:02:15  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/28 02:53:11  nogin
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
