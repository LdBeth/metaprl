(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.
 *)

open Term_ds
open Term_shape_sig

module TermShape : TermShapeSig
                   with type term = Term.term

(*
 * $Log$
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
