(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.
 *)

open Term_std
open Term_shape_sig

module TermShape : TermShapeSig
                   with type term = Term.term

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:14:49  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
