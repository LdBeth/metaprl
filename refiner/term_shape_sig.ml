(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.
 *)

module type TermShapeSig =
sig
   type term
   type shape

   val shape_of_term : term -> shape
   val print_shape : out_channel -> shape -> unit
end

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:14:46  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
