(*
 * Convert between proofs and terms.
 *)

open Opname
open Term

open Filter_proof_type

val interface_op : opname
val implementation_op : opname

val term_of_proof : proof -> term
val proof_of_term : term -> proof

(*
 * $Log$
 * Revision 1.1  1998/04/17 01:31:04  jyh
 * Editor is almost constructed.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
