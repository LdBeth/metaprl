(*
 * Convert between proofs and terms.
 *)

open Opname
open Refiner.Refiner.Term

open Filter_proof_type

val interface_op : opname
val implementation_op : opname

(*
 * During the conversion to terms, the tactics are extracted
 * and returned with the term.
 *)
val term_of_proof : proof -> term
val proof_of_term : term -> proof

val tactics_of_proof : proof -> (string * MLast.expr) array

(*
 * $Log$
 * Revision 1.3  1998/05/27 15:12:58  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.2  1998/04/22 22:44:28  jyh
 * *** empty log message ***
 *
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
