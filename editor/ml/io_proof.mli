(*
 * Convert between proofs and terms.
 *)

include Io_proof_type

open Opname
open Dform
open Refiner.Refiner.Term

open Io_proof_type

val interface_op : opname
val implementation_op : opname

(*
 * Conversion to terms.
 *)
val term_of_proof : ('term -> term) -> 'term proof -> term
val proof_of_term : (term -> 'term) -> 'term Tactic_type.attributes -> term -> 'term proof

(*
 * This function extracts an expression that
 * evaluates to a (string * tactic) array.
 *)
val tactics_of_proof : 'term proof -> MLast.expr

(*
 * Print the proof.
 *)
val print_proof : dform_base -> term proof -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
