(*
 * Convert between proofs and terms.
 *)

include Io_proof_type

open Opname
open Refiner.Refiner.Term

open Io_proof_type

val interface_op : opname
val implementation_op : opname

(*
 * During the conversion to terms, the tactics are extracted
 * and returned with the term.
 *)
val term_of_proof : proof -> term
val proof_of_term : term -> proof

(*
 * This function extracts an expression that
 * evaluates to a (string * tactic) array.
 *)
val tactics_of_proof : proof -> MLast.expr

(*
 * $Log$
 * Revision 1.1  1998/05/28 13:45:36  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
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
