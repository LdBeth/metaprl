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
 * Normalize a proof when it is unmarshaled.
 *)
val normalize_proof : proof -> unit

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
 * Print the proof.
 *)
val print_proof : dform_base -> proof -> unit

(*
 * $Log$
 * Revision 1.2  1998/06/15 22:28:59  jyh
 * Added CZF.
 *
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
