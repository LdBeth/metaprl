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
 * $Log$
 * Revision 1.3  1998/07/03 22:05:07  jyh
 * IO terms are now in term_std format.
 *
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
