(*
 * Convert between terms and strings.
 * We use Obj module to marshal them.
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta

val term_of_string : string -> term
val string_of_term : term -> string

val mterm_of_string : string -> meta_term
val string_of_mterm : meta_term -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
