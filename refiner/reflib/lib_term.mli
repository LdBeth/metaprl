(*
 * Library term functions.
 * Override the normal mk_term and dest_term, so
 * that they work on special terms as well.
 *)

open Refiner.Refiner.TermType

val mk_term : operator -> bound_term list -> term
val make_term : term' -> term
val dest_term : term -> term'

(* Represents sequents using "ugly" generic terms *)
val ugly_term_of_sequent_term : term -> term
val sequent_term_of_ugly_term : term -> term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
