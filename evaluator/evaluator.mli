open Refiner.Refiner.Term

val evaluate_term : term -> term

val add_reduction_rule : term -> (param' list -> term) -> unit
val add_simple_rule : term -> term -> unit
val add_abstraction : term -> term -> unit

(*
 *val del_reduction_rule : term -> unit
 *val del_abstraction : term -> unit
 *)
