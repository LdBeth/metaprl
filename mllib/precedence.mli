(*
 * A precedence is really just a partial order.
 *
 *)

(*
 * Actual precedence.
 *)
type precedence

(*
 * Relations.
 *)
type relation =
   NoRelation
 | LTRelation
 | EQRelation
 | GTRelation

(*
 * Smallest and largest elements.
 *)
val min_prec : precedence
val max_prec : precedence

(*
 * New precedence in the base.
 *)
val new_prec : unit -> precedence

(*
 * Install a relation.
 *)
val add_lt : precedence -> precedence -> unit
val add_eq : precedence -> precedence -> unit

(*
 * Get the relation.
 *)
val get_prec : precedence -> precedence -> relation

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
