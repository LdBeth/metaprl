(*
 * Operations on arrays.
 *)

(* Membership in an array *)
val mem : 'a -> 'a array -> bool
val index : 'a -> 'a array -> int
val exists : ('a -> bool) -> 'a array -> bool
val find_index : ('a -> bool) -> 'a array -> int

(* Raises Failure *)
val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
