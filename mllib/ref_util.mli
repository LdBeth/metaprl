(*
 * Reference operations.
 *)

(* Stack ops *)
val push : 'a -> ('a list) ref -> unit
val pop : ('a list) ref -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
