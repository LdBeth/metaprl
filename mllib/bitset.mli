(*
 * All we want is a packed boolean array.
 *)

type t

val create : int -> t
val set : t -> int -> unit
val reset : t -> int -> unit
val get : t -> int -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
