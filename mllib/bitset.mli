(*
 * All we want is a packed boolean array.
 *)

type t

val create : int -> t
val set : t -> int -> unit
val reset : t -> int -> unit
val get : t -> int -> bool

(*
 * $Log$
 * Revision 1.1  1998/04/08 14:57:15  jyh
 * ImpDag is in mllib.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
