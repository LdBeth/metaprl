(*
 * An "infinite" buffer.  The buffer grows as the space requirements
 * increase.
 *)

type t

val create : unit -> t
val putc : t -> char -> unit
val puts : t -> string -> unit
val clear : t -> unit
val gets : t -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
