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
 * $Log$
 * Revision 1.1  1998/02/19 17:13:54  jyh
 * Splitting filter_parse.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
