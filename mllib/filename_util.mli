(*
 * Utilities on filenames.
 *)

val split : string -> string list

(*
 * Components of the path.
 *)
val tail : string -> string
val head : string -> string
val root : string -> string
val suffix : string -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
