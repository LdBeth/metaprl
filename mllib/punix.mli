(*
 * This module extends the Unix library.
 *)

val putenv : string -> int

(*
 * We make our own, because somehow the win32 version seems to fail.
 *)
val execvp : string -> string array -> unit
val execv : string -> string array -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
