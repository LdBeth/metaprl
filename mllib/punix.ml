(*
 * This module extends the Unix library.
 *)

external putenv : string -> int = "caml_putenv"

external execv : string -> string array -> unit = "caml_execv"
external execvp : string -> string array -> unit = "caml_execvp"

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
