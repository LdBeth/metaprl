(*
 * Commands to manipulate debug variables.
 *)

(*
 * Set a particular debug flag.
 *)
val set_debug : string -> bool -> unit
val get_debug : string -> string * bool
val debuggers : unit -> (string * string * bool) array

val init : unit -> unit

(*
 * $Log$
 * Revision 1.2  1998/04/28 18:30:28  jyh
 * ls() works, adding display.
 *
 * Revision 1.1  1998/04/24 02:42:31  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/01/09 19:29:59  jyh
 * Removed NT client.
 *
 * Revision 1.1  1997/11/04 21:36:26  jyh
 * Interim update.
 * Restart not quite works yet.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
