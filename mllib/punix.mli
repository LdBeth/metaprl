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
 * $Log$
 * Revision 1.1  1998/06/17 16:59:01  jyh
 * Added punix.ml
 *
 * Revision 1.2  1998/06/03 22:19:02  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.1  1997/08/06 16:16:58  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
