(*
 * This module extends the Unix library.
 *)

external putenv : string -> int = "caml_putenv"

external execv : string -> string array -> unit = "caml_execv"
external execvp : string -> string array -> unit = "caml_execvp"

(*
 * $Log$
 * Revision 1.1  1998/06/17 16:59:01  jyh
 * Added punix.ml
 *
 * Revision 1.2  1998/06/03 22:19:01  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.1  1997/08/06 16:16:57  jyh
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
