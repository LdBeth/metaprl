(*
 * This module extends the Unix library.
 *)

external putenv : string -> int = "caml_putenv"

(*
 * $Log$
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
