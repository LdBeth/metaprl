(*
 * This is bogus.
 * We have to bypass the type system a little to get the generic
 * exception.
 *)
val generic_refiner_exn : exn ref
val set_generic_refiner_exn : exn -> unit

(*
 * $Log$
 * Revision 1.1  1998/07/02 18:36:18  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
