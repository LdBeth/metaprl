(*
 * Abstract refiner.
 *)

module Refiner = Refiner_std.Refiner

open Refiner.RefineError

let _ = Term_exn.set_generic_refiner_exn (RefineError ("generic", GenericError))

(*
 * $Log$
 * Revision 1.4  1998/07/02 22:24:54  jyh
 * Created term_copy module to copy and normalize terms.
 *
 * Revision 1.3  1998/07/02 18:35:27  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.2  1998/06/12 20:46:07  jyh
 * Switched to term_ds.
 *
 * Revision 1.1  1998/05/28 15:00:27  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:13:59  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
