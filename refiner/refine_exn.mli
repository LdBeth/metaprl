(*
 * Define a printer for all escaping refiner exceptions.
 *)

open Refine_sig

val string_of_refine_error : refine_err -> string
val print_exn : ('a -> 'b) -> 'a -> 'b

(*
 * $Log$
 * Revision 1.2  1998/04/23 20:04:39  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.1  1998/04/09 15:26:42  jyh
 * Added strip_mfunction.
 *
 * Revision 1.1  1998/04/08 14:57:34  jyh
 * ImpDag is in mllib.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
