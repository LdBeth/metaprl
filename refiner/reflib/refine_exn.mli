(*
 * Define a printer for all escaping refiner exceptions.
 *)

open Refiner.Refiner.RefineErrors
open Rformat
open Dform
open Dform_print

val format_rewrite_error : dform_base -> buffer -> rewrite_error -> unit
val format_refine_error : dform_base -> buffer -> refine_error -> unit
val format_exn : dform_base -> buffer -> exn -> unit

val print : dform_base -> ('a -> 'b) -> 'a -> 'b
val print_exn : dform_mode_base -> out_channel -> string -> exn -> 'a

(*
 * $Log$
 * Revision 1.2  1998/07/01 04:37:03  nogin
 * Moved Refiner exceptions into a separate module RefineErrors
 *
 * Revision 1.1  1998/05/28 15:01:06  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.4  1998/05/27 15:13:55  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.3  1998/04/28 18:30:44  jyh
 * ls() works, adding display.
 *
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
