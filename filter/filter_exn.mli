(*
 * Print exceptions from filter, refiner, and others.
 *)

open Rformat
open Dform
open Dform_print

val format_exn : dform_base -> buffer -> exn -> unit

val print : dform_base -> ('a -> 'b) -> 'a -> 'b
val print_exn : dform_base -> out_channel -> exn -> 'a

(*
 * $Log$
 * Revision 1.1  1998/04/28 18:30:09  jyh
 * ls() works, adding display.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
