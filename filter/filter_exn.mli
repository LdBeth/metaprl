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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
