(*
 * Define a printer for all escaping refiner exceptions.
 *)

open Refiner.Refiner.RefineError
open Rformat
open Dform
open Dform_print

val format_refine_error : dform_base -> buffer -> string -> refine_error -> unit
val format_exn : dform_base -> buffer -> exn -> unit

val print : dform_base -> ('a -> 'b) -> 'a -> 'b
val print_exn : dform_mode_base -> out_channel -> string -> exn -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
