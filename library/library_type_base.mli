(*
 * Make a combo to read from the library.
 *)

open Refiner.Refiner.Term

open File_base_type

val maybe_lib_open : unit -> unit

module IO : IOSig with type t = term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
