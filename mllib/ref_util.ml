(*
 * Operations on references.
 *)

open Printf
open Debug
(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Ref_util%t" eflush

(*
 * Stack operations.
 *)
let push a stack =
   stack := a::!stack

let pop stack =
   match !stack with
      h::t ->
         stack := t;
         h
    | [] ->
         raise (Invalid_argument "pop")

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
