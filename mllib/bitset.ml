(*
 * Packed boolean array.
 *)

open Printf
open Nl_debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Bitset%t" eflush

let int_size = 30

type t = int array

let create len =
   Array.create ((len + int_size - 1) / int_size) 0

let set bits i =
   let index = i / int_size in
   let bit = i mod int_size in
      bits.(index) <- bits.(index) lor (1 lsl bit)

let reset bits i =
   let index = i / int_size in
   let bit = i mod int_size in
      bits.(index) <- bits.(index) land (lnot (1 lsl bit))

let get bits i =
   let index = i / int_size in
   let bit = i mod int_size in
      (bits.(index) land (1 lsl bit)) <> 0

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
