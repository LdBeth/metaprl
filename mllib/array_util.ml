(*
 * Operations on arrays.
 *)

open Printf
open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Array_util%t" eflush

(*
 * Membership in an array.
 *)
let mem i v =
   let l = Array.length v in
   let rec aux j =
      j < l & ( i = v.(j) or
                aux (j + 1) )
   in
      aux 0

(*
 * Membership in an array.
 *)
let index i v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if i = v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * Membership in an array.
 *)
let exists f v =
   let l = Array.length v in
   let rec aux j =
      j < l & ( f v.(j) or
                aux (j + 1) )
   in
      aux 0

let find_index f v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if f v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0

(*
 * Iterate over two arrays.
 *)
let iter2 f a1 a2 =
   let len = Array.length a1 in
      if Array.length a2 <> len then
         raise (Failure "Array.iter2")
      else
         let rec apply f a1 a2 i len =
            if i < len then
               begin
                  f a1.(i) a2.(i);
                  apply f a1 a2 (i + 1) len
               end
         in
            apply f a1 a2 0 len

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
