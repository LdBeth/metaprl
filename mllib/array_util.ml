(*
 * Operations on arrays.
 *)

open Printf
open Nl_debug

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

let append_list a = function
   [] -> a
 | hd :: tl ->
      let l = Array.length a in
      let res = Array.create (l + List.length tl + 1) hd in
      for i = 0 to pred l do Array.unsafe_set res i (Array.unsafe_get a i) done;
      let rec aux i = function
         [] -> res
       | hd :: tl ->
            Array.unsafe_set res i hd;
            aux (succ i) tl
      in aux (succ l) tl

let replace a i j = function
   [] ->
      if j>0 then Array.append (Array.sub a 0 i) (Array.sub a (i+j) (Array.length a-i-j))
      else raise (Invalid_argument "Array_util.replace")
 | hd :: tl ->
      let l = Array.length a in
      let ij = i + j in 
      if i>=0 && j>0 && ij<=l then 
         let dl = List.length tl - j +1 in
         let res = Array.create (l+dl) hd in
         for k=0 to (pred i) do 
            Array.unsafe_set res k (Array.unsafe_get a k)
         done;
         for k=ij to (pred l) do
            Array.unsafe_set res (k+dl) (Array.unsafe_get a k)
         done;
         let rec aux k = function
            [] -> res
          | hd :: tl ->
               Array.unsafe_set res k hd;
               aux (succ k) tl
         in aux (succ i) tl
      else raise (Invalid_argument "Array_util.replace")

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
