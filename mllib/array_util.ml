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
 * Parts for collecting arrays.
 *)
type ('a, 'b) array_part =
   ArrayElement of 'a
 | ArrayArray of 'b * int * int

(*
 * Boolean values.
 *)
let all_true v =
   let rec search i len v =
      i = len || (v.(i) && search (succ i) len v)
   in
      search 0 (Array.length v) v

let exists_true v =
   let rec search i len v =
      i <> len && (v.(i) || search (succ i) len v)
   in
      search 0 (Array.length v) v

let for_all f v =
   let rec search f i len v =
      i = len || (f v.(i) && search f (succ i) len v)
   in
      search f 0 (Array.length v) v

let exists f v =
   let rec search f i len v =
      i <> len && (f v.(i) || search f (succ i) len v)
   in
      search f 0 (Array.length v) v

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
 * Map over a subarray.
 *)
let sub_map f a i len =
   if i < 0 || len < 0 || i + len > Array.length a then
      raise (Invalid_argument "sub_map")
   else
      match len with
         0 ->
            [||]
       | 1 ->
            [| f (Array.unsafe_get a i) |]
       | _ ->
            let a' = Array.create len (f (Array.unsafe_get a i)) in
               for j = 1 to len - 1 do
                  Array.unsafe_set a' j (f (Array.unsafe_get a (i + j)))
               done;
               a'

(*
 * Compute the total length of the parts.
 * As a side-effect, we raise an exception if
 * any of the subarrays are ill-defined.
 *)
let rec parts_length len = function
   [] ->
      len
 | part :: parts ->
      match part with
         ArrayElement _ ->
            parts_length (len + 1) parts
       | ArrayArray (a, i, len') ->
            if i < 0 || len' < 0 || i + len' > Array.length a then
               raise (Invalid_argument "Array.collect")
            else
               parts_length (len + len') parts

(*
 * Add the parts to the array.
 * We are guaranteed that the arrays will be in bounds.
 *)
let rec collect_append a off = function
   [] ->
      a
 | part :: parts ->
      match part with
         ArrayElement x ->
            Array.unsafe_set a off x;
            collect_append a (off + 1) parts
       | ArrayArray (a', i, len) ->
            if len <> 0 then
               Array.blit a' i a off len;
            collect_append a (off + len) parts

(*
 * Collect function works in two parts.
 * The first part creates the initial array,
 * and the second part adds to it.
 *)
let rec collect = function
   [] ->
      [||]
 | part :: parts ->
      match part with
         ArrayElement x ->
            let len = parts_length 1 parts in
            let a' = Array.create len x in
               collect_append a' 1 parts

       | ArrayArray (a, i, len) ->
            match len with
               0 ->
                  collect parts
             | len ->
                  let len' = parts_length len parts in
                  let a' = Array.create len' a.(i) in
                     if len > 1 then
                        Array.blit a (i + 1) a' 1 (len - 1);
                     collect_append a' len parts

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
