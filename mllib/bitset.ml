(*
 * Packed boolean array.
 *)

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
 * $Log$
 * Revision 1.1  1998/04/08 14:57:13  jyh
 * ImpDag is in mllib.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
