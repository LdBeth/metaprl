(*
 * Testing function for red-black sets.
 *)

module Ord =
struct
   type t = int

   let print i =
      Format.print_int i

   let compare i j =
      i - j
end

module IntSet = Red_black_set.MakeDebug (Ord)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
