(*
 * Operations on arrays.
 *)

(* Membership in an array *)
val mem : 'a -> 'a array -> bool
val index : 'a -> 'a array -> int
val exists : ('a -> bool) -> 'a array -> bool
val find_index : ('a -> bool) -> 'a array -> int

(* Raises Failure *)
val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit

(*
 * replace A i j B creates a copy of array A
 * where j elements with indeces from i to i + j - 1 are
 * replaced with B's elements (if B has length different from j,
 * new array will have length different from A).
 * Raises invalid_argument if numbers out of range.
 *)
val replace : 'a array -> int -> int -> 'a list -> 'a array

val append_list : 'a array -> 'a list -> 'a array

(*
 * Map over a sub-array.
 *)
val sub_map : ('a -> 'b) -> 'a array -> int -> int -> 'b array

(*
 * This function builds arrays out of sub-arrays.
 *)
type ('a, 'b) array_part =
   ArrayElement of 'a
 | ArrayArray of 'b * int * int

val collect : ('a, 'a array) array_part list -> 'a array

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
