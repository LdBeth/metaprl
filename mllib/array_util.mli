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

(* Test boolean values *)
val all_true : bool array -> bool
val exists_true : bool array -> bool
val for_all : ('a -> bool) -> 'a array -> bool
val exists : ('a -> bool) -> 'a array -> bool

(*
 * replace A i j B creates a copy of array A
 * where j elements with indices from i to i + j - 1 are
 * replaced with B's elements (if B has length different from j,
 * new array will have length different from A).
 * Raises invalid_argument if numbers out of range.
 *)
val replace : 'a array -> int -> int -> 'a list -> 'a array

val append_list : 'a array -> 'a list -> 'a array
val append_list_array : 'a array -> 'a list -> 'a array -> 'a array

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

(* In-place quicksort of an array. Uses code from
 * http://www-staff.mcs.uts.edu.au/~cbj/FISh/Benchmarks/Plots/Quicksort2/Ocaml/Source/benchmark.ml
 *
 * After we upgrade to Ocaml 3.0, we may consider using their sort.
 *)
val qsort: ('a -> 'a -> int) -> 'a array -> unit

(* Sorts an array, than eliminates the duplicate elements
 * and moves the remaining elements into an initial segment
 * of the input array. Returns the # of distinct elements.
 *)
val distinct: ('a -> 'a -> int) -> 'a array -> int

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
