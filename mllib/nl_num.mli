(*
 * Our slow implementation of numbers
 * without using C libraries.
 *)

type big_int

type num =
   Int of int
 | Big_int of big_int

(*
 * Operations.
 *)
val add_num : num -> num -> num
val sub_num : num -> num -> num
val mult_num : num -> num -> num
val div_num : num -> num -> num
val mod_num : num -> num -> num
val quo_num : num -> num -> num
val rem_num : num -> num -> num
val abs_num : num -> num
val power_num : num -> num -> num

val succ_num : num -> num
val pred_num : num -> num

(*
 * Comparision.
 *)
val lt_num : num -> num -> bool
val le_num : num -> num -> bool
val eq_num : num -> num -> bool
val ge_num : num -> num -> bool
val gt_num : num -> num -> bool
val compare_num : num -> num -> int

(*
 * Conversion.
 *)
val is_integer_num : num -> bool
val integer_num : num -> int
val num_of_int : int -> num
val int_of_num : num -> int
val string_of_num : num -> string
val num_of_string : string -> num

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
