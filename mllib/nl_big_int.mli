(*
 * Our implementation of big_int.
 *)

type big_int = bool * int list

val big_int_of_int : int -> big_int

(*
 * Operations.
 *)
val add_big_int : big_int -> big_int -> big_int
val sub_big_int : big_int -> big_int -> big_int
val mult_big_int : big_int -> big_int -> big_int
val div_big_int : big_int -> big_int -> big_int
val quo_big_int : big_int -> big_int -> big_int
val mod_big_int : big_int -> big_int -> big_int
val rem_big_int : big_int -> big_int -> big_int

val abs_big_int : big_int -> big_int

(*
 * Comparisons.
 *)
val eq_big_int : big_int -> big_int -> bool
val compare_big_int : big_int -> big_int -> int

(*
 * Conversion.
 *)
val is_integer_big_int : big_int -> bool
val integer_big_int : big_int -> int

val string_of_big_int : big_int -> string
val big_int_of_string : string -> big_int

(*
 * Special cases.
 *)
val div10 : big_int -> int * big_int
val mult10 : big_int -> big_int

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
