(*
 * Some character type operations.
 *)

(* Case conversion *)
val toupper : char -> char
val tolower : char -> char

(* Digits *)
val is_digit : char -> bool

(* Capital letter *)
val is_upperchar : char -> bool
val is_lowerchar : char -> bool

(* String operations *)
val is_capitalized : string -> bool
val is_uppercase : string -> bool
val is_lowercase : string -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
