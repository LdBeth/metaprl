exception Not_unifiable
exception Failed

(* Utilities *)

val is_const : string -> bool
val is_var : string -> bool
val r_1 : 'a list -> 'b list -> 'c list -> bool
val r_2 : 'a list -> 'b list -> 'c list -> bool
val r_3 : 'a list -> 'b list -> 'a list -> bool
val r_4 : string list -> 'a list -> string list -> bool
val r_5 : string list -> 'b list -> bool
val r_6 : string list -> 'a list -> string list -> bool
val r_7 : string list -> string list -> bool
val r_8 : string list -> 'a list -> string list -> bool
val r_9 : string list -> 'a list -> string list -> bool
val r_10 : string list -> string list -> bool
val com_subst : 'a -> 'a list -> 'a list -> 'a list
val apply_element : 'a -> 'a list -> 'a list -> 'a list -> 'a list * 'a list

(* Debugging *)

val print_equations : (string list * (string list * string list)) list -> unit

val print_tunify : int * (string * string list) list -> unit
