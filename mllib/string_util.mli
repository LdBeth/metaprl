(*
 * Extra operations on strings.
 *)

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

(*
 * Functions for cacthing errors.
 *)
val create : string -> int -> string
val make : string -> int -> char -> string
val sub : string -> string -> int -> int -> string
val blit : string -> string -> int -> string -> int -> int -> unit
val set : string -> string -> int -> char -> unit
val get : string -> string -> int -> char

(*
 * Find a char in a string.
 *)
val strchr : string -> char -> int

(*
 * Mapping.
 *)
val for_all : (char -> bool) -> string -> bool

(*
 * Membership.
 *)
val mem : char -> string -> bool

(*
 * Get the index of any char in the set.
 *)
val index_set : string -> string -> int
val rindex_set : string -> string -> int

(*
 * Split at a char.
 *)
val split : char -> string -> string list
val split_set : string -> string -> string list

(*
 * Cat strings together.
 *)
val concat : string -> string list -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
