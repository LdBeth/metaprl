
open Term
open Basic
open Stream
open Ascii_scan

val db_init		: string -> bool (* ascii? *) -> unit
val stamp_and_type_of_idata_persist_term	: term -> (stamp * string)

(*val db_query		: pathname -> term * stamp list*)

val db_read		: stamp -> string -> term
val db_write		: stamp -> string -> term -> unit

(* db ascii*)
val string_to_parameter : string (*value*) -> string (*type*) -> param
val string_to_bindings	: string (*value*) -> string list


val string_to_term	: string -> term
val session_string_to_term	: string -> term
