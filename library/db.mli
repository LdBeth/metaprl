
open Term
open Basic
open Stream
open Ascii_scan

val db_init		: string -> unit
(*val db_query		: pathname -> term * stamp list*)

val db_read		: stamp -> string -> term
val db_write		: stamp -> string -> term -> unit

(* db ascii*)
val string_to_parameter : string (*value*) -> string (*type*) -> param
val string_to_bindings	: string (*value*) -> string list

val read_term 		: (char t) -> term

val scan_level_expression	: scanner -> level_exp