
open Term
open Basic


val db_init		: stamp -> string -> unit
 (*val db_query		: pathname -> term * stamp list*)

val db_read		: stamp -> string -> term
val db_write		: stamp -> string -> term -> unit
