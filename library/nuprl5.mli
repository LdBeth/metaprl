open Opname
open Term
open BigInt

val nuprl5_opname	: opname

(* parameter mapping *)

val make_bool_parameter	: bool -> param
val make_time_parameter	: bigint -> param

val time_parameter_p	: param -> bool
val bool_parameter_p	: param -> bool

val destruct_time_parameter	: param -> bigint
val destruct_bool_parameter	: param -> bool


(* common terms *)
