 (*module type BigSig =
sig*)
(************************************************************************
 * Types                                                                *
 ************************************************************************)

type int32

exception IntSize of string * int

val create : int -> int32
val mk_bint : int -> int32
val make_int32 : int * int -> int32
val dest_bint : int32 -> int
val dest_int32 : int32 -> int * int

val int_of_int32 : int32 -> int
val int32_of_int : int -> int32

val print_int32 : int32 -> unit

val lband : int32 -> int32 -> int32
val lbor : int32 -> int32 -> int32
val lbsl : int32 -> int -> int32
(*
val lbsr : int32 -> int -> int32
val basr : int32 -> int -> int32
*)
val bplus : int32 -> int -> int32
val bminus : int32 -> int -> int32


val bequal : int32 -> int32 -> bool
val blt : int32 -> int32 -> bool
val bgt : int32 -> int32 -> bool
val blte : int32 -> int32 -> bool
val bgte : int32 -> int32 -> bool

val bdecr : int32 ref -> unit

 (* end*)
