 (*module type BigSig =
sig*)
(************************************************************************
 * Types                                                                *
 ************************************************************************)

type bigint

exception IntSize of string * int

val create : int -> bigint
val mk_bint : int -> bigint
val make_bigint : int * int -> bigint
val dest_bint : bigint -> int
val dest_bigint : bigint -> int * int

val print_bigint : bigint -> unit

val lband : bigint -> bigint -> bigint
val lbor : bigint -> bigint -> bigint
val lbsl : bigint -> int -> bigint
(*
val lbsr : bigint -> int -> bigint
val basr : bigint -> int -> bigint
*)
val bplus : bigint -> int -> bigint
val bminus : bigint -> int -> bigint

val bequal : bigint -> bigint -> bool
val blt : bigint -> bigint -> bool
val bgt : bigint -> bigint -> bool
val blte : bigint -> bigint -> bool
val bgte : bigint -> bigint -> bool

val bdecr : bigint ref -> unit

 (* end*)
