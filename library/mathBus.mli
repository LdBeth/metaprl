
(************************************************************************
 * Types                                                                *
 ************************************** **********************************)

open BigInt
open Registry

type mbnode =   Mbint of bigint | Mnode of mbterm
 and mbterm = mbnode array

val stream_mode: string

val make_mbnode:  bigint -> int -> mbterm
val mbnode:  bigint -> mbterm list -> mbterm

val mb_string: string -> mbterm
val mb_stringq: string -> bigint  -> mbterm
val string_value: mbterm -> string

(*val mb_integerb: bigint -> mbnode*)
val mb_integer: int -> mbterm
val mb_integerq: int (*value*) -> bigint (*label*) -> mbterm
val integer_value: mbterm -> int 

val subterm_types: bigint -> regval
val mbnode_subtermq:  mbterm -> int  -> mbnode
val mbnode_nSubtermsq: mbterm -> int
val mbnode_label: mbterm -> bigint
val mbnode_labelq: mbterm -> bigint

val write_node: mbterm -> out_channel -> unit
val initialize_base64:  unit 
val read_node: in_channel -> mbterm
val print_node: mbterm -> unit

val numeric_label: string -> regval
val numeric_label2: string -> bigint
val symbolic_label: bigint ->  string

val mbs_String: bigint
val mbs_LongInteger: bigint


(*debugging purposes*)
(*
val mbnode_nsubterms: mbnode -> int
val declare_local_stringid: string -> bigint -> bigint
val mbnode_subterm: mbNode -> int -> mbnode
val global_assign:
val loop_over_subterms: mbterm -> (int -> string -> unit) -> unit
    
val  minimum_global_numeric_label:  bigint
val  maximum_global_numeric_label: bigint
val  minimum_local_numeric_label: bigint
val  maximum_local_numeric_label: bigint

val write_32bit: bigint -> out_channel -> unit
val print_32bit: bigint  -> unit
val  next_local_label: bigint ref
val  buffer: int ref
val   flush_buffer: int ref-> out_channel -> int ref -> unit
val   byte_count: int ref
val   base64_by_char_table: int  array
val   base64_by_num_table:  char  array
val    base64_char_count: int ref
val    base64_icount: int ref
val    base64_ibuffer: int ref
val   cnt: int ref
val subterm_types: bigint ->  regval
*)

