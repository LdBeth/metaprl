
(************************************************************************
 * Types                                                                *
 ************************************** **********************************)
open Num
open Int32
open Registry

type mbnode =   Mbint of int32 | Mnode of mbterm
 and mbterm = mbnode array

val stream_mode: string

val make_mbnode:  int32 -> int -> mbterm
val mbnode:  int32 -> mbterm list -> mbterm

val mb_string: string -> mbterm
val mb_stringq: string -> int32  -> mbterm
val string_value: mbterm -> string

(*val mb_integerb: int32 -> mbnode*)
val mb_integer: int -> mbterm
val mb_integerq: int (*value*) -> int32 (*label*) -> mbterm
val integer_value: mbterm -> int


val mb_number: num -> mbterm
val mb_numberq: num (*value*) -> int32 (*label*) -> mbterm
val number_value: mbterm -> num

val subterm_types: int32 -> int32
val mbnode_subtermq:  mbterm -> int -> mbnode
val mbnode_nSubtermsq: mbterm -> int
val mbnode_label: mbterm -> int32
val mbnode_labelq: mbterm -> int32

val write_node: mbterm -> out_channel -> unit
val initialize_base64:  unit
val read_node: in_channel -> mbterm
val print_node: mbterm -> unit

val numeric_label: string -> int32
val symbolic_label: int32 ->  string

val mbs_String: int32
val mbs_LongInteger: int32
val mBS_Attributes: int32


(*debugging purposes*)
(*
val mbnode_nsubterms: mbnode -> int
val declare_local_stringid: string -> int32 -> int32
val mbnode_subterm: mbNode -> int -> mbnode
val global_assign:
val loop_over_subterms: mbterm -> (int -> string -> unit) -> unit

val  minimum_global_numeric_label:  int32
val  maximum_global_numeric_label: int32
val  minimum_local_numeric_label: int32
val  maximum_local_numeric_label: int32

val write_32bit: int32 -> out_channel -> unit
val print_32bit: int32  -> unit
val  next_local_label: int32 ref
val  buffer: int ref
val   flush_buffer: int ref-> out_channel -> int ref -> unit
val   byte_count: int ref
val   base64_by_char_table: int  array
val   base64_by_num_table:  char  array
val    base64_char_count: int ref
val    base64_icount: int ref
val    base64_ibuffer: int ref
val   cnt: int ref
val subterm_types: int32 ->  regval
*)

