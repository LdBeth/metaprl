(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Authors: Lori Lorigo, Richard Eaton
 *)


(************************************************************************
 * Types                                                                *
 ************************************** **********************************)
open Mp_num
open Lint32
open Registry

type mbnode =   Mbint of int32 | Mnode of mbterm
 and mbterm = mbnode array

val use_unicode:  bool ref

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
val mbs_Token: int32
val mbs_LongInteger: int32
val mBS_Attributes: int32


(*debugging purposes*)

val mbnode_nSubterms: mbterm -> int
val mbnode_subterm: mbterm -> int -> mbnode
val loop_over_subterms: mbterm -> (int -> string option -> unit) -> unit

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



