(*
 * This file is part of Nuprl-Light, a modular, higher order
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


open Refiner.Refiner.Term
open Opname
open Nl_num

(*
 * common terms
 *)

val itoken_term		: string -> term
val inatural_term	: int -> term
val itext_term		: string -> term
val ioid_term		: object_id -> term

val imessage_term	: string list -> object_id list -> term list -> term

val iterm_term		: term -> term
val iterm_bterms	: bound_term list -> term

val ivoid_term		: term
val ivoid_term_p	: term -> bool

val number_of_inatural_term	: term -> int
val oid_of_ioid_term		: term -> object_id
val oids_of_ioid_term		: term -> object_id list
val string_of_itoken_term	: term -> string

val operator_of_term		: term -> operator
val bound_terms_of_term		: term -> bound_term list
val parameters_of_term		: term -> param list
val token_parameter_to_string	: param -> string

val dest_obid_param 		: param -> object_id
val dest_token_param		: param -> string
val dest_int_param		: param -> int
val dest_num_param		: param -> num

(*
 * failure
 *)

(* throws Exception Nuprl5 of Term *)
val error		: string list -> object_id list -> term list -> 'a

(* catches Exception Nuprl5 of Term *)
val error_handler	: (unit -> 'b) -> (term -> 'b) -> 'b

val unconditional_error_handler : (unit -> 'b) -> (term -> 'b) -> 'b

val special_error_handler :  (unit -> 'b) -> (string -> term -> 'b) -> 'b

val unwind_error	: (unit-> 'b) -> (unit -> 'c) -> 'b

(*
 * stamps
 *)

type stamp = {term: term;
	      process_id: string;
	      transaction_seq: int;
	      seq: int;
	      time: num
	      }

val print_stamp		: stamp -> unit

val dest_stamp		: stamp -> stamp
val term_to_stamp	: term -> stamp
val stamp_to_term	: stamp -> term
val stamp_to_object_id	: stamp -> object_id

val equal_stamps_p	: stamp -> stamp ->  bool

(* (in_transaction_p a b) = true <-> (in_transaction_p b a = true) *)
val in_transaction_p 	: stamp -> stamp -> bool

(* stamps from same library session can be ordered, if stamps from
 * distinct lib sessions compared then < fails.
 * transaction_less a b = true when a occured earlier than b.
 *)
val transaction_less	: stamp -> stamp -> bool

val new_stamp 		: unit ->  stamp
val get_stamp 		: unit ->  stamp


val sequence		: unit -> int
val tid			: unit -> term
val tideq		: term -> term -> bool

val term_of_unbound_term	: bound_term -> term
val unbound_bterm_p		: bound_term -> bool
val string_of_itext_term	: term -> string


val mk_nuprl5_op	: param list -> operator

val icons_term			: operator -> term -> term -> term
val hd_of_icons_term		: operator -> term -> term
val tl_of_icons_term		: operator -> term -> term


val list_to_ilist_by_op_map	: operator -> ('a -> term) -> 'a list -> term
val list_to_ilist_by_op		: operator -> term list -> term

val list_to_ilist		: term list -> term
val list_to_ilist_map		: ('a -> term) -> 'a list -> term

val map_isexpr_to_list_by_op	: operator -> (term -> 'a) -> term -> 'a list
val map_isexpr_to_list		: (term -> 'a) -> term -> 'a list
val map_isexpr_by_op		: operator -> (term -> unit) -> term -> unit

val ioption_term		: term option -> term
val option_of_ioption_term	: term -> term option

val iproperty_term		: (string * term) -> term
val property_of_iproperty_term	: term -> (string * term)

val istring_term		: string -> term
val string_of_istring_term	: term -> string


val nullp			: 'a list -> bool
val chareq			: char -> char -> bool
val inteq			: int -> int -> bool
val stringeq			: string -> string -> bool

val listeq			: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

val parmeq			: param -> param -> bool
val oideq			: object_id -> object_id -> bool

val opeq			: operator -> operator -> bool

val nuprl5_opname_p		: opname -> bool

val parmhash			: param -> int


