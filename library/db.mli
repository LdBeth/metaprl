(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

open Refiner.Refiner.TermType
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

(* below temp for interactive debugging *)

val make_term_scanner : char t -> scanner
val myscanner : scanner ref
