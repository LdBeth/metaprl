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

open Lint32

type regtb

val global_registry: regtb
val local_registry: regtb
val token_table: (string, int) Hashtbl.t
val index_table: (int, string) Hashtbl.t

val registry_types: string list ref

val get_registry_file: unit -> string
val token_file: string

val define_registry_type: string -> bool -> unit

val clear_registry: bool -> bool -> unit

val registry_lookup_value: string -> string -> lint32
val registry_lookup_identifier: string -> lint32 -> string
val registry_store_local: string -> string -> lint32 -> unit

val read_string: in_channel -> string
 (*val read_number: in_channel -> num*)
val read_int32 : in_channel -> lint32

val read_registry: unit -> unit
val read_tokens: unit

 (*val default_registry_files: unit ->

val registry_header_text: unit -> string

val generate_registry_declarations &optional ofile file =
*)

