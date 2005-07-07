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

open Basic
open Refiner.Refiner.TermType

type term_entry
type termtable

val make_termtable	: unit -> termtable

val apply_broadcast	: termtable ->
				term (* data *) ->
				term (* description *) ->
				stamp (* transaction *) ->
				stamp option (* auto-commit *) ->
				unit

val termtable_lookup	: termtable -> stamp -> object_id -> term_entry

val termtable_unit_map	: termtable -> stamp -> (object_id -> term_entry -> unit) -> unit
val termtable_map	: termtable -> stamp -> (object_id -> term_entry -> 'a option) -> 'a list



(* more specific funcs for termtable variants *)
val roots		: termtable -> stamp -> (string * object_id) list
val directory_p	: termtable -> stamp -> object_id -> bool
val directory_children	: termtable -> stamp -> object_id -> (string * object_id) list

(* would be nice if termtable were functorized so
that term_entry need not be defined apriori. *)
