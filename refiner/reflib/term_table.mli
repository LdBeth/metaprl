(*
 * Make a hashtable for terms based on patterns.
 * Essentially, we want to be able to construct tables of pairs
 *    (pattern, 'a)
 * where pattern is a pattern that matches terms.  The lookup
 * function:
 *    lookup : table -> term -> 'a
 * should retrieve the value with the most specific pattern match.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.Rewrite

type 'a term_table

val new_table : unit -> 'a term_table
val insert : 'a term_table -> term -> 'a -> 'a term_table
val join_tables : 'a term_table -> 'a term_table -> 'a term_table
val lookup : string -> 'a term_table -> term -> rewrite_stack * rewrite_item list * 'a

(*
 * Destruction.
 *)
type 'a table_entry =
   TableEntry of term * 'a
 | TableTable of 'a term_table

val is_empty_table : 'a term_table -> bool
val equal_tables : 'a term_table -> 'a term_table -> bool
val dest_table : 'a term_table -> 'a table_entry * 'a term_table

(*
 * Debugging.
 *)
val print_table : 'a term_table -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
