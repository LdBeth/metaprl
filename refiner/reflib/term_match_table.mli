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

(*
 * Table type.
 *)
type ('a, 'b) term_table

(*
 * This is the info we keep for the entries in the table.
 *)
type 'a info_entry =
   { info_term : term;
     info_redex : rewrite_redex;
     info_value : 'a
   }

(*
 * Create a new table.
 *)
val new_table : unit -> ('a, 'b) term_table

(*
 * Table operations.
 *)
val insert : ('a, 'b) term_table -> term -> 'a -> ('a, 'b) term_table
val join_tables : ('a, 'b) term_table -> ('a, 'b) term_table -> ('a, 'b) term_table

(*
 * The function argument is a function that takes the list
 * of entries for a term and compacts them.  It is perfectly fine
 * for this function to be the identity.
 *)
val lookup : string -> ('a, 'b) term_table ->
   ('a info_entry list -> 'b info_entry list) ->
   term -> rewrite_item list * 'b

(*
 * Destruction.
 *)
type ('a, 'b) table_entry =
   TableEntry of term * 'a
 | TableTable of ('a, 'b) term_table

val is_empty_table : ('a, 'b) term_table -> bool
val equal_tables : ('a, 'b) term_table -> ('a, 'b) term_table -> bool
val dest_table : ('a, 'b) term_table -> ('a, 'b) table_entry * ('a, 'b) term_table

(*
 * Debugging.
 *)
val print_table : ('a, 'b) term_table -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
