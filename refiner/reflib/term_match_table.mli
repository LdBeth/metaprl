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
 * logical framework that provides a logical term_programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1999-2006 MetaPRL Group, Cornell University and
 * California Institute of Technology
 *
 * This term_program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This term_program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this term_program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

open Refiner.Refiner.TermType
open Refiner.Refiner.Rewrite
open Mp_resource

(*
 * Table type.
 *)
type 'a term_table
type 'a term_map_table

type 'a lazy_lookup = unit -> ('a * 'a lazy_lookup) option

(*
 * Debugging.
 *)
val debug_term_table : bool ref

(*
 * Table management.  Most recently added items will be preferred in lookups.
 *)
val empty_table : 'a term_table
val empty_map_table : 'a term_map_table

(*
 * Standard interface.
 *
 * Lookup functions take an additional selector function; only selected items
 * could be returned.
 * The lookup_bucket find the most specific _pattern_ that has matching entries
 * end returns all the matching entries for that pattern. The list is always non-nil.
 *)
val add_item : 'a term_table -> term -> 'a -> 'a term_table
val lookup_rwi : 'a term_table -> ('a -> bool) -> term -> (rewrite_item list * 'a) option
val lookup : 'a term_table -> ('a -> bool) -> term -> 'a option
val lookup_all : 'a term_table -> ('a -> bool) -> term -> 'a lazy_lookup
val lookup_bucket : 'a term_table -> ('a -> bool) -> term -> ('a list) option

(*
 * Create a resource_info that can be used to create a resource As an input
 * this function takes the extractor function that uses table to do the actual
 * resource work.
 *)
val table_resource_info :
   ('a term_table -> 'b) ->
   (term * 'a, 'a term_table, 'b) resource_info

(* term -> term  mappings *)
val add_map : 'a term_map_table -> term -> term list -> 'a -> 'a term_map_table
val lookup_rmap : 'a term_map_table -> ('a -> bool) -> term -> (term list * 'a) option

val rmap_table_resource_info:
   ('a term_map_table -> 'b) ->
   (term * term list * 'a, 'a term_map_table, 'b) resource_info

(* Always returns true *)
val select_all : 'a -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
