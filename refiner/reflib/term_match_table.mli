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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.Rewrite
open Mp_resource

(*
 * Table type.
 *)
type 'a term_table

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
 *
 * The function argument is a function that takes the list
 * of entries for a term and compacts them.  It is perfectly fine
 * for this function to be the identity.
 *
 * Earlier items in the list (term * 'a) list will be preffered in lookups
 *)
val create_table :
   (term * 'a) list ->
   ('a info_entry list -> 'b info_entry list) ->
   'b term_table

val print_term_match : term list -> unit
val eval_term_match : term -> unit

(*
 * Create a resource_info that can be used to create a resource
 * As an input this function takes a compactor (same as the second arg to
 * create_table) and the extractor function that uses table to do the actual
 * resource work.
 *)
val table_resource_info :
   ('a info_entry list -> 'b info_entry list) ->
   ('b term_table -> 'c) ->
   (term * 'a, (term * 'a) list, 'c) resource_info

val lookup : 'a term_table -> term -> rewrite_item list * 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
