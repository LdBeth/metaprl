(*
 * This is an implementation of a list, where list appending
 * occurs only if one list is not a sublist of the other.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A single item in the tree.
 *)
type 'a item =
   Entry of 'a
 | Mark of 'a item list

(*
 * The tree is a list of items.
 *)
type 'a t = 'a item list

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty tree.
 *)
let nil = []

(*
 * Add the entry.
 *)
let cons h t =
   Entry h :: t

(*
 * Append two lists, unless on of the
 * lists is a sublist of the other.
 *)
let append list1 list2 =
   [Mark list1; Mark list2]

(*
 * Collect the entries into a single list.
 * Keep a list of marks that have been inserted so that each mark is only inserted
 * once.  Marks are just lists of items; compare them with physical equality.
 *)
let to_list items =
   let rec insert_item marks_entries = function
      [] ->
         marks_entries
    | h::t ->
         let marks, entries = marks_entries in
            match h with
               Entry info ->
                  insert_item (marks, info :: entries) t
             | Mark mark ->
                  if List.memq mark marks then
                     insert_item marks_entries t
                  else
                     let marks, entries = insert_item marks_entries mark in
                        insert_item (mark :: marks, entries) t
   in
      snd (insert_item ([], []) items)

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
