(*
 * This is a simplified version of termTable.
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
 *)

open Printf
open Mp_debug

open Opname
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermShape

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_stable%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A table is just a list of items.
 *)
type 'a term_stable =
   Empty
 | Cons of shape * 'a * 'a term_stable
 | Join of 'a term_stable * 'a term_stable

(*
 * An extracted table is a hashtbl.
 *)
type 'a term_sextract = (shape, 'a) Hashtbl.t

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty table.
 *)
let new_stable () =
   Empty

(*
 * Insert into the list.
 *)
let sinsert tbl t v =
   Cons (shape_of_term t, v, tbl)

(*
 * Join the data from two bases.
 *)
let join_stables data1 data2 =
   Join (data1, data2)

(*
 * Compute the hashtable from the info.
 * We only have to check for multiple listings at joins.
 *)
let sextract info =
   let tbl = Hashtbl.create 97 in
   let rec collect tables = function
      Empty ->
         ()
    | Cons (key, v, next) ->
         Hashtbl.add tbl key v;
         collect tables next
    | Join (table1, table2) ->
         if List.memq table1 tables then
            if List.memq table2 tables then
               ()
            else
               collect (table2 :: tables) table2
         else if List.memq table2 tables then
            collect (table1 :: tables) table1
         else
            let tables = table1 :: table2 :: tables in
               collect tables table1;
               collect tables table2
   in
      collect [] info;
      tbl

(*
 * Lookup.
 *)
let slookup tbl t =
   Hashtbl.find tbl (shape_of_term t)

(*
 * Debugging
 *)
let print_key out shape data =
   fprintf out "\t%a;\n" print_shape shape

let print_keys out tbl =
   fprintf out "Term_stable contains shapes:\n";
   Hashtbl.iter (print_key out) tbl

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
