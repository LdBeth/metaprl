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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_printf

open Term_sig
open Refiner.Refiner.TermShape

open Mp_resource

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_stable%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A table is just a hashtbl.
 *)
type 'a term_stable = (shape, 'a) Hashtbl.t

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let new_stable () =
   Hashtbl.create 97

(*
 * Insert into the list.
 *)
let sinsert tbl (t, v) =
   Hashtbl.add tbl (shape_of_term t) v

(*
 * Lookup.
 *)
let slookup tbl t =
   Hashtbl.find tbl (shape_of_term t)

let slookup_all tbl t =
   Hashtbl.find_all tbl (shape_of_term t)

let stable_resource_info retrieve =
   Imperative {
      imp_create = new_stable;
      imp_add = sinsert;
      imp_retr = retrieve
   }

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
