(*
 * Define a resource to evaluate toplevel expressions.
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
extends Summary

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Tactic_type.Tactic

open Shell_sig

type top_table
type item = string * string * top_expr * top_type

(*
 * The resource maps strings to values.
 * Input: module name, local name, expr
 *)
resource (item, item list -> top_table) toploop

val mem : top_table -> string -> bool

(*
 * A resource for compiling expressions from OCaml input.
 *)
val tactic_of_ocaml_expr : top_table -> MLast.expr -> tactic
val evaluate_ocaml_expr : top_table -> MLast.expr -> top_expr * top_type
val evaluate_ocaml_str_item : top_table -> MLast.str_item -> top_expr * top_type
val str_typ : top_type -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
