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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
extends Summary

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr

open Tactic_type.Tacticals
open Tactic_type.Conversionals

(*
 * These are the values that we recognize.
 *)
type expr =
   (* Base types *)
   UnitExpr of unit
 | BoolExpr of bool
 | IntExpr of int
 | StringExpr of string
 | TermExpr of term
 | TacticExpr of tactic
 | ConvExpr of conv
 | AddressExpr of address

   (* Uptyped tuples and functions *)
 | ListExpr of expr list
 | TupleExpr of expr list
 | FunExpr of (expr -> expr)

   (* Common cases are typed *)
 | UnitFunExpr of (unit -> expr)
 | BoolFunExpr of (bool -> expr)
 | IntFunExpr of (int -> expr)
 | StringFunExpr of (string -> expr)
 | TermFunExpr of (term -> expr)
 | TacticFunExpr of (tactic -> expr)
 | IntTacticFunExpr of ((int -> tactic) -> expr)
 | ConvFunExpr of (conv -> expr)
 | AddressFunExpr of (address -> expr)

   (* These functions take lists *)
 | AddrFunExpr of (int list -> expr)
 | StringListFunExpr of (string list -> expr)
 | TermListFunExpr of (term list -> expr)
 | TacticListFunExpr of (tactic list -> expr)
 | ConvListFunExpr of (conv list -> expr)

type top_table

(*
 * The resource maps strings to values.
 * Input: module name, local name, expr
 *)
resource (string * string * expr, top_table) toploop

val add_commands : top_table -> (string * expr) list -> unit
val mem : top_table -> string -> bool

(*
 * A resource for compiling expressions from OCaml input.
 *)
val expr_of_ocaml_expr : top_table -> MLast.expr -> expr
val expr_of_ocaml_str_item : top_table -> MLast.str_item -> expr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
