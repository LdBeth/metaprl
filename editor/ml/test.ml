(*
 * Display all the elements in a particular theory.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

include Itt_theory

open Printf
open Nl_debug

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Itt_logic
open Itt_struct
open Itt_prop_decide
open Tacticals
open Base_auto_tactic
open Base_dtactic
open Conversionals
open Var

interactive test1 'H :
   sequent [squash] { 'H >- "type"{'A} } -->
   sequent [squash] { 'H >- "type"{'B} } -->
   sequent [squash] { 'H >- "type"{'C} } -->
   sequent ['ext] { 'H >- (('A or 'B) => 'C) => (('A => 'C) & ('B => 'C)) }

interactive test2 'H : :
   sequent ['ext] { 'H >- (('A or 'B) => 'C) => (('A => 'C) & ('B => 'C)) }

let f () =
   raise Not_found

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
