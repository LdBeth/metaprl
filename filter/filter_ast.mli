(*
 * Grammar utilities.
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

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

val build_ml_term : MLast.loc -> term -> MLast.expr
val build_ml_mterm : MLast.loc -> meta_term -> MLast.expr
val list_expr : MLast.loc -> ('a -> MLast.expr) -> 'a list -> MLast.expr
val apply_patt : MLast.loc -> ('a -> MLast.patt) -> 'a list -> MLast.patt
val list_patt : MLast.loc -> ('a -> MLast.patt) -> 'a list -> MLast.patt
val fun_expr : MLast.loc -> string list -> MLast.expr -> MLast.expr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
