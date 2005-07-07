(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
 * Authors: Lori Lorigo, Richard Eaton
 *)
open Lm_symbol

open Opname
open Refiner.Refiner.TermType
open Lm_num

val mk_nuprl5_op	: param list -> operator
val nuprl5_opname	: opname
val nuprl5_opname_p		: opname -> bool

(* parameter mapping *)

val make_bool_parameter	: bool -> param
val make_time_parameter	: num -> param

val time_parameter_p	: param -> bool
val bool_parameter_p	: param -> bool

val destruct_time_parameter	: param -> num
val destruct_bool_parameter	: param -> bool


(* itt logic functions *)

val nuprl_is_all_term : term -> bool
val nuprl_dest_all : term -> var * term * term

val nuprl_is_exists_term : term -> bool
val nuprl_dest_exists : term -> var * term * term

val nuprl_is_or_term : term -> bool
val nuprl_dest_or : term -> term * term

val nuprl_is_and_term : term -> bool
val nuprl_dest_and : term -> term * term

val nuprl_is_implies_term : term -> bool
val nuprl_dest_implies : term -> term * term

val nuprl_is_not_term : term -> bool
val nuprl_dest_not : term -> term

val nuprl_is_var_term : term -> bool
val nuprl_dest_var : term -> var
