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

(*conversion between nuprl-light terms and mathbus terms*)

open Refiner.Refiner.TermType
open MathBus

val use_table : bool ref
val term_of_mbterm: mbterm -> term
val mbterm_of_term: term -> mbterm

val print_param: param -> unit
val print_term: term -> unit

(*
val param_of_opname: opname ->  param
val opname_of_param: param -> opname
val op_of_params: param list -> operator

val mbparameter_of_param: param -> mbterm
val mbbinding_of_binding: string -> mbterm

val param_of_mbparameter: mbterm -> param
val bvars_of_mbbindings: mbterm -> string list
*)

val write_node_to_file: mbterm -> string -> unit
val read_node_from_file: string -> mbterm

val assign_mbs_terms: unit -> unit
