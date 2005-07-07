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

 (*module type BigSig =
sig*)
(************************************************************************
 * Types                                                                *
 ************************************************************************)

type lint32

exception IntSize of string * int

val create : int -> lint32
val mk_bint : int -> lint32
val make_lint32 : int * int -> lint32
val dest_bint : lint32 -> int
val dest_lint32 : lint32 -> int * int

val int_of_lint32 : lint32 -> int
val lint32_of_int : int -> lint32

val print_lint32 : lint32 -> unit

val lband : lint32 -> lint32 -> lint32
val lbor : lint32 -> lint32 -> lint32
val lbsl : lint32 -> int -> lint32
(*
val lbsr : lint32 -> int -> lint32
val basr : lint32 -> int -> lint32
*)
val bplus : lint32 -> int -> lint32
val bminus : lint32 -> int -> lint32

val bequal : lint32 -> lint32 -> bool
val blt : lint32 -> lint32 -> bool
val bgt : lint32 -> lint32 -> bool
val blte : lint32 -> lint32 -> bool
val bgte : lint32 -> lint32 -> bool

val bdecr : lint32 ref -> unit

 (* end*)
