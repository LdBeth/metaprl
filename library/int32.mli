(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

type int32

exception IntSize of string * int

val create : int -> int32
val mk_bint : int -> int32
val make_int32 : int * int -> int32
val dest_bint : int32 -> int
val dest_int32 : int32 -> int * int

val int_of_int32 : int32 -> int
val int32_of_int : int -> int32

val print_int32 : int32 -> unit

val lband : int32 -> int32 -> int32
val lbor : int32 -> int32 -> int32
val lbsl : int32 -> int -> int32
(*
val lbsr : int32 -> int -> int32
val basr : int32 -> int -> int32
*)
val bplus : int32 -> int -> int32
val bminus : int32 -> int -> int32


val bequal : int32 -> int32 -> bool
val blt : int32 -> int32 -> bool
val bgt : int32 -> int32 -> bool
val blte : int32 -> int32 -> bool
val bgte : int32 -> int32 -> bool

val bdecr : int32 ref -> unit

 (* end*)
