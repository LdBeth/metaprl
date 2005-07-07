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

type bigint

exception IntSize of string * int

val create : int -> bigint
val mk_bint : int -> bigint
val make_bigint : int * int -> bigint
val dest_bint : bigint -> int
val dest_bigint : bigint -> int * int

val print_bigint : bigint -> unit

val lband : bigint -> bigint -> bigint
val lbor : bigint -> bigint -> bigint
val lbsl : bigint -> int -> bigint
(*
val lbsr : bigint -> int -> bigint
val basr : bigint -> int -> bigint
*)
val bplus : bigint -> int -> bigint
val bminus : bigint -> int -> bigint

val bequal : bigint -> bigint -> bool
val blt : bigint -> bigint -> bool
val bgt : bigint -> bigint -> bool
val blte : bigint -> bigint -> bool
val bgte : bigint -> bigint -> bool

val bdecr : bigint ref -> unit

 (* end*)
