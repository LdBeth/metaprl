(*
 * This file is part of Nuprl-Light, a modular, higher order
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


  (* reference with option *)
type 'a oref

exception OrefNone

val null_oref	: unit -> 'a oref
val oref	: 'a -> 'a oref
val oref_set	: 'a oref -> 'a -> 'a
val oref_nullify	: 'a oref -> unit

val oref_p	: 'a oref -> bool
val oref_val	: 'a oref -> 'a		(* fails if None *)
val oref_option	: 'a oref -> 'a option


open List

(* some useful list hacking funcs *)

val assoc_if	: ('a -> bool) -> 'a list -> 'a option

(* removes first occurence *)
val remove_if	: ('a -> bool) -> 'a list
			 -> ('a option (* value removed, if any *)
			      * 'a list)
val remove_from_end_if	: ('a -> bool) -> 'a list
			 -> ('a option (* value removed, if any *)
			      * 'a list)
val remove_if'	: ('a -> bool) -> 'a list -> 'a list
val remove	: 'a -> 'a list -> 'a list

(* removes all occurences *)
val filter	: ('a -> bool) -> 'a list -> 'a list

(* print user & system time used by a function, Unix only *)
val time_it: ('a -> 'b) -> 'a -> 'b
