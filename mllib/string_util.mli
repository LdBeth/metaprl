(*
 * Extra operations on strings.
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

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

(*
 * Functions for cacthing errors.
 *)
val create : string -> int -> string
val make : string -> int -> char -> string
val sub : string -> string -> int -> int -> string
val blit : string -> string -> int -> string -> int -> int -> unit
val set : string -> string -> int -> char -> unit
val get : string -> string -> int -> char

(*
 * Find a char in a string.
 *)
val strchr : string -> char -> int

(*
 * Mapping.
 *)
val for_all : (char -> bool) -> string -> bool

(*
 * Membership.
 *)
val mem : char -> string -> bool

(*
 * Get the index of any char in the set.
 *)
val index_set : string -> string -> int
val rindex_set : string -> string -> int

(*
 * Split at a char.
 *)
val split : char -> string -> string list
val split_set : string -> string -> string list

(*
 * Quote strings that contains unprintable characters, spaces or qoutes.
 * unquote(quote (s)) = s 
 * When s does not have any special characters, quote(s) == s
 *)
val quote: string -> string
val unquote: string -> string

(*
 * Find a new name (usually - for a variable) avoiding those where the predicate is true
 *)
val vnewname : string -> (string -> bool) -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
