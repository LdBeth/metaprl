(*
 * Nonempty list with quick access to first and last,
 * and append operations.
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

type 'a t

type 'a tree =
   Empty
 | Leaf of 'a
 | Append of 'a tree * 'a tree

val create : 'a -> 'a t
val first : 'a t -> 'a
val last : 'a t -> 'a
val singleton : 'a t -> bool
val append : 'a t -> 'a t -> 'a t

(*
 * This operation appends the two lists,
 * without the last elemnt from the first list,
 * without the first element from the second list,
 * and the second argument in their place.
 *)
val append_skip : 'a t -> 'a -> 'a t -> 'a t

(*
 * Get the tree.
 * This is O(1).
 *)
val tree_of_list : 'a t -> 'a tree

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
