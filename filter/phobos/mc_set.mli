(*
 * Set module, implemented using red-black trees
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

module type OrderedType =
sig
   type t
   val compare : t -> t -> int
end

(*
 * Our version.
 *)
module type McSet =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : t -> elt -> t
   val singleton : elt -> t
   val remove : t -> elt -> t
   val union : t -> t -> t
   val inter : t -> t -> t
   val diff : t -> t -> t
   val compare : t -> t -> int
   val equal : t -> t -> bool
   val subset : t -> t -> bool
   val iter : (elt -> unit) -> t -> unit
   val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
   val for_all : (elt -> bool) -> t -> bool
   val exists : (elt -> bool) -> t -> bool
   val filter : (elt -> bool) -> t -> t
   val partition : (elt -> bool) -> t -> t * t
   val cardinal : t -> int
   val elements : t -> elt list
   val min_elt : t -> elt
   val max_elt : t -> elt
   val choose : t -> elt

   val add_list : t -> elt list -> t
   val subtract_list : t -> elt list -> t
   val of_list : elt list -> t
   val to_list : t -> elt list
end

(*
 * Backwards-compatible version.
 *)
module type S =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : elt -> t -> bool
   val add : elt -> t -> t
   val singleton : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val inter : t -> t -> t
   val diff : t -> t -> t
   val compare : t -> t -> int
   val equal : t -> t -> bool
   val subset : t -> t -> bool
   val iter : (elt -> unit) -> t -> unit
   val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
   val for_all : (elt -> bool) -> t -> bool
   val exists : (elt -> bool) -> t -> bool
   val filter : (elt -> bool) -> t -> t
   val partition : (elt -> bool) -> t -> t
   val cardinal : t -> int
   val elements : t -> elt list
   val min_elt : t -> elt
   val max_elt : t -> elt
   val choose : t -> elt
end

module McMake (Ord : OrderedType) : (McSet with type elt = Ord.t)
module Make (Ord : OrderedType) : (S with type elt = Ord.t)
