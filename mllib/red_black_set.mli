(*
 * Implementation of sets based on red-black trees.
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

   val print : t -> unit
   val compare : t -> t -> int
end

module type S =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : elt -> t -> t
   val make : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val elements : t -> elt list
   val iter : (elt -> unit) -> t -> unit
   val cardinal : t -> int
   val mem_filt : t -> elt list -> elt list
   val fst_mem_filt : t -> (elt * 'a) list -> (elt * 'a) list
   val not_mem_filt : t -> elt list -> elt list
   val intersectp : t -> t -> bool
   val of_list : elt list -> t

   val print : t -> unit
end

module MakeDebug (Ord : OrderedType)
: S with type elt = Ord.t

module Make (Ord : Set.OrderedType)
: Mp_set.S with type elt = Ord.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
