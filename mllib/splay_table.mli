(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
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
module type SplayTableSig =
sig
   type elt
   type set
   type t
   type data

   val create : set -> t
   val add : t -> elt -> data -> t
   val union : t -> t -> t
   val mem : t -> elt -> bool
   val find : t -> elt -> data
   val find_all : t -> elt -> data list
   val remove : t -> elt -> t
   val iter : (elt -> data -> unit) -> t -> unit
   val map : (elt -> data -> data) -> t -> t
end

(*
 * Ordering module takes a comparison set.
 *)
module type TableBaseSig =
sig
   type elt
   type set
   type data

   val union : set -> set -> set
   val compare : set -> elt -> elt -> int
   val append : data list -> data list -> data list
end

(*
 * Build the table over an ordered type.
 *)
module MakeSplayTable (Base : TableBaseSig)
: SplayTableSig
  with type elt = Base.elt
  with type set = Base.set
  with type data = Base.data

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
