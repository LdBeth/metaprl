(*
 * Map module based on red-black trees
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *)

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type McMap =
sig
   type key
   type 'a t

   val empty : 'a t
   val is_empty : 'a t -> bool
   val cardinal : 'a t -> int
   val add : 'a t -> key -> 'a -> 'a t
   val find : 'a t -> key -> 'a
   val remove : 'a t -> key -> 'a t
   val mem : 'a t -> key -> bool
   val iter : (key -> 'a -> unit) -> 'a t -> unit
   val map : ('a -> 'b) -> 'a t -> 'b t
   val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
   val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val fold_map : ('a -> key -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
   val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
   val forall : (key -> 'a -> bool) -> 'a t -> bool
   val exists : (key -> 'a -> bool) -> 'a t -> bool
   val isect_mem : 'a t -> (key -> bool) -> 'a t
   val union : 'a t -> 'a t -> 'a t

   val filter_add : 'a t -> key -> ('a option -> 'a) -> 'a t
   val filter_remove : 'a t -> key -> ('a -> 'a option) -> 'a t
   val keys : 'a t -> key list
   val data : 'a t -> 'a list
end

(*
 * This is the backwards-compatible version.
 *)
module type S =
sig
   type key
   type 'a t

   val empty : 'a t
   val add : key -> 'a -> 'a t -> 'a t
   val find : key -> 'a t -> 'a
   val remove : key -> 'a t -> 'a t
   val mem : key -> 'a t -> bool
   val iter : (key -> 'a -> unit) -> 'a t -> unit
   val map : ('a -> 'b) -> 'a t -> 'b t
   val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
   val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type McMapList =
sig
   include McMap

   val filter : 'a t -> key -> ('a list -> 'a list) -> 'a t
   val find_all : 'a t -> key -> 'a list
   val iter_all : (key -> 'a list -> unit) -> 'a t -> unit
   val mapi_all : (key -> 'a list -> 'b list) -> 'a t -> 'b t
   val fold_all : ('a -> key -> 'b list -> 'a) -> 'a -> 'b t -> 'a
   val data_all : 'a t -> 'a list list
end

module Make       (Ord : OrderedType) : (S         with type key = Ord.t)
module McMake     (Ord : OrderedType) : (McMap     with type key = Ord.t)
module McMakeList (Ord : OrderedType) : (McMapList with type key = Ord.t)
