(*
 * Serialized map. Acts as a normal map, but the order in
 * which elements are inserted is retained, and all iterating
 * functions visit elements in that order.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * granicz@cs.caltech.edu
 *)

open Mc_map

(*
 * These are the functions provided by the table.
 *)
module type SerialMap =
sig
   type key
   type 'a tt

   val empty : 'a tt
   val is_empty : 'a tt -> bool
   val cardinal : 'a tt -> int
   val add : 'a tt -> key -> 'a -> 'a tt
   val find : 'a tt -> key -> 'a
   val remove : 'a tt -> key -> 'a tt
   val mem : 'a tt -> key -> bool

   val iter : (key -> 'a -> unit) -> 'a tt -> unit
   val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b tt -> 'a

   (* Both keys and data return a list ordered by insertion order *)
   val keys : 'a tt -> key list
   val data : 'a tt -> 'a list
end

(*
 * Make the map.
 *)
module SerialMapMake (Base : OrderedType) : SerialMap with type key = Base.t =
struct
   module IntModule = struct
      type t = int
      let compare = Pervasives.compare
   end

   module SMap2 = McMake (IntModule)
   module SMapShadow = McMake (Base)

   type key = Base.t
   type 'elt tt = (key SMap2.t * ('elt * int) SMapShadow.t)

   let counter = ref 0

   let empty = (SMap2.empty, SMapShadow.empty)

   let is_empty (table, shadow) = SMap2.is_empty table

   let cardinal (table, shadow) = SMap2.cardinal table

   let add (table, shadow) key data =
      let index =
         try
            let data, index = SMapShadow.find shadow key in
            (* Key is already in map *)
               index
         with
            Not_found ->
               incr counter;
               !counter
      in
         SMap2.add table index key, SMapShadow.add shadow key (data, index)

   let find (table, shadow) key =
      fst (SMapShadow.find shadow key)

   let remove (table, shadow) key =
      let data, index = SMapShadow.find shadow key in
         SMap2.remove table index, SMapShadow.remove shadow key

   let mem (table, shadow) key =
      SMapShadow.mem shadow key

   let iter f (table, shadow) =
      SMap2.iter (fun index key ->
         let data, index = SMapShadow.find shadow key in
            f key data) table

   let fold f accum (table, shadow) =
      SMap2.fold (fun accum index key ->
         let data, index = SMapShadow.find shadow key in
            f accum key data) accum table

   let keys (table, shadow) =
      let keys_rev =
         SMap2.fold (fun keys index key ->
            key :: keys) [] table
      in
         List.rev keys_rev

   let data (table, shadow) =
      let data_list_rev =
         SMap2.fold (fun data_list index key ->
            let data, _ = SMapShadow.find shadow key in
               data :: data_list) [] table
      in
         List.rev data_list_rev

end

