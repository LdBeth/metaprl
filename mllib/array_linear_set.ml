(*
 * This module provides a linearly ordered numbered set implementation
 * with lazy functions based on arrays
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
 * Author: Alexey Nogin
 *)
type 'a linear_set = 'a array

open Linear_set

module Make (Type : TypeSig) = 
struct
   type elt = Type.t
   type t = Type.t linear_set
   type index = int
   
   let empty = [||]
   let singleton x = [|x|]
   let length = Array.length
   let create = Array.create
   let make = Array.create
   let iter = Array.iter
   let of_list = Array.of_list
   let to_list = Array.to_list
   let lazy_apply = Array.map
   let append a1 e a2 = 
      let l1 = length a1 and l2 = length a2 in
      if l1 = 0 && l2 = 0 then [|e|] else begin
         let r = create (succ l1 + l2) e in
         for i = 0 to l1 - 1 do Array.unsafe_set r i (Array.unsafe_get a1 i) done;  
         let l1 = succ l1 in
         for i = 0 to l2 - 1 do Array.unsafe_set r (i + l1) (Array.unsafe_get a2 i) done;  
         r
      end

   let get = Array.get

   let split t ind =
      Array.sub t 0 ind, t.(ind), Array.sub t (succ ind) (Array.length t - ind - 1)

   let lazy_sub_map = Lm_array_util.sub_map
   let append_list = Lm_array_util.append_list_array
   let mapi = Array.mapi
   let init = Array.init
   let collect = Lm_array_util.collect

end
