(*
 * Overlay list of elements as small sets over another
 * set implementation.
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

module Make (BigSet : Nl_set.S) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Max size of small sets.
    * This number is arbitrary.
    *)
   let max_size = 12

   (*
    * Type of elements in the set.
    *)
   type elt = BigSet.elt

   (*
    * Either a list or a set.
    *)
   type t =
      List of elt list
    | Set of BigSet.t

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Conversion.
    *)
   let rec set_of_list_aux set = function
      h :: t ->
         set_of_list_aux (BigSet.add h set) t
    | [] ->
         set

   let set_of_list l =
      Set (set_of_list_aux BigSet.empty l)

   (*
    * Set operations.
    *)
   let empty = List []

   let is_empty = function
         List [] ->
            true
       | List (_ :: _) ->
            false
       | Set s ->
            BigSet.is_empty s

   let mem set x =
      match set with
         List l ->
            List.mem x l
       | Set s ->
            BigSet.mem s x

   let add x set =
      match set with
         List l ->
            if List.length l = max_size then
               set_of_list (x :: l)
            else
               List (x :: l)
       | Set s ->
            Set (BigSet.add x s)

   let make x =
      List [x]

   let rec remove_list x = function
      h :: t ->
         if x = h then
            remove_list x t
         else
            h :: remove_list x t
    | [] ->
         []

   let remove x = function
      List l ->
         List (remove_list x l)
    | Set s ->
         Set (BigSet.remove x s)

   let rec union_set_list set = function
      x :: t ->
         union_set_list (BigSet.add x set) t
    | [] ->
         Set set

   let union set1 set2 =
      match set1, set2 with
         List l1, List l2 ->
            let l = List_util.union l1 l2 in
               if List.length l <= max_size then
                  List l
               else
                  set_of_list l
       | Set s1, Set s2 ->
            Set (BigSet.union s1 s2)
       | List l1, Set s2 ->
            union_set_list s2 l1
       | Set s1, List l2 ->
            union_set_list s1 l2

   let elements = function
      List l ->
         l
    | Set s ->
         BigSet.elements s

   let of_list l =
      if List.length l <= max_size then
         List l
      else
         Set (BigSet.of_list l)

   let iter f = function
      List l ->
         List.iter f l
    | Set s ->
         BigSet.iter f s

   let cardinal = function
      List l ->
         List.length l
    | Set s ->
         BigSet.cardinal s

   (*
    * Filter out the elements that are in the intersection.
    *)
   let mem_filt set elements =
      match set with
         List l ->
            List_util.intersect l elements
       | Set s ->
            BigSet.mem_filt s elements

   let not_mem_filt set elements =
      match set with
         List l ->
            List_util.subtract elements l
       | Set s ->
            BigSet.not_mem_filt s elements

   let rec collect l = function
      (x, v) :: tl ->
         if List.mem x l then
            (x, v) :: collect l tl
         else
            collect l tl
    | [] ->
         []

   let fst_mem_filt set elements =
      match set with
         List l ->
            collect l elements
       | Set s ->
            BigSet.fst_mem_filt s elements


   let rec set_list_intersectp set = function
      h :: t ->
         (BigSet.mem set h) || (set_list_intersectp set t)
    | [] ->
         false

   let intersectp set1 set2 =
      match set1, set2 with
         List l1, List l2 ->
            List_util.intersects l1 l2
       | Set s1, Set s2 ->
            BigSet.intersectp s1 s2
       | List l1, Set s2 ->
            set_list_intersectp s2 l1
       | Set s1, List l2 ->
            set_list_intersectp s1 l2
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
