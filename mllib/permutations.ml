(*
 * Permutations.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)

(* permutation = element of the S_n symmetric group *)
type permutation = int array

(* identity element *)
let id_perm n =
   let result = Array.make n 0 in
   for i=0 to pred n do
      Array.unsafe_set result i i
   done;
   result

(* Permutes a list. Raises an Invalid_argument if the list has the wrong length *)
let permute_list p = function
   [] -> if (p<>[||]) then raise (Invalid_argument "Permutations.permute_list") else []
 | hd::tl ->
      let l = Array.length p in
      if succ (List.length tl) = l then
         let res = Array.make l hd in
         let rec aux i = function
            [] -> ()
          | hd::tl ->
               res.(p.(i)) <- hd;
               aux (succ i) tl
         in aux 1 tl;
         Array.to_list res
      else raise (Invalid_argument "Permutations.permute_list")

(* if l1 contains all elements of l2 and, possibly, some other elements, than
 * (permute (get_permut l1 l2) l2) is a list whose elements appear in the same order as in l1
 * If l1 does not contain all of l2's elements or if l2 has repeated elements, get_permut would
 * raise a Failure "Permutations.get_permut"
 * If l1 has dumplicated elements, the first occurence of the element in l1 is used  *)
let rec get_permut_set (res: int array) i j elem = function
   [] ->
      i
 | hd :: _ when (hd = elem) ->
      if res.(j)<0 then begin
         res.(j) <- i;
         (succ i)
      end else i
 | _ :: tl ->
      get_permut_set res i (succ j) elem tl

let rec get_permut_aux res l2 i = function
   [] -> ()
 | hd::tl ->
       let i'=get_permut_set res i 0 hd l2 in
       get_permut_aux res l2 i' tl

let get_permut l1 l2 =
   let l = List.length l2 in
   let res = Array.make l (-1) in
   get_permut_aux res l2 0 l1;
   for i=0 to pred l do
      if res.(i) < 0 then raise (Failure "Permutations.get_permut")
   done;
   res

