(*
 * Permutations.
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)

(* permutation = element of the S_n symmetric group *)
type permutation

(* identity element *)
val id_perm : int -> permutation

(* Permutes a list. Raises an Invalid_argument if the list has the wrong length *)
val permute_list : permutation -> 'a list -> 'a list

(* if l1 contains all elements of l2 and, possibly, some other elements, than 
 * (permute (get_permut l1 l2) l2) is a list whose elements appear in the same order as in l1 
 * If l1 does not contain all of l2's elements or if l2 has repeated elements, get_permut would 
 * raise a Failure "Permutations.get_permut"
 * If l1 has dumplicated elements, the first occurence of the element in l1 is used  *)
val get_permut : 'a list -> 'a list -> permutation
