(*
 * Additional operations on lists.
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

(* Filter items out of a list *)
val filter : ('a -> bool) -> 'a list -> 'a list

(* Lexicographic comparison of two lists *)
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(* Elements must by physically equal *)
val compare_eq : 'a list -> 'a list -> bool

(* Elements must be equal, but lists may be different lengths *)
val compare_cmp : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(*
 * These functions are just liek the List functions
 * but they raise Failure, not Invalid_argument.
 *)
val nth : 'a list -> int -> 'a
val allp : ('a -> bool) -> 'a list -> bool
val existsp : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(* Remove items marked by a vector of bools *)
val remove_elements : bool list -> 'a list -> 'a list
val remove_suffix : 'a list -> 'a list -> 'a list

(* Interated forms *)
val nth_tl : int -> 'a list -> 'a list

(* Functional replacement *)
val replacef_nth : int -> ('a -> 'a) -> 'a list -> 'a list
val replacef_arg_nth : int -> ('a -> 'a * 'b) -> 'a list -> 'a list * 'b
val replace_nth : int -> 'a -> 'a list -> 'a list
val replaceq : 'a -> 'a -> 'a list -> 'a list
val replace_first : ('a -> bool) -> 'a -> 'a list -> 'a list
val replace_all : ('a -> bool) -> 'a -> 'a list -> 'a list

val remove_nth : int -> 'a list -> 'a list
val insert_nth : int -> 'a -> 'a list -> 'a list
val remove : 'a -> 'a list -> 'a list
(* tryremove does not raise any exception when element is not in the list *)
val tryremove : 'a -> 'a list -> 'a list
val removeq : 'a -> 'a list -> 'a list

(* Find the index of the element that satisfies a predicate *)
val find : ('a -> bool) -> 'a list -> 'a
val find_item : ('a -> bool) -> 'a list -> int
val find_index : 'a -> 'a list -> int
val find_indexq : 'a -> 'a list -> int

(* Set-like operations *)
val intersect : 'a list -> 'a list -> 'a list
val intersectq : 'a list -> 'a list -> 'a list
val intersects : 'a list -> 'a list -> bool
val subtract : 'a list -> 'a list -> 'a list
val subtractq : 'a list -> 'a list -> 'a list
val subtract_multiset : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val unionq : 'a list -> 'a list -> 'a list

(*
 * Reverse iteration
 *)
val rev_iter : ('a -> 'b) -> 'a list -> unit
val rev_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val flat_map : ('a -> 'b list) -> 'a list -> 'b list
val fail_map : ('a -> 'b) -> 'a list -> 'b list
val some_map : ('a -> 'b option) -> 'a list -> 'b list
val some_map_safe : ('a -> 'a option) -> 'a list -> 'a list
val fold_left : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

(*
 * Association lists.
 *)
(* zip_list a b c zips b and c and puts the result in front of a in reverce order *)
val zip_list : ('a * 'b) list -> 'a list -> 'b list -> ('a * 'b) list
val zip : 'a list -> 'b list -> ('a * 'b) list
val assoc_index : ('a * 'b) list -> 'a -> int
val assoc_replace : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val add_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
val assoc_in_dom : ('b -> 'a -> bool) -> 'b -> ('a * 'c) list -> bool
val assoc_in_range : ('b -> 'c -> bool) -> 'b -> ('a * 'c) list -> bool
(* assoc_append_replace_snd l1 b l2 replaces the second element of all pairs in l2 with b 
   and appends l1 at the end of the result *)
val assoc_append_replace_snd : ('a * 'b) list -> 'b -> ('a * 'c) list -> ('a * 'b) list

(*
 * if either of the assoc list sides has duplicate entires, only the first entry is used
 * and the duplicate entry forces all second component matches to return false
 *
 * i.e. check_assoc v1 v2 [1,2; 3,2; 3,4] = (v1<>3) && (v2<>4) && check_assoc v1 v2 [1,2]
 *
 * try_check_assoc is the same as check_assoc, but raises an exception if an entry is not found
 *)
val check_assoc : 'a -> 'a -> ('a *'a) list -> bool
val try_check_assoc : 'a -> 'b -> ('a * 'b) list -> bool

(* if left side has duplicate entires, only the first entry is used *)
val try_assoc: 'a -> ('a * 'a) list -> 'a

(*
 * List splitting.
 *)
val split_list : int -> 'a list -> 'a list * 'a list
val split_last : 'a list -> 'a list * 'a
val last : 'a list -> 'a
val fst_split : ('a * 'b) list -> 'a list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
