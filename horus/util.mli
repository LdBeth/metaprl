(*
 * Basic utility functions.
 * There is no particular order.
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

(************************************************************************
 * LISTS                                                                *
 ************************************************************************)

(* Filter items out of a list *)
val filter : ('a -> bool) -> 'a list -> 'a list;;

(* Lexicographic comparison of two lists *)
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int;;

(* Remove items marked by a vector of bools *)
val remove_elements : 'a list -> bool list -> 'a list;;
val remove_suffix : 'a list -> 'a list -> 'a list;;

(* Interated forms *)
val nth_tl : 'a list -> int -> 'a list;;

(* Functional replacement *)
val replacef_nth : 'a list -> int -> ('a -> 'a) -> 'a list;;
val replace_nth : 'a list -> int -> 'a -> 'a list;;
val remove_nth : 'a list -> int -> 'a list;;
val insert_nth : 'a list -> int -> 'a -> 'a list;;
val removeq : 'a list -> 'a -> 'a list;;

(* find the index of the element that satisfies a predicate *)
val find : 'a list -> ('a -> bool) -> 'a;;
val find_item : 'a list -> ('a -> bool) -> int;;
val find_index : 'a list -> 'a -> int;;
val find_indexq : 'a list -> 'a -> int;;

(* subtract an elenet from a list *)
val subtract : 'a list -> 'a list -> 'a list;;
val subtractq : 'a list -> 'a list -> 'a list;;
val union : 'a list -> 'a list -> 'a list;;
val unionq : 'a list -> 'a list -> 'a list;;

(*
 * Association lists.
 *)
val zip_list : ('a * 'b) list -> 'a list -> 'b list -> ('a * 'b) list;;
val zip : 'a list -> 'b list -> ('a * 'b) list;;
val assoc_index : ('a * 'b) list -> 'a -> int;;
val assoc_replace : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list;;
val add_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list;;

(*
 * List splitting.
 *)
val split_list : int -> 'a list -> 'a list * 'a list;;
val split_last : 'a list -> 'a list * 'a;;
val last : 'a list -> 'a;;

(************************************************************************
 * REFS                                                                 *
 ************************************************************************)

(* Stack ops *)
val push : 'a -> ('a list) ref -> unit;;
val pop : ('a list) ref -> 'a;;

(************************************************************************
 * ARRAYS                                                               *
 ************************************************************************)

(* Membership in an array *)
val array_mem : 'a -> 'a array -> bool;;
val array_index : 'a -> 'a array -> int;;
val array_exists : ('a -> bool) -> 'a array -> bool;;
val array_find_index : 'a array -> ('a -> bool) -> int;;

(************************************************************************
 * CHARACTERS                                                           *
 ************************************************************************)

(* Case conversion *)
val toupper : char -> char;;
val tolower : char -> char;;

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

(*
 * find a char in a string.
 *)
val strchr : string -> char -> int;;

(* Case conversion *)
val uppercase : string -> string;;
val lowercase : string -> string;;

(************************************************************************
 * FILES                                                                *
 ************************************************************************)

(*
 * Open a file somewhere in the path if possible.
 *)
val open_in_path : string list -> string -> in_channel * string;;

(*
 * Safe file handling.
 * the files are closed on exception.
 *)
val with_input_file : string -> (in_channel -> 'a) -> 'a;;
val with_output_file : string -> (out_channel -> 'a) -> 'a;;

(************************************************************************
 * GENERIC EXCEPTIONS                                                   *
 ************************************************************************)

(* Can't open and can't find a file *)
exception CantOpen of string;;
exception CantFind of string;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

