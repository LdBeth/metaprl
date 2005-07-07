(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
 * Authors: Lori Lorigo, Richard Eaton
 *)
open Lm_debug
open Lm_debug

let _ =
   show_loading "Loading Utils%t"

type 'a oref = {mutable ocontents : 'a option}

let null_oref ()  = {ocontents = None}
let oref a = {ocontents = (Some  a)}
let oref_set a b = (a.ocontents <- Some b); b
let oref_nullify a = (a.ocontents <- None); ()

exception OrefNone

let oref_p = function
 { ocontents = None} -> false
 | { ocontents = Some _ } -> true

let oref_option a = a.ocontents

let oref_val = function
 { ocontents = None} -> raise OrefNone
 | { ocontents = Some a } -> a


let assoc_if f l =
    let rec aux l =
      match l with
       [] -> None
      | head :: tail -> if (f head) then (Some head) else (aux tail) in
    aux l


let remove_if f l =
   let item = null_oref () in
    let rec aux l =
      match l with
       [] -> []
      | head :: tail -> if (f head)
			   then ((oref_set item head); tail)
			   else head :: (aux tail) in
    ((oref_option item), (aux l))

let remove_from_end_if f l =
   let item = null_oref ()
   and foundp = ref false in
    let rec aux l =
      match l with
       [] -> []
      | head :: tail ->
	  let ntail = aux tail in
	    if !foundp then head :: ntail
	    else if (f head) then (foundp := true; (oref_set item head); ntail)
 	    else l
	in
    ((oref_option item), (aux l))

let remove_if' f l =
    let rec aux l =
      match l with
       [] -> []
      | head :: tail -> if (f head)
			   then tail
			   else head :: (aux tail) in
     (aux l)


let remove item l =
    let rec aux l =
      match l with
       [] -> []
      | head :: tail -> if (item =  head)
			   then tail
			   else head :: (aux tail) in
     (aux l)

let filter f l =
    let rec aux l =
      match l with
       [] -> []
      | head :: tail -> if (f head)
			   then aux tail
			   else head :: (aux tail) in
     (aux l)


let time_it f arg =
   let t1 = Unix.times () in
   let res = f arg in
   let t2 = Unix.times () in
      print_string "User time: ";
      print_float (t2.Unix.tms_utime -. t1.Unix.tms_utime);
      print_string "\nSystem time: ";
      print_float (t2.Unix.tms_stime -. t1.Unix.tms_stime);
      print_string "\n\n";
      flush stdout;
      res
