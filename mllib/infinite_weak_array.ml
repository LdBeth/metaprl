(* This file implements infinite array of weak pointers to objects.
 *
 * -----------------------------------------------------------------
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 *)

open Array

module InfiniteWeakArray =
struct

type 'a weak_descriptor = int

type 'a descriptor = { descriptor: 'a weak_descriptor;
					   anchor: 'a 
					 }

let describe_weak wd = wd
let subscribe_weak i = i

let describe d = d.descriptor
let weaking d = d.descriptor

type 'a t =
   {
     external_gc : 'a t -> int * 'a weak_descriptor list;

     mutable count : int;
     mutable array: 'a Weak.t;
     mutable holes : 'a weak_descriptor list;
   }

(*
exception EmptySlot
*)
exception Cell_is_full
exception Inconsistency

let is_debug = ref false
let dp_string s = if !is_debug then print_string s
let dp_int i = if !is_debug then print_int i


let create size extern_gc = 
   if size <=0 then invalid_arg "size must greater than 0"
   else
   { count = 0;
     array = Weak.create size;
     holes=[];
     external_gc=extern_gc
   }

let set ar wd item = 
(* check for empty - debug mode *)
   match Weak.get ar.array wd with
      Some e -> 
         dp_string "InfiniteWeakArray: Cell "; dp_int wd; dp_string " is full\n";
         raise Cell_is_full
    | None ->
	     Weak.set ar.array wd (Some item);
         { descriptor = wd; anchor = item }

let gc ar =
   dp_string "InfiniteWeakArray: GC\n";
   let (list_length, new_holes) = ar.external_gc ar in
   dp_string "InfiniteWeakArray: GC: "; dp_int list_length; dp_string " holes found\n";
   let critical_level = 20 in (* percent of free cells when better to enlarge the array *)
   let old_ar_length = Weak.length ar.array in
   if 100 * list_length < critical_level * old_ar_length then
      let new_ar = Weak.create (2*old_ar_length) in
   	     Weak.blit ar.array 0 new_ar 0 old_ar_length;
         ar.array <- new_ar;
         ar.holes <- new_holes;
         dp_string "InfiniteWeakArray: GC: array size doubled, empty position is "; dp_int ar.count; dp_string "\n";
         (ar.count, true)
   else
      match new_holes with
         wd::tl ->
	        ar.holes <- tl;
	        (wd, false)
	   | [] ->
	        raise Inconsistency
      
let holes_usage = ref 0

let store ar item =
   let {array=array; count=index; holes=holes} = ar in
   if index=Weak.length array then
     match holes with
     	wd::tl ->
     	   begin
     	      ar.holes <- tl;
     	      incr holes_usage;
     		  set ar wd item
     	   end
      | [] ->
           let (wd, increment) = gc ar in
              dp_string "InfiniteWeakArray: store: position "; dp_int wd; dp_string " is used after GC\n";
              if increment then
	              ar.count<-index+1
	          else incr holes_usage;
              set ar wd item
   else
      begin
         ar.count<-index+1;
(*         dp_string "InfiniteWeakArray: store: position "; dp_int index; dp_string " is used normally\n";
*)
         set ar index item
      end

let weak_get ar wd = Weak.get ar.array wd

let guard_get ar wd = 
   match Weak.get ar wd with
      Some item -> item
    | None -> raise Not_found

let get ar d = guard_get ar.array d.descriptor

let subscribe ar i = { descriptor = i; anchor = guard_get ar.array i }

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "weak_memo, term_header"
 * End:
 * -*-
 *)
