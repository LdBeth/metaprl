(* This file implements hash table with GC-feature
 *
 * -----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

open Hash_with_gc_sig

module HashWithGC : HashWithGCSig =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type ('key, 'value) hash = int

   type ('key, 'value) t =
      { hash_func : 'key -> int;
        compare : 'key -> 'key -> bool;

        mutable table : ('key * 'value) list array;
        mutable count : int;

        mutable gc_on : bool;
        mutable gc_bucket : int;
        mutable gc_count : int;

        gc_critical_level : int
      }

   exception GC_Not_Finished
   exception GC_Not_Started
   exception Expand_During_GC

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   let create i crit_lev hf comp =
      if crit_lev <= 0 then
         invalid_arg "Hash_with_gc.create: level argument should be positive";

      { hash_func = hf;
        compare = comp;
        table = Array.make i [];
        count = 0;

        gc_on = false;
        gc_bucket = 0;
        gc_count = 0;
        gc_critical_level = crit_lev;
      }

   let hash info = info.hash_func

   (*
    * Rehash the table.
    *)
   let rehash info =
      let len = (Array.length info.table * 2) + 1 in
      let table = Array.make len [] in
      let insert bucket =
         let insert ((x, _) as entry) =
            let index = (info.hash_func x) mod len in
               table.(index) <- entry :: table.(index)
         in
            List.iter insert bucket
      in
         Array.iter insert info.table;
         table

   (*
    * Associative search in list.
    *)
   let rec assoc compare x = function
      (x', y) :: tl ->
         if compare x' x then
            Some y
         else
            assoc compare x tl
    | [] ->
         None

   let seek info hash key =
      let len = Array.length info.table in
      let index = hash mod len in
      let bucket = info.table.(index) in
         assoc info.compare key bucket

   let insert info hash key value =
      let count = info.count in
      let table = info.table in
      let len   = Array.length table in
      let index = hash mod len in
         info.count <- count + 1;
         table.(index) <- ((key, value) :: table.(index));
         if count > len * 2 then
            if info.gc_on then
               raise Expand_During_GC
            else
               info.table <- rehash info

   let iter f info = Array.iter (List.iter f) info.table

   let gc_start info =
      if info.gc_on then
         raise GC_Not_Finished
      else
         begin
            info.gc_on <- true;
            info.gc_bucket <- 0;
            info.gc_count <- 0
         end

   type ('a, 'b) option2 =
      Some2 of 'a * 'b
    | None2

   let rec scan_list_for_release test = function
      hd::tl ->
         if test hd then
            Some2 (hd, tl)
         else
            begin match scan_list_for_release test tl with
                     Some2 (item, rest) -> Some2 (item, hd::rest)
                   | None2 -> None2
            end
    | [] -> None2

   let rec scan_for_release test info =
      let i = info.gc_bucket in
         match scan_list_for_release test info.table.(i) with
            Some2(item,new_bucket) ->
               begin
                  info.table.(i) <- new_bucket;
                  info.count <- (pred info.count);
                  info.gc_count <- succ info.gc_count;
                  Some item
               end
          | None2 ->
               let i' = succ i in
                  if i' = Array.length info.table then
                     if 100 * info.gc_count < info.gc_critical_level * info.count then
                        begin
                           info.gc_on <- false;
                           None
                        end
                     else
                        begin
                           info.gc_count <- 0;
                           info.gc_bucket <- 0;
                           scan_for_release test info
                        end
                  else
                     begin
                        info.gc_bucket <- i';
                        scan_for_release test info
                     end

   let gc_iter test info =
      if not info.gc_on then
         raise GC_Not_Started
      else
         scan_for_release test info

   let is_gc info = info.gc_on
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
