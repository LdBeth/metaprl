(*
 * Generic memoize function.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Lm_debug
open Lm_printf

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

module Memo =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Memo table.
    * The function is the function we are caching.
    * The table is the cached values of the function.
    * The count is used for rehashing.
    *
    * The nth key corresponds to the nth value.
    *)
   type ('param, 'arg, 'header, 'weak_header, 'result) t =
      { memo_f : 'param -> 'arg -> 'header;
        memo_g : 'param -> 'header -> 'result;
        memo_convert : 'param -> 'header -> 'weak_header;
        memo_compare : 'weak_header -> 'weak_header -> bool;

        mutable memo_f_table : ('arg * 'result) list array;
        mutable memo_f_count : int;

        mutable memo_g_table : ('header * 'result) list array;
        mutable memo_g_count : int
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Empty tables.
    *)
   let create size _ _ f convert compare g =
      { memo_f = f;
        memo_g = g;
        memo_convert = convert;
        memo_compare = compare;

        memo_f_table = Array.make size [];
        memo_f_count = 0;

        memo_g_table = Array.make size [];
        memo_g_count = 0
      }

   (*
    * Rehash the table.
    *)
   let rehash table =
      if !debug_memo then
         eprintf "Memo.rehash: %d%t" (Array.length table) eflush;
      let entries = ref [] in
      let len = Array.length table in
      let _ =
         for i = 0 to len - 1 do
            entries := table.(i) :: !entries
         done
      in
      let len = (len * 2) + 1 in
      let table = Array.make len [] in
      let insert bucket =
         let insert ((x, _) as entry) =
            let index = Hashtbl.hash x mod len in
               table.(index) <- entry :: table.(index)
         in
            List.iter insert bucket
      in
         List.iter insert !entries;
         table

   (*
    * Asssocive search by pointer equality.
    *)
   let rec assocq x = function
      (x', b) :: tl ->
         if x' == x then
            Some b
         else
            assocq x tl
    | [] ->
         None

   (*
    * Associative search.
    *)
   let rec assoc compare x = function
      (x', b) :: tl ->
         if compare x' x then
            Some b
         else
            assoc compare x tl
    | [] ->
         None

   (*
    * Insert a f-value into its table.
    * Rehash if necessary.
    *)
   let insert_f info hash x b =
      let { memo_f_count = count; memo_f_table = table; _ } = info in
      let len = Array.length table in
      let index = hash mod len in
         info.memo_f_count <- count + 1;
         table.(index) <- (x, b) :: table.(index);
         if count > len * 2 then
            info.memo_f_table <- rehash table

   (*
    * Insert a g-value into its table.
    * Rehash if necessary.
    *)
   let insert_g info hash x b =
      let { memo_g_count = count; memo_g_table = table; _ } = info in
      let len = Array.length table in
      let index = hash mod len in
         info.memo_g_count <- count + 1;
         table.(index) <- (x, b) :: table.(index);
         if count > len * 2 then
            info.memo_g_table <- rehash table

   (*
    * Lookup.
    *)
   let apply info arg x =
      let table = info.memo_f_table in
      let len = Array.length table in
      let hash_x = Hashtbl.hash x in
      let index = hash_x mod len in
      let bucket = table.(index) in
         match assocq x bucket with
            Some b ->
               (*
                * We have seen this exact application before.
                *)
               if !debug_memo then
                  eprintf "F: success%t" eflush;
               b
          | None ->
               let y = info.memo_f arg x in
               let table = info.memo_g_table in
               let len = Array.length table in
               let hash_y = Hashtbl.hash y in
               let index = hash_y mod len in
               let bucket = table.(index) in
               let convert = info.memo_convert arg in
               let compare a b =
                  info.memo_compare (convert a) (convert b)
               in
                  match assoc compare y bucket with
                     Some b ->
                        (*
                         * This happens when g finds an approximate match,
                         * but the exact pointer equality has not been seen.
                         * Remember this in f_table.
                         *)
                        if !debug_memo then
                           eprintf "G: success%t" eflush;
                        insert_f info hash_x x b;
                        b
                   | None ->
                        (*
                         * We have never seen the value before.
                         * Remember it in both f_table and g_table.
                         *)
                        let b = info.memo_g arg y in
                           if !debug_memo then
                              eprintf "G: failed%t" eflush;
                           insert_f info hash_x x b;
                           insert_g info hash_y y b;
                           b
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
