(* This file implements simple has htable
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

open Simplehash_sig

module Simplehashtbl : SimpleHashSig =
struct

type ('key, 'value) t =
   {
      compare: 'key -> 'key -> bool;

      mutable table: ('key * 'value) list array;
      mutable count: int;
   }

let create i comp =
   {
      compare = comp;
      table = Array.make i [];
      count = 0;
   }

(*
 * Rehash the table.
 *)
let rehash table =
(*
   if !debug_memo then
      eprintf "Memo.rehash: %d%t" (Array.length table) eflush;
*)
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
   let count = info.count and table = info.table in
   let len = Array.length table in
   let index = hash mod len in
      info.count <- count + 1;
      table.(index) <- (key, value) :: table.(index);
      if count > len * 2 then
         info.table <- rehash table

let extr tt = ( tt.table, tt.count )

let iter f info = Array.iter (List.iter f) info.table

let gc hashfun test info =
   let new_table = create 17 info.compare in
   let agent (key, value) =
         if test (key, value) then
            insert new_table (hashfun key) key value
         else
            ()
         in
      iter agent info;
      info.table <- new_table.table;
      info.count <- new_table.count

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "bi_memo, weak_memo"
 * End:
 * -*-
 *)
