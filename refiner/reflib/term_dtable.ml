(*
 * Make a hashtable for terms based on patterns.
 * Essentially, we want to be able to construct tables of pairs
 *    (pattern, 'a)
 * where pattern is a pattern that matches terms.  The lookup
 * function:
 *    lookup : table -> term -> 'a
 * should retrieve the value with the most specific pattern match.
 *
 * This implementation is as a two level table.  The first level
 * is a hash table the is used to look up a value based on the
 * outermost structure of the term.  The next lookup uses the
 * rewriter for full pattern matching.
 *
 * The interface is functional, so to make this a little
 * less expensive, we compile a regular table on the first access,
 * and just keep around a list of entries normally.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Term_sig
open Refiner.Refiner.TermShape
open Refiner.Refiner.Rewrite

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_dtable%t"

let debug_rewrite = load_debug "rewrite"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Function on pairs of terms.
 *)
type 'a pair_fun = (term * term * 'a) list -> term * term -> 'a

(*
 * pre-entries just contain the pattern/value pair.
 * For a LREntry or RLEntry the rule rewrites to the opposite side,
 * and no subgolas are generated.  For A DEntry, the rewrite writes to the
 * subgoals.
 *)
type 'a info_entry =
   { info_pattern : shape list;
     info_rw : rewrite_rule;
     info_value : 'a pair_fun
   }

(*
 * Pre-entries are classified:
 * A LREntry can be used in left-to-right order.
 * A RLEntry can be used in right-to-left order.
 * A DEntry can only be used as a whole.
 *)
type 'a table_entry =
   LREntry of 'a info_entry
 | RLEntry of 'a info_entry
 | DEntry of 'a info_entry

(*
 * A table has a list of pairs, plus an position for a compute
 * table.
 *)
type 'a term_dtable = 'a table_entry list

type 'a lookup_table = (shape list, 'a info_entry list) Hashtbl.t

type 'a term_dextract =
   { ext_lrtable : 'a lookup_table;
     ext_rltable : 'a lookup_table;
     ext_dtable : 'a lookup_table
   }

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

(*
 * Spread out the pairs in the list.
 *)
let rec unzip_list = function
   (x, y)::tl ->
      x :: y :: unzip_list tl
 | [] ->
      []

(*
 * Map f over pairs of values in the list.
 *)
let map_pair f =
   let rec aux = function
      a::b::t ->
         (a, b, (f (a, b))) :: (aux t)
    | [] ->
         []
    | [_] ->
         raise (Invalid_argument "map_pair")
   in
      aux

(*
 * Pair function.
 *)
let shift_pair = function
   h, [a; b] ->
      h, (a, b)
 | _ ->
      raise (Invalid_argument "shift_pair")

(*
 * Shift hd to fst.
 *)
let shift_fst = function
   h::t, [b] ->
      t, (h, b)
 | _ ->
      raise (Invalid_argument "shift_left")

(*
 * Shift hd to snd.
 *)
let shift_snd = function
   h::t, [b] ->
      t, (b, h)
 | _ ->
      raise (Invalid_argument "shift_right")

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty table contains nothing.
 *)
let empty_dtable = []

(*
 * Add an entry.
 *)
let insert_aux t1 t2 v =
   let rw = term_rewrite Rewrite_sig.Relaxed empty_args_spec t1 t2 in
   let template = List.map shape_of_term t1 in
      { info_pattern = template;
        info_rw = rw;
        info_value = v
      }

let insert_left items l v =
   match l with
      (t1, t2)::t ->
         let suffix = unzip_list t in
         let entry1 = LREntry (insert_aux [t1] (t2 :: suffix) v) in
         let entry2 = DEntry (insert_aux [t1; t2] suffix v) in
            entry1 :: entry2 :: items

    | [] ->
         raise (Invalid_argument "insert_left")

let insert_right items l v =
   match l with
      (t1, t2)::t ->
         let suffix = unzip_list t in
         let entry1 = RLEntry (insert_aux [t2] (t1 :: suffix) v) in
         let entry2 = DEntry (insert_aux [t1; t2] suffix v) in
            entry1 :: entry2 :: items

    | [] ->
         raise (Invalid_argument "insert_left")

let insert items l v =
   match l with
      (t1, t2)::t ->
         let suffix = unzip_list t in
         let entry = DEntry (insert_aux [t1; t2] suffix v) in
            entry::items

    | [] ->
         raise (Invalid_argument "insert")

(*
 * Compile the table from the list of entries.
 * Filter out redundant entries.
 *
 * Note that the items lists have to be reversed before insertion
 * so that newer entries will override older ones.
 *)
let extract entries =
   let lrbase = Hashtbl.create 97 in
   let rlbase = Hashtbl.create 97 in
   let dbase = Hashtbl.create 97 in

   (* Insert a new entry into the table *)
   let insert_entry base info =
      let pattern = info.info_pattern in
      let entries =
         try Hashtbl.find base pattern with
            Not_found -> []
      in
      let entries' = info::entries in
         Hashtbl.remove base pattern;
         Hashtbl.add base pattern entries'

   (* Insert a generic item *)
   in let insert_item = function
      LREntry info -> insert_entry lrbase info
    | RLEntry info -> insert_entry rlbase info
    | DEntry info -> insert_entry dbase info

   in
      List.iter insert_item (List.rev entries);
      { ext_lrtable = lrbase;
        ext_rltable = rlbase;
        ext_dtable = dbase
      }

(*
 * Second level of lookup for pair lookups.
 * Choose by trying to apply the rewrite in the argument.
 *)
let find_entry
    (f : term * term -> 'a)
    (f' : term list * term list -> term list * (term * term))
    (entries : 'a info_entry list)
    (t : term)
    (tl : term list) =
   let match_entry { info_rw = rw; info_value = v } =
      let t2 = apply_rewrite rw empty_args t tl in
      let t2', arg = f' (t2, t::tl) in
         v (map_pair f t2') arg
   in
   let rec aux = function
      h::t ->
         begin
            try match_entry h with
               _ -> aux t
         end
    | [] ->
         raise Not_found
   in
      aux entries

(*
 * Lookup an entry.
 * Perform a depth-first search.
 * We assume that no cycles are generated.
 * Paired entries take precedence,
 * the left-to-right rules,
 * then right-to-left rules.
 *)
let rec lookup { ext_lrtable = lrbase;
             ext_rltable = rlbase;
             ext_dtable = dbase
    } t1 t2 =
   let rec aux (t1, t2) =
      let temp1 = shape_of_term t1 in
      let temp2 = shape_of_term t2 in
      let template = [temp1; temp2] in
         try
            find_entry aux shift_pair (Hashtbl.find dbase template) t1 [t2]
         with
            Not_found ->
               begin
                  try
                     find_entry aux shift_snd (Hashtbl.find lrbase [temp1]) t1 []
                  with
                     Not_found ->
                        find_entry aux shift_fst (Hashtbl.find rlbase [temp2]) t2 []
               end
   in
      aux (t1, t2)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
