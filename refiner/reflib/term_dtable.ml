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
 *)

open Printf
open Debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermShape
open Refiner.Refiner.Rewrite

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_dtable%t" eflush

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
 | Table of 'a table_entry list

(*
 * A table has a list of pairs, plus an position for a compute
 * table.
 *)
type 'a term_dtable =
   { table_items : 'a table_entry list }

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
let new_dtable () =
   { table_items = [] }

(*
 * Add an entry.
 *)
let insert_aux t1 t2 v =
   let rw = term_rewrite ([||], [||]) t1 t2 in
   let template = List.map shape_of_term t1 in
      { info_pattern = template;
        info_rw = rw;
        info_value = v
      }

let insert_left { table_items = items } l v =
   match l with
      (t1, t2)::t ->
         let suffix = unzip_list t in
         let entry1 = LREntry (insert_aux [t1] (t2 :: suffix) v) in
         let entry2 = DEntry (insert_aux [t1; t2] suffix v) in
            { table_items = entry1 :: entry2 :: items }

    | [] ->
         raise (Invalid_argument "insert_left")

let insert_right { table_items = items } l v =
   match l with
      (t1, t2)::t ->
         let suffix = unzip_list t in
         let entry1 = RLEntry (insert_aux [t2] (t1 :: suffix) v) in
         let entry2 = DEntry (insert_aux [t1; t2] suffix v) in
            { table_items = entry1 :: entry2 :: items }

    | [] ->
         raise (Invalid_argument "insert_left")

let insert { table_items = items } l v =
   match l with
      (t1, t2)::t ->
         let suffix = unzip_list t in
         let entry = DEntry (insert_aux [t1; t2] suffix v) in
            { table_items = entry::items }

    | [] ->
         raise (Invalid_argument "insert")

(*
 * Join another table.
 *)
let join_tables { table_items = items1 } { table_items = items2 } =
   { table_items = (Table items2)::items1 }

(*
 * Compile the table from the list of entries.
 * Filter out redundant entries.
 *
 * Note that the items lists have to be reversed before insertion
 * so that newer entries will override older ones.
 *)
let extract { table_items = entries } =
   let lrbase = Hashtbl.create 97 in
   let rlbase = Hashtbl.create 97 in
   let dbase = Hashtbl.create 97 in

   (* Insert a new entry into the table *)
   let insert_entry base ({ info_pattern = pattern } as info) =
      let entries =
         try Hashtbl.find base pattern with
            Not_found -> []
      in
      let entries' = info::entries in
         Hashtbl.remove base pattern;
         Hashtbl.add base pattern entries'
   in

   (*
    * Insert another table.  Keep a list of tables that
    * have been inserted so that each table is only inserted
    * once.  Tables are just lists of items; compare them with
    * physical equality.
    *)
   let rec insert_table tables t =
      (* Remember, t is a list of items *)
      if List.memq t tables then
         tables
      else
         let tables' = t::tables in
            List.fold_left insert_item tables' (List.rev t)

   (* Insert a generic item *)
   and insert_item tables = function
      LREntry info -> insert_entry lrbase info; tables
    | RLEntry info -> insert_entry rlbase info; tables
    | DEntry info -> insert_entry dbase info; tables
    | Table t -> insert_table tables t

   in
      List.fold_left insert_item [] (List.rev entries);
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
    (t : term list) =
   let match_entry { info_rw = rw; info_value = v } =
      let t2, _ = apply_rewrite rw ([||], [||]) t in
      let t2', arg = f' (t2, t) in
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
let lookup { ext_lrtable = lrbase;
             ext_rltable = rlbase;
             ext_dtable = dbase
    } t1 t2 =
   let rec aux (t1, t2) =
      let arg = [t1; t2] in
      let temp1 = shape_of_term t1 in
      let temp2 = shape_of_term t2 in
      let template = [temp1; temp2] in
         try
            find_entry aux shift_pair (Hashtbl.find dbase template) arg
         with
            Not_found ->
               begin
                  try
                     find_entry aux shift_snd (Hashtbl.find lrbase [temp1]) [t1]
                  with
                     Not_found ->
                        find_entry aux shift_fst (Hashtbl.find rlbase [temp2]) [t2]
               end
   in
      aux (t1, t2)

(*
 * $Log$
 * Revision 1.3  1998/06/09 20:52:19  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.2  1998/06/01 13:55:04  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:01:16  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.4  1998/05/27 15:14:22  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.3  1998/04/29 14:48:27  jyh
 * Added ocaml_sos.
 *
 * Revision 1.2  1998/04/24 02:43:00  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:51:44  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.1  1996/11/13 22:58:36  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.2  1996/05/21 02:14:25  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/04/07 18:27:09  jyh
 * Intermediate checking while updating dform commands.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
