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
open Term
open Term_util
open Term_template
open Rewrite

open Simple_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_table%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * pre-entries just contain the pattern/value pair.
 *)
type 'a info_entry =
   { info_term : term;
     info_pattern : term_template;
     info_redex : rewrite_redex;
     info_value : 'a
   }

(*
 * We can add entries, as well as other tables.
 * When the tables are compiled, common ancestors
 * are merged.
 *)
type 'a table_item =
   Entry of 'a info_entry
 | Table of ('a table_item) list

(*
 * A compiled table is a hashtable of entries.
 *)
type 'a term_extract = (term_template, ('a info_entry) list) Hashtbl.t

(*
 * A table has a list of pairs, plus an position for a compute
 * table.
 *)
type 'a term_table =
   { tbl_items : ('a table_item) list;
     mutable tbl_base : 'a term_extract option
   }

(*
 * Destruction.
 *)
type 'a table_entry =
   TableEntry of term * 'a
 | TableTable of 'a term_table

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty table contains nothing.
 *)
let new_table () = { tbl_items = []; tbl_base = None }

(*
 * Destruction.
 *)
let is_empty_table = function
   { tbl_items = [] } -> true
 | _ -> false

let equal_tables { tbl_items = items1 } { tbl_items = items2 } =
   items1 == items2

let dest_table { tbl_items = items } =
   match items with
      [] -> raise (Invalid_argument "dest_table")
    | x::tl ->
         let entry =
            match x with
               Entry { info_term = t; info_value = x } -> TableEntry (t, x)
             | Table t -> TableTable { tbl_items = t; tbl_base = None }
         in
            entry, { tbl_items = tl; tbl_base = None }

(*
 * Add an entry.
 *)
let insert { tbl_items = items } t v =
   let template = compute_template t in
   let redex = compile_redex [||] t in
   let entry =
      { info_term = t;
        info_pattern = template;
        info_redex = redex;
        info_value = v
      }
   in
      { tbl_items = (Entry entry)::items; tbl_base = None }

(*
 * Join another table.
 *)
let join_tables { tbl_items = entries } { tbl_items = items } =
   { tbl_items = (Table items)::entries; tbl_base = None }

(*
 * Compile the table from the list of entries.
 * Filter out redundant entries.
 *
 * Note that the items lists have to be reversed before insertion
 * so that newer entries will override older ones.
 *)
let compute_base entries =
   let base = Hashtbl.create 97 in

   (* Insert a new entry into the table *)
   let insert_entry ({ info_term = term; info_pattern = pattern } as info) =
      let entries = 
         try Hashtbl.find base pattern with
            Not_found -> []
      in
      let gen_filter { info_term = term' } =
         generalizes term term'
      in
      let entries' = info :: (List_util.filter gen_filter entries) in
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
         let tables' = t :: tables in
            List.fold_left insert_item tables' (List.rev t)

   (* Insert a generic item *)
   and insert_item tables = function
      Entry info -> insert_entry info; tables
    | Table t -> insert_table tables t

   in
      List.fold_left insert_item [] (List.rev entries);
      base

(*
 * Lookup the base.
 *)
let get_base = function
   { tbl_base = Some base } ->
      base
 | { tbl_items = items } as tbl ->
      let base = compute_base items in
         tbl.tbl_base <- Some base;
         base

(*
 * Second level of lookup.
 *)
let find_entry entries t =
   let match_entry { info_term = t'; info_redex = redex; info_value = v } =
      if !debug_dform then
         eprintf "Term_table.find_entry.match_entry: %s%t" (string_of_term t') eflush;
      let stack, items = apply_redex' redex [||] t in
         stack, items, v
   in
   let rec aux = function
      h::t ->
         begin
            try match_entry h with
               _ ->
                  aux t
         end
    | [] ->
         raise Not_found
   in
      aux entries

(*
 * Lookup an entry.
 *)
let lookup tbl t =
   (* First level of lookup *)
   let base = get_base tbl in
   let template = compute_template t in
   let entries = Hashtbl.find base template in
      (* Second level of lookup *)
      find_entry entries t

(*
 * $Log$
 * Revision 1.4  1998/04/28 21:38:11  jyh
 * Adjusted uppercasing.
 *
 * Revision 1.3  1998/04/28 18:30:49  jyh
 * ls() works, adding display.
 *
 * Revision 1.2  1998/04/24 02:43:03  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:51:46  jyh
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
 * Revision 1.3  1996/11/13 22:59:01  jyh
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
