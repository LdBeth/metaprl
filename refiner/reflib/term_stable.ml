(*
 * This is a simplified version of termTable.
 *)

open Printf
open Debug

open Opname
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermShape

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_stable%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A table is just a list of items.
 *)
type 'a term_stable = 
   Empty
 | Cons of shape * 'a * 'a term_stable
 | Join of 'a term_stable * 'a term_stable

(*
 * An extracted table is a hashtbl.
 *)
type 'a term_sextract = (shape, 'a) Hashtbl.t

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty table.
 *)
let new_stable () =
   Empty

(*
 * Insert into the list.
 *)
let sinsert tbl t v =
   Cons (shape_of_term t, v, tbl)

(*
 * Join the data from two bases.
 *)
let join_stables data1 data2 =
   Join (data1, data2)

(*
 * Compute the hashtable from the info.
 * We only have to check for multiple listings at joins.
 *)
let sextract info =
   let tbl = Hashtbl.create 97 in
   let rec collect tables = function
      Empty ->
         ()
    | Cons (key, v, next) ->
         Hashtbl.add tbl key v;
         collect tables next
    | Join (table1, table2) ->
         if List.memq table1 tables then
            if List.memq table2 tables then
               ()
            else
               collect (table2 :: tables) table2
         else if List.memq table2 tables then
            collect (table1 :: tables) table1
         else
            let tables = table1 :: table2 :: tables in
               collect tables table1;
               collect tables table2
   in
      collect [] info;
      tbl

(*
 * Lookup.
 *)
let slookup tbl t =
   Hashtbl.find tbl (shape_of_term t)

(*
 * $Log$
 * Revision 1.3  1998/06/09 20:52:20  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.2  1998/06/01 13:55:05  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:01:20  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.4  1998/05/27 15:14:52  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.3  1998/04/24 02:43:02  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1997/09/12 17:21:46  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.1  1997/04/28 15:51:45  jyh
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
 * Revision 1.1  1996/11/13 22:58:57  jyh
 * Initial version of forward/backward chaining cache.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
