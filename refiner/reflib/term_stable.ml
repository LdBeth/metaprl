(*
 * This is a simplified version of termTable.
 *)

open Printf
open Nl_debug

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
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
