(*
 * Make a hashtable for terms based on patterns.
 * Essentially, we want to be able to construct tables of pairs
 *    (pattern, 'a)
 * where pattern is a pattern that matches terms.  The lookup
 * function:
 *    lookup : table -> term -> 'a
 * should retrieve the value with the most specific pattern match.
 *
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.Rewrite

type 'a term_table

val new_table : unit -> 'a term_table
val insert : 'a term_table -> term -> 'a -> 'a term_table
val join_tables : 'a term_table -> 'a term_table -> 'a term_table
val lookup : string -> 'a term_table -> term -> rewrite_stack * rewrite_item list * 'a

(*
 * Destruction.
 *)
type 'a table_entry =
   TableEntry of term * 'a
 | TableTable of 'a term_table

val is_empty_table : 'a term_table -> bool
val equal_tables : 'a term_table -> 'a term_table -> bool
val dest_table : 'a term_table -> 'a table_entry * 'a term_table

(*
 * Debugging.
 *)
val print_table : 'a term_table -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
