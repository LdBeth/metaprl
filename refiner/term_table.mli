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

open Term
open Rewrite

type 'a term_table

val new_table : unit -> 'a term_table
val insert : 'a term_table -> term -> 'a -> 'a term_table
val join_tables : 'a term_table -> 'a term_table -> 'a term_table
val lookup : 'a term_table -> term -> rewrite_stack * rewrite_item list * 'a

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
 * $Log$
 * Revision 1.3  1998/05/01 14:59:44  jyh
 * Updating display forms.
 *
 * Revision 1.2  1998/04/29 14:48:32  jyh
 * Added ocaml_sos.
 *
 * Revision 1.1  1997/04/28 15:51:47  jyh
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
 * Revision 1.2  1996/05/21 02:14:27  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/04/07 18:27:10  jyh
 * Intermediate checking while updating dform commands.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
