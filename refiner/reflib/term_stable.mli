(*
 * Simplified version of termTable.
 *)

open Refiner.Refiner.Term

type 'a term_stable
type 'a term_sextract

val new_stable : unit -> 'a term_stable

val sinsert : 'a term_stable -> term -> 'a -> 'a term_stable
val join_stables : 'a term_stable -> 'a term_stable -> 'a term_stable
val sextract : 'a term_stable -> 'a term_sextract
val slookup : 'a term_sextract -> term -> 'a

(*
 * $Log$
 * Revision 1.2  1998/06/09 20:52:21  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.1  1998/05/28 15:01:21  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.2  1998/05/27 15:14:53  jyh
 * Functorized the refiner over the Term module.
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
 * Revision 1.1  1996/11/13 22:59:00  jyh
 * Initial version of forward/backward chaining cache.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
