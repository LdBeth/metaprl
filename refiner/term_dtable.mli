(*
 * Simplified version of termTable.
 *)

open Refiner.Refiner.Term

type 'a term_dtable
type 'a term_dextract

type 'a pair_fun = (term * term * 'a) list -> term * term -> 'a

val new_dtable : unit -> 'a term_dtable

val insert_left : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val insert_right : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val insert : 'a term_dtable -> (term * term) list -> 'a pair_fun -> 'a term_dtable
val join_tables : 'a term_dtable -> 'a term_dtable -> 'a term_dtable
val extract : 'a term_dtable -> 'a term_dextract
val lookup : 'a term_dextract -> term -> term -> 'a

(*
 * $Log$
 * Revision 1.3  1998/05/27 15:14:23  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.2  1998/04/29 14:48:29  jyh
 * Added ocaml_sos.
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
 * Revision 1.1  1996/11/13 22:58:42  jyh
 * Initial version of forward/backward chaining cache.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
