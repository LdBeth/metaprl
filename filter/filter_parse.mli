(*
 * Parser for PRL files.
 *)

open Refiner.Refiner.Term

(* Add an include directory *)
val set_include_path : string list -> unit

(*
 * $Log$
 * Revision 1.5  1998/05/27 15:12:53  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.4  1998/04/28 18:30:14  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/02/19 17:13:59  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1998/01/27 23:04:19  jyh
 * Adding OCaml1.07 syntax.
 *
 * Revision 1.1  1997/04/28 15:50:57  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
