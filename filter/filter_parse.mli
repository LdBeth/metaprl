(*
 * Parser for PRL files.
 *)

open Term

(* Add an include directory *)
val set_include_path : string list -> unit
 
(*
 * The interpretation function on terms.
 * BUG: this should be a read-time resource.
 *)
val set_interp : (term -> term) -> unit

(*
 * $Log$
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
