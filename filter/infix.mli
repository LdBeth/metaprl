(*
 * Code for adding infixes to the expression syntax.
 *)

open Filter_type

val make_infix : loc -> string -> Ast.expr -> Ast.expr -> Ast.expr
val add_infix : string -> unit

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:04  jyh
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
