(*
 * Compute the free vars of a term.
 *)

val patt_vars : Ast.patt -> string list
val free_vars : Ast.expr -> string list
val new_vars : Ast.expr -> string list -> string list

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:03  jyh
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
