(*
 * Grammar utilities.
 *)

open Term
open Term_util
open Filter_type

val build_ml_term : loc -> term -> Ast.expr
val build_ml_mterm : loc -> meta_term -> Ast.expr
val list_expr : loc -> ('a -> Ast.expr) -> 'a list -> Ast.expr
val apply_patt : loc -> ('a -> Ast.patt) -> 'a list -> Ast.patt
val list_patt : loc -> ('a -> Ast.patt) -> 'a list -> Ast.patt
val fun_expr : loc -> string list -> Ast.expr -> Ast.expr

(*
 * $Log$
 * Revision 1.2  1997/05/05 21:04:47  jyh
 * Changing filter_p4 to filter.
 *
 * Revision 1.1  1997/04/28 15:50:51  jyh
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
