(*
 * Grammar utilities.
 *)

open Term
open Term_util
open Filter_type

val build_ml_term : MLast.loc -> term -> MLast.expr
val build_ml_mterm : MLast.loc -> meta_term -> MLast.expr
val list_expr : MLast.loc -> ('a -> MLast.expr) -> 'a list -> MLast.expr
val apply_patt : MLast.loc -> ('a -> MLast.patt) -> 'a list -> MLast.patt
val list_patt : MLast.loc -> ('a -> MLast.patt) -> 'a list -> MLast.patt
val fun_expr : MLast.loc -> string list -> MLast.expr -> MLast.expr

(*
 * $Log$
 * Revision 1.3  1997/08/06 16:17:27  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
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
