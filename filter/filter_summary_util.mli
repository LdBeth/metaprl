(*
 * These are utilities for the filter_summary module.
 *)

open Term

val param_expr : MLast.loc -> Filter_summary.param -> MLast.expr
val params_ctyp : MLast.loc -> MLast.ctyp -> Filter_summary.param list -> MLast.ctyp
val extract_params : string list -> string list -> term list -> Filter_summary.param list

(*
 * $Log$
 * Revision 1.5  1998/02/19 17:14:04  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1997/08/06 16:17:36  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:01  jyh
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
