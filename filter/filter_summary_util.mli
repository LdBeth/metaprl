(*
 * These are utilities for the filter_summary module.
 *)

open Term
open Opname
open Filter_type
open Filter_summary
open Filter_cache

(************************************************************************
 * CONTEXT PARAM OPERATORS                                              *
 ************************************************************************)

val param_expr : loc -> param -> Ast.expr
val params_ctyp : loc -> Ast.ctyp -> param list -> Ast.ctyp
val extract_params : string list -> string list -> term list -> param list

(************************************************************************
 * OPNAMES                                                              *
 ************************************************************************)

(*
 * Make an opname from an ascii spec.
 *)
val mk_opname : module_cache -> loc -> string list -> opname

(*
 * $Log$
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
