(*
 * Hash functions.
 *)

val hash_expr : int -> MLast.expr -> int
val hash_patt : int -> MLast.patt -> int
val hash_type : int -> MLast.ctyp -> int
val hash_sig_item : int -> MLast.sig_item -> int
val hash_str_item : int -> MLast.str_item -> int
val hash_module_type : int -> MLast.module_type -> int
val hash_module_expr : int -> MLast.module_expr -> int

(*
 * $Log$
 * Revision 1.2  1997/08/06 16:17:30  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:54  jyh
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
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
