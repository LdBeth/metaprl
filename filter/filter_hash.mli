(*
 * Hash functions.
 *)

val hash_expr : int -> Ast.expr -> int
val hash_patt : int -> Ast.patt -> int
val hash_type : int -> Ast.ctyp -> int
val hash_sig_item : int -> Ast.sig_item -> int
val hash_str_item : int -> Ast.str_item -> int
val hash_module_type : int -> Ast.module_type -> int
val hash_module_expr : int -> Ast.module_expr -> int

(*
 * $Log$
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
