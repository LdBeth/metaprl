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
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
