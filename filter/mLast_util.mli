(*
 * Handle commenting.
 * We parse a file, and scan in the comments and their
 * locations.  then we pass through the terms, and match their
 * locations with the comments.
 *)

(*
 * Iteration functions.
 *)
type 'a fold =
   { fold_expr        : 'a -> MLast.expr -> 'a;
     fold_patt        : 'a -> MLast.patt -> 'a;
     fold_type        : 'a -> MLast.ctyp -> 'a;
     fold_sig_item    : 'a -> MLast.sig_item -> 'a;
     fold_str_item    : 'a -> MLast.str_item -> 'a;
     fold_module_expr : 'a -> MLast.module_expr -> 'a;
     fold_module_type : 'a -> MLast.module_type -> 'a;
     fold_with_constr : 'a -> MLast.with_constr -> 'a;
     fold_class       : 'a -> MLast.class_decl -> 'a;
     fold_class_field : 'a -> MLast.class_field -> 'a
   }

(*
 * Ast folders.
 *)
val fold_expr : 'a fold -> 'a -> MLast.expr -> 'a
val fold_patt : 'a fold -> 'a -> MLast.patt -> 'a
val fold_type : 'a fold -> 'a -> MLast.ctyp -> 'a
val fold_sig_item : 'a fold -> 'a -> MLast.sig_item -> 'a
val fold_str_item : 'a fold -> 'a -> MLast.str_item -> 'a
val fold_module_expr : 'a fold -> 'a -> MLast.module_expr -> 'a
val fold_module_type : 'a fold -> 'a -> MLast.module_type -> 'a
val fold_with_constr : 'a fold -> 'a -> MLast.with_constr -> 'a
val fold_class : 'a fold -> 'a -> MLast.class_decl -> 'a
val fold_class_field : 'a fold -> 'a -> MLast.class_field -> 'a

(*
 * $Log$
 * Revision 1.1  1998/02/19 17:14:05  jyh
 * Splitting filter_parse.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
