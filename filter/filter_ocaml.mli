(*
 * Convert between camlp4 terms and NL terms.
 *)

open MLast

open Term

(*
 * Terms to MLAst.
 * FormatError (reason, term that failed)
 *)
exception FormatError of string * term

val expr_of_term : term -> expr
val patt_of_term : term -> patt
val type_of_term : term -> ctyp
val sig_item_of_term : term -> sig_item
val str_item_of_term : term -> str_item
val module_type_of_term : term -> module_type
val module_expr_of_term : term -> module_expr
val class_of_term : term -> class_decl

(*
 * MLast to term.
 *)
val term_of_expr : expr -> term
val term_of_patt : patt -> term
val term_of_type : ctyp -> term
val term_of_sig_item : sig_item -> term
val term_of_str_item : str_item -> term
val term_of_module_type : module_type -> term
val term_of_module_expr : module_expr -> term
val term_of_class : class_decl -> term

(*
 * $Log$
 * Revision 1.1  1997/09/12 17:21:37  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
