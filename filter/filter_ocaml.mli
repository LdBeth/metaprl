(*
 * Convert between camlp4 terms and NL terms.
 *)

open MLast

open Opname
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
 *
 * The extra (MLast.loc -> term -> term) argument is used to annoate the results.
 * It is mapped over all terms postfix order.
 *)
val term_of_expr : (MLast.loc -> term -> term) -> expr -> term
val term_of_patt : (MLast.loc -> term -> term) -> patt -> term
val term_of_type : (MLast.loc -> term -> term) -> ctyp -> term
val term_of_sig_item : (MLast.loc -> term -> term) -> sig_item -> term
val term_of_str_item : (MLast.loc -> term -> term) -> str_item -> term
val term_of_module_type : (MLast.loc -> term -> term) -> module_type -> term
val term_of_module_expr : (MLast.loc -> term -> term) -> module_expr -> term
val term_of_class : (MLast.loc -> term -> term) -> class_decl -> term

(*
 * Specific values useful for writing
 * term interpreters.
 *)
val some_op : opname
val none_op : opname
val true_op : opname
val false_op : opname
val list_op : opname
val cons_op : opname
val nil_op : opname

(*
 * Common destructors.
 *)
val dest_loc : term -> int * int
val dest_loc_string : term -> (int * int) * string
val dest_loc_int : term -> (int * int) * string
val dest_opt : (term -> 'a) -> term -> 'a option
val dest_string : term -> string
val dest_string_opt : term -> string option
val dest_var : term -> string

(*
 * $Log$
 * Revision 1.4  1998/02/19 17:13:58  jyh
 * Splitting filter_parse.
 *
 * Revision 1.3  1998/02/12 23:38:10  jyh
 * Added support for saving intermediate files to the library.
 *
 * Revision 1.2  1998/01/27 23:04:15  jyh
 * Adding OCaml1.07 syntax.
 *
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
