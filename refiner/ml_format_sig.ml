(*
 * Shared types for printing ML terms.
 *)

open Term
open Term_util

(*
 * OCAML expression syntax.
 *)
type ml_expr =
   ML_Var of string
 | ML_Int of int
 | ML_Num of Num.num
 | ML_String of string
 | ML_List of ml_expr list
 | ML_Let of string * ml_expr * ml_expr
 | ML_Apply of ml_expr list
 | ML_Tuple of ml_expr list
 | ML_Record of (ml_expr * ml_expr) list
 | ML_Module_Var of string list

(*
 * Printer module.
 *)
module type FormatTermSig =
sig
   val format_term : term -> ml_expr
   val format_mterm : meta_term -> ml_expr
   val format_term_list : term list -> ml_expr
end

(*
 * $Log$
 * Revision 1.2  1998/03/20 22:16:18  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.1  1997/04/28 15:51:24  jyh
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
