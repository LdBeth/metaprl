(*
 * Conversion form filter_summary to program text.
 *)

open Term

open Filter_summary
open Filter_cache

(*
 * Program extraction from sepc.
 *)
val extract_sig :
   (unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info -> string ->
   (MLast.sig_item * (int * int)) list

val extract_str :
   (proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info -> string ->
   (MLast.str_item * (int * int)) list

(*
 * $Log$
 * Revision 1.2  1998/02/23 14:46:16  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1998/02/21 20:57:49  jyh
 * Two phase parse/extract.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
