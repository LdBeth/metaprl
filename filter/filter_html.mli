(*
 * Conversion form filter_summary to HTML text.
 *)

open Refiner.Refiner.Term

open Filter_summary
open Filter_cache

(*
 * Program extraction from sepc.
 *)
val print_sig : out_channel ->
   (unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info -> unit

val print_str : out_channel ->
   (proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info -> unit

(*
 * $Log$
 * Revision 1.2  1998/05/27 15:12:46  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.1  1998/03/12 00:27:06  jyh
 * Added filter_html, but its not finished yet.
 *
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
