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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
