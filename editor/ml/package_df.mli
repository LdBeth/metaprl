(*
 * Display a particular theory.
 *)

open Term
open Rformat

open Filter_cache

include Base_theory

include Package_info

(*
 * Default proof used for display.
 *)
val null_term : term
val null_proof : proof_type

(*
 * Summaries conversions.
 *)
val convert_intf : (unit, MLast.ctyp, MLast.expr, MLast.sig_item) convert
val convert_impl : (proof_type, MLast.ctyp, MLast.expr, MLast.str_item) convert

(*
 * Printers.
 *)
val format_interface : string -> buffer -> Package.package -> unit
val format_implementation : string -> buffer -> Package.package -> unit

val format_packages : buf -> Package.t -> unit

(*
 * $Log$
 * Revision 1.1  1998/04/17 20:48:13  jyh
 * Updating refiner for extraction.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
