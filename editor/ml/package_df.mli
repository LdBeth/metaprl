(*
 * Display a particular theory.
 *)

include Base_theory

include Package_info

open Refiner.Refiner.Term
open Rformat

open Filter_cache

open Filter_summary

open Package_info

(*
 * Display parameters.
 *)
val tabstop : int
val min_screen_width : int

(*
 * Default proof used for display.
 *)
val null_term : term
val null_proof : 'a proof_type

(*
 * Summaries conversions.
 *)
val convert_intf : (unit, MLast.ctyp, MLast.expr, MLast.sig_item, term, term, term, term) convert
val convert_impl : ('a proof_type, MLast.ctyp, MLast.expr, MLast.str_item, term, term, term, term) convert

(*
 * Printers.
 *)
val format_interface : string -> buffer -> Package.package -> unit
val format_implementation : string -> buffer -> Package.package -> unit

val format_packages : buffer -> Package.t -> unit

(*
 * $Log$
 * Revision 1.3  1998/05/28 13:45:40  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.2  1998/04/23 20:03:40  jyh
 * Initial rebuilt editor.
 *
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
