(*
 * Display a particular theory.
 *)

include Base_theory

include Package_info

open Refiner.Refiner.TermType
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
val convert_intf : (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item,
                    term, term, term, term, term, term) convert
val convert_impl : (term, meta_term, 'a proof_type, MLast.ctyp, MLast.expr, MLast.str_item,
                    term, term, term, term, term, term) convert

(*
 * Printers.
 *)
val format_interface : string -> buffer -> Package.package -> unit
val format_implementation : string -> buffer -> Package.package -> unit

val format_packages : buffer -> Package.t -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
