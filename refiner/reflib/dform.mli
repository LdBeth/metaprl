(*
 * Display form handler.
 *
 *)

open Precedence
open Rformat
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite

(************************************************************************
 * DFORM INTERFACE                                                      *
 ************************************************************************)

(*
 * The display database is functional, and display formas
 * are added for term templates.
 *)
type dform_base

(*
 * A display form printer knows about this term, and
 * a printer for subterms.  The subterm printer takes
 * an extra argument that specifies parenthesization.
 *)
type parens =
   NOParens
 | LTParens
 | LEParens

type dform_printer_info =
   { dform_term : term;
     dform_stack : rewrite_stack;
     dform_items : rewrite_item list;
     dform_printer : buffer -> parens -> term -> unit;
     dform_buffer : buffer
   }

type dform_printer =
   DFormExpansion of term
 | DFormPrinter of (dform_printer_info -> unit)

(*
 * Options on a dform.
 *)
type dform_option =
   (* Parens and precedences *)
   DFormInheritPrec
 | DFormPrec of precedence
 | DFormParens

(*
 * This is the info needed for each display form.
 *)
type dform_info =
   { dform_name : string;
     dform_pattern : term;
     dform_options : dform_option list;
     dform_print : dform_printer
   }

(*
 * Destruct a base.
 *)
type dform_entry =
   DFormEntry of dform_info
 | DFormBase of dform_base

(*
 * Display form installation.
 *)
val null_base : dform_base
val add_dform : dform_base -> dform_info -> dform_base

(*
 * Join two bases.
 *)
val join_dforms : dform_base -> dform_base -> dform_base

val is_null_dfbase : dform_base -> bool
val equal_dfbases : dform_base -> dform_base -> bool
val dest_dfbase : dform_base -> dform_entry * dform_base

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

val format_quoted_term : dform_base -> buffer -> term -> unit
val format_term : dform_base -> buffer -> term -> unit
val print_term_fp : dform_base -> out_channel -> term -> unit
val print_term : dform_base -> term -> unit
val prerr_term : dform_base -> term -> unit
val string_of_term : dform_base -> term -> string

val format_short_term : dform_base -> (string -> opname) -> buffer -> term -> unit
val print_short_term_fp : dform_base -> (string -> opname) -> out_channel -> term -> unit

val format_bterm : dform_base -> buffer -> bound_term -> unit
val print_bterm_fp : dform_base -> out_channel -> bound_term -> unit
val print_bterm : dform_base -> bound_term -> unit
val prerr_bterm : dform_base -> bound_term -> unit
val string_of_bterm : dform_base -> bound_term -> string

val format_mterm : dform_base -> buffer -> meta_term -> unit
val print_mterm_fp : dform_base -> out_channel -> meta_term -> unit
val print_mterm : dform_base -> meta_term -> unit
val prerr_mterm : dform_base -> meta_term -> unit
val string_of_mterm : dform_base -> meta_term -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
