(*
 * Pretty printer for terms.
 *
 *)

open Rformat
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMeta

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

val format_simple_level_exp : buffer -> level_exp -> unit
val print_simple_level_exp_fp : out_channel -> level_exp -> unit
val print_simple_level_exp : level_exp -> unit
val prerr_simple_level_exp : level_exp -> unit
val string_of_level_exp : level_exp -> string

val string_of_opname : opname -> string

val format_simple_param : buffer -> param -> unit
val print_simple_param_fp : out_channel -> param -> unit
val print_simple_param : param -> unit
val prerr_simple_param : param -> unit
val string_of_param : param -> string

val format_simple_term : buffer -> term -> unit
val print_simple_term_fp : out_channel -> term -> unit
val print_simple_term : term -> unit
val prerr_simple_term : term -> unit
val string_of_term : term -> string

val format_simple_bterm : buffer -> bound_term -> unit
val print_simple_bterm_fp : out_channel -> bound_term -> unit
val print_simple_bterm : bound_term -> unit
val prerr_simple_bterm : bound_term -> unit
val string_of_bterm : bound_term -> string

val format_simple_mterm : buffer -> meta_term -> unit
val print_simple_mterm_fp : out_channel -> meta_term -> unit
val print_simple_mterm : meta_term -> unit
val prerr_simple_mterm : meta_term -> unit
val string_of_mterm : meta_term -> string

val print_simple_address_fp : out_channel -> address -> unit
val print_simple_address : address -> unit
val prerr_simple_address : address -> unit

(*
 * $Log$
 * Revision 1.2  1998/05/27 15:14:13  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

