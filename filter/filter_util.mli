(*
 * Common utilities for filtering modules.
 *
 *)

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

(************************************************************************
 * UTILITIES								*
 ************************************************************************)

val context_vars_list : term list -> string list
val binding_vars_list : term list -> string list
val unzip_rewrite : string -> meta_term -> term list * term * term

val split_mfunction : meta_term -> term list * meta_term

(*
 * Module paths.
 *)
val string_of_path : module_path -> string
val output_path : out_channel -> module_path -> unit

(************************************************************************
 * OPNAMES								*
 ************************************************************************)

val string_of_opname_list : string list -> string
val translate_opname : opname -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
