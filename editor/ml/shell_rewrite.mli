(*
 * Commands for editing a rewrite.
 *)

include Shell_type
include Package_info
include Package_df

open Refiner.Refiner.TermType

open Filter_cache
open Filter_summary

open Tacticals
open Shell_type
open Package_info

(*
 * Make an editable rewrite.
 *)
val create :
   Package.package ->
   string ->
   edit_object

val view_rw :
   Package.package ->
   (term, Package.proof proof_type) rewrite_info ->
   edit_object

val view_crw :
   Package.package ->
   (term, Package.proof proof_type) cond_rewrite_info ->
   edit_object

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
