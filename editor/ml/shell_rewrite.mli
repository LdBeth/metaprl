(*
 * Commands for editing a rewrite.
 *)

include Shell_type
include Package_info
include Package_df

open Filter_cache
open Filter_summary

open Tactic_type
open Shell_type
open Package_info

(*
 * Make an editable rewrite.
 *)
val create :
   Package.package ->
   Filter_prog.t ->
   tactic_resources ->
   cache ->
   string ->
   edit_object

val view_rw :
   Package.package ->
   Filter_prog.t ->
   tactic_resources ->
   cache ->
   (string * tactic) array ->
   proof_type rewrite_info ->
   edit_object

val view_crw :
   Package.package ->
   Filter_prog.t ->
   tactic_resources ->
   cache ->
   (string * tactic) array ->
   proof_type cond_rewrite_info ->
   edit_object

(*
 * $Log$
 * Revision 1.3  1998/05/07 16:02:27  jyh
 * Adding interactive proofs.
 *
 * Revision 1.2  1998/04/23 20:04:22  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.1  1998/04/17 20:48:18  jyh
 * Updating refiner for extraction.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
