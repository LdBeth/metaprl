(*
 * Commands for editing a rewrite.
 *)

include Shell_type
include Package_info
include Package_df

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
   Package.proof proof_type rewrite_info ->
   edit_object

val view_crw :
   Package.package ->
   Package.proof proof_type cond_rewrite_info ->
   edit_object

(*
 * $Log$
 * Revision 1.6  1998/07/02 18:34:45  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.5  1998/05/29 14:52:54  jyh
 * Better Makefiles.
 *
 * Revision 1.4  1998/05/28 13:46:04  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
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
