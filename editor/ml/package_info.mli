(*
 * The editor collects information about each theory and
 * summarizes it in a "package".  At start-up, the pre-loaded packages
 * are collected and presented as read-only theories.
 *
 * We can also create interactive packages, which are writable,
 * and may contain interactivly generated proofs.
 *)

open List
open Util
open Term
open Dform_print
open Theory
open Proof_edit
open Filter_cache

(*
 * Save status of a module.
 *)
type package_status =
   PackageUnmodified
 | PackageModified
 | PackageReadOnly

(*
 * This is the info for each open Package.
 *)
type package_info
     
exception PackageReadError of package_info

(*
 * Info about theorems.
 *)
type thm_info =
   { thm_name : string;
     thm_ped : ped
   }
     
(*
 * Create a Package from a raw theory.
 * This creates a read-only Package.
 *)
val package_of_theory : theory -> package_info
val create_package : string -> package_info
val make_package : theory -> thm_info list -> string -> package_info

(*
 * Access.
 *)
val package_status : package_info -> package_status
val package_cache : package_info -> module_cache
val package_refiner : package_info -> refiner
val package_dforms : package_info -> dform_mode_base
val package_thms : package_info -> thm_info list
val package_name : package_info -> string
val package_file : package_info -> string option

(*
 * Modification.
 *)
val package_set_file : package_info -> string -> unit

(*
 * Interactive additions.
 *)
val package_add_parent : package_info -> package_info -> unit
val package_add_thm : package_info -> string -> ped -> unit

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:17  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:33:24  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:31  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:59  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
