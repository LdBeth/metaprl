(*
 * The editor collects information about each theory and
 * summarizes it in a "package".  At start-up, the pre-loaded packages
 * are collected and presented as read-only theories.
 *
 * We can also create interactive packages, which are writable,
 * and may contain interactivly generated proofs.
 *)

include Package_type
include Proof_type

open Refiner.Refiner.Refine

open Filter_prog

open Tactic_type
open Package_type
open Proof_type

module Extract : ExtractSig
module Package : PackageSig
                 with type proof = Extract.proof

(*
 * Auxiliary functions for managing resources and proofs.
 *)
val install_tactic_argument : tactic_argument -> unit
val prove : string -> (string * tactic) array -> refiner -> unit -> extract

(*
 * For debugging purposes.
 *)
val debug_item : MLast.str_item ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
