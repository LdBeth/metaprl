(*
 * The proof editor constructs a proof interactively.
 * We provide a notion of a "current" address into the
 * proof, which is the point in the proof that is displayed
 * on the screen.
 *
 * At the base level, this data structure just adds undo capability
 * to proofs, and in doing so, the operations become imperative.
 *
 * Also add display capability.
 *)

include Proof

open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Rformat
open Dform

open Sequent
open Tacticals

(*
 * The is the state of the current proof.
 *)
type t

(*
 * Constructors.
 *)
val create : term Filter_summary.param list -> tactic_arg -> t
val ped_of_proof : term Filter_summary.param list -> Proof.t -> t
val set_params : t -> term Filter_summary.param list -> unit
val ped_arg : t -> tactic_arg

(*
 * Destructors.
 *)
val proof_of_ped : t -> Proof.t
val status_of_ped : t -> Proof.status

(*
 * Display operation.
 *)
val format : dform_base -> buffer -> t -> unit

(*
 * Refinement, and undo lists.
 * A finite number of undo's are allowed.
 * The (string, Ast.expr, tactic) are all different
 * representations of the same thing.
 *
 * After a refine_ped or nop_ped, the undo stack gets reset.
 * The nop_ped does nothing but reset the undo stack.
 *)
val refine_ped : t -> string -> MLast.expr -> tactic -> unit
val undo_ped : t -> unit
val nop_ped : t -> unit
val fold_ped : t -> unit
val fold_all_ped : t -> unit

(*
 * Navigation.
 *)
val up_ped : t -> int -> unit
val down_ped : t -> int -> unit
val root_ped : t -> unit

(*
 * Check the proof and return its extract.
 * Two versions for handling refinement errors:
 *    check_proof: expand until first error, exceptions propagate
 *       On failure, the ped is modified to point to the error
 *    expand_proof: check as much of the proof as possible,
 *       no exceptions are raised
 *)
val check_ped : t -> extract
val expand_ped : dform_base -> t -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
