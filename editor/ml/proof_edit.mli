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
open Tactic_type

(*
 * The is the state of the current proof.
 *)
type t

(*
 * Constructors.
 *)
val create : Filter_summary.param list -> tactic_arg -> t
val ped_of_proof : Proof.t -> t
val set_params : t -> Filter_summary.param list -> unit
val ped_arg : t -> tactic_arg

(*
 * Destructors.
 *)
val proof_of_ped : t -> Proof.t

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
val up_ped : t -> unit
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
 * $Log$
 * Revision 1.5  1998/05/28 13:45:52  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.4  1998/04/28 18:29:48  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/23 20:03:55  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.2  1998/04/09 19:07:26  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:23  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.3  1996/09/02 19:33:35  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:41  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 17:00:09  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
