(*
 * A proof is a collection of inferences, where each inference is
 * a proof step or it is a nested proof.  Each inference
 * has the same goal as a subgoal of a previous inference.
 *
 *                   Goal           status:
 *                    |                bad: one if the proof_items has failed
 *                    |                partial: some incomplete subgoals
 *                    |                asserted: pretend like the proof is complete
 *                    |                complete: all steps have been checked
 *                    |
 *                   Item           proof_item
 *                  / | \
 *                 /  |  \
 *                /   |   \
 *               C1   C2  C3        children
 *              / |   |   | \
 *             /  |   |   |  \
 *            .   .   .   .   .
 *           .    .   .   .    .
 *          SG1  SG2 SG3 SG4  SG5   subgoals
 *
 * We also provide tools for navigation:
 *    1. Get the parent inference
 *    2. Get a subgoal inference
 *    3. Replace a subgoal inference
 *    4. Replace the tactic of the current inference
 *
 *)

include Io_proof_type
include Proof_type
include Tactic_type

include Proof_step

open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Dform

open Io_proof_type
open Tactic_type
open Proof_type

(* Abstract type *)
type t

(*
 * Head of a proof is a single step, or another proof.
 * This is the "justification" between the goal of the
 * proof and the first children.
 *)
type item =
   ProofStep of Proof_step.t
 | ProofProof of t

(*
 * Children are either leaves, or they
 * are subproofs.
 *)
type child =
   ChildTerm of tactic_arg
 | ChildProof of t

(*
 * Status of a proof node is cached,
 * but it can be computed from the children.
 * A proof is:
 *    bad: if the tactic in the rule box fails, or any of the children are bad
 *    partial: the proof is just a goal, or any of the children are partial
 *    asserted: force the proof to be accepted
 *    complete: all the children are complete
 *)
type status =
   Bad
 | Partial
 | Asserted
 | Complete

(*
 * An address is a integer path.
 *)
type address = int list

(*
 * This exception is raised when composed proofs don't match.
 * The goal if the subproof must be alpha-equal to the child
 * of the parent.
 *)
exception Match

(*
 * We overload the refinement error to give the location of
 * the error.
 *)
exception ProofRefineError of t * refine_error

(*
 * Constructors
 *)
val of_step : Proof_step.t -> t

(*
 * Destructors of three types:
 * I. Proof as a whole
 *   a. proof_goal: main goal of the proof
 *   b. proof_subgoals: list the leaves of the proof
 * II. Uppermost inference only
 *   a. proof_item: first proof step
 *   b. proof_children: first list of nodes
 * III. Proof as subproof of a surrounding proof
 *   a. proof_parent: go up the tree one inference
 *   b. proof_main: get outermost proof
 *   c. proof_status: trace status of proof up the tree to root
 *   d. proof_address: return address of current proof relative to root
 *)
val goal : t -> tactic_arg
val subgoals : t -> tactic_arg list

val item : t -> item
val children : t -> child list

val parent : t -> t
val main : t -> t
val status : t -> (status * int) list
val node_status : t -> status
val address : t -> address

(*
 * Addressed subgoal.  The address is the
 * path to the subgoal -- raises InvalidAddress
 * if the address is bad.
 *)
val index : t -> address -> t
val child : t -> int -> t

(*
 * Functional updates.
 * For replace_item, the goal
 *    and subgoals must match the previous values.
 * For replace_child, the goal must match the given child goal.
 *)
val fold : t -> t
val fold_all : t -> t
val replace_item : t -> item -> t
val replace_child : t -> int -> t -> t
val remove_child : t -> int -> t
val remove_children : t -> t

(*
 * Check the proof and return its extract.
 * Two versions for handling refinement errors:
 *    check_proof: expand until first error, exceptions propagate
 *    expand_proof: check as much of the proof as possible,
 *       no exceptions are raised
 *)
val check : t -> extract
val expand : dform_base -> t -> t

(*
 * IO
 *)
val io_proof_of_proof : t -> proof
val proof_of_io_proof : tactic_argument -> (string * tactic) array -> proof -> t

(*
 * $Log$
 * Revision 1.11  1998/06/15 22:31:46  jyh
 * Added CZF.
 *
 * Revision 1.10  1998/06/09 20:51:16  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.9  1998/05/28 13:45:49  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.8  1998/04/28 18:29:45  jyh
 * ls() works, adding display.
 *
 * Revision 1.7  1998/04/23 20:03:49  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.6  1998/04/22 22:44:18  jyh
 * *** empty log message ***
 *
 * Revision 1.5  1998/04/22 14:06:23  jyh
 * Implementing proof editor.
 *
 * Revision 1.4  1998/04/17 01:30:46  jyh
 * Editor is almost constructed.
 *
 * Revision 1.3  1998/04/13 21:10:54  jyh
 * Added interactive proofs to filter.
 *
 * Revision 1.2  1998/04/09 19:07:25  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:22  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:32  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:38  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:07  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:23  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
