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
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

include Io_proof_type
include Proof_type
include Tacticals

include Proof_step

open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Dform

open Sequent
open Tacticals

open Io_proof_type
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
exception ProofRefineError of t * string * refine_error

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
val extras : t -> t list

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
type io_proof = Refiner_std_verb.Refiner.TermType.term proof

val io_proof_of_proof : t -> io_proof
val proof_of_io_proof : tactic_argument -> (MLast.expr -> tactic) -> Tactic_type.sentinal -> io_proof -> t

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
