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
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
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

open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Dform_print
open Dform

open Tactic_type
open Tactic_type.Sequent
open Tactic_type.Tacticals

(*
 * The is the state of the current proof.
 *)
type ped

(*
 * Info for proof-type objects.
 *)
type edit_info =
   { edit_goal : tactic_arg;
     edit_expr : string;
     edit_subgoals : tactic_arg list;
     edit_extras : tactic_arg list
   }

(*
 * Constructors.
 *)
val create : term Filter_type.param list -> tactic_arg -> ped
val ped_of_proof : term Filter_type.param list -> Proof.proof -> ped
val set_params : ped -> term Filter_type.param list -> unit
val set_goal : ped -> msequent -> unit

val edit_info_of_ped : ped -> edit_info

(*
 * Destructors.
 *)
val proof_of_ped : ped -> Proof.proof
val status_of_ped : ped -> Proof.status
val node_count_of_ped : ped -> int * int

(*
 * Refinement, and undo lists.
 * A finite number of undo's are allowed.
 * The (string, Ast.expr, tactic) are all different
 * representations of the same thing.
 *
 * After a refine_ped or nop_ped, the undo stack gets reset.
 * The nop_ped does nothing but reset the undo stack.
 *)
val refine_ped : ped -> string -> MLast.expr -> tactic -> unit
val unfold_ped : ped -> unit
val undo_ped : ped -> unit
val redo_ped : ped -> unit
val nop_ped : ped -> unit
val kreitz_ped : ped -> unit

(*
 * Navigation.
 *)
val up_ped : ped -> int -> unit
val down_ped : ped -> int -> unit
val root_ped : ped -> unit
val addr_ped : ped -> int list -> unit
val rotate_ped : ped -> int -> unit

(*
 * Editing.
 *)
val copy_ped : ped -> string -> unit
val paste_ped : ped -> string -> unit
val cp_ped : ped -> int list -> int list -> unit
val make_assum_ped : ped -> unit
val clean_ped : ped -> unit
val squash_ped : ped -> unit

(*
 * Check the proof and return its extract.
 * Two versions for handling refinement errors:
 *    check_proof: expand until first error, exceptions propagate
 *       On failure, the ped is modified to point to the error
 *    expand_proof: check as much of the proof as possible,
 *       no exceptions are raised
 *)
val check_ped : ped -> term
val expand_ped : dform_base -> ped -> unit

(*
 * Put all the commands into a single argument for interpretation.
 *)
type proof_command =
   ProofRefine of string * MLast.expr * tactic
 | ProofUnfold
 | ProofUndo
 | ProofRedo
 | ProofNop
 | ProofKreitz
 | ProofUp of int
 | ProofDown of int
 | ProofRoot
 | ProofAddr of int list
 | ProofRotate of int
 | ProofCopy of string
 | ProofPaste of string
 | ProofCp of int list * int list
 | ProofExpand of dform_base
 | ProofMakeAssum
 | ProofClean
 | ProofSquash

val interpret : ped -> proof_command -> unit

(*
 * Display.
 *)
type window

type incomplete_ped =
   Primitive of tactic_arg
 | Incomplete of tactic_arg
 | Derived of tactic_arg * MLast.expr

(*
 * Create text or HTML.
 *)
val create_text_window : dform_mode_base -> string -> window
val create_tex_window : dform_mode_base -> window
val create_proof_window : Mux_channel.session -> dform_mode_base -> window

(*
 * Create a new window.
 * On text displays, this does nothing.
 * On graphics displays, this allocates a new
 * window, but may not immediately display it.
 *)
val new_window : window -> window

(*
 * Display the goals.
 *)
val format_incomplete : window -> incomplete_ped -> unit
val format : window -> ped -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
