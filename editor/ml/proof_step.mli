(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
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

include Tacticals
include Io_proof_type
include Proof_type

open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Dform

open Sequent
open Tacticals

open Io_proof_type
open Proof_type

(* Abstract type for steps *)
type t

(* Constructor *)
val create :
   tactic_arg ->            (* Goal *)
   tactic_arg list ->       (* Subgoals *)
   string ->                (* Text in rule box *)
   MLast.expr ->            (* Parsed ML expression *)
   tactic ->                (* Corresponding tactic *)
   t

(* Destructors *)
val goal : t -> tactic_arg
val subgoals : t -> tactic_arg list
val text : t -> string
val ast : t -> MLast.expr
val tactic : t -> tactic

(*
 * Check the tactic in a particular refiner.
 *   check: raises RefineError if the refinement changes
 *   expand: allow arbitrary changes in the refinement
 *)
val check : t -> extract
val expand : dform_base -> t -> t

(* IO *)
type 'a norm
type 'a denorm

val create_denorm : (term -> 'a) -> 'a denorm
val create_norm :
   ('a -> term) ->                      (* normalizer *)
   tactic_argument ->                   (* Default attributes *)
   (string -> MLast.expr) ->            (* Parser *)
   (MLast.expr -> tactic) ->            (* Tactic compiler *)
   Tactic_type.sentinal ->              (* Sentinal to be used in the proof *)
   'a norm

val io_step_of_step : 'a denorm -> t -> 'a proof_step
val step_of_io_step : 'a norm -> 'a proof_step -> t

(*
 * Other helper functions.
 *)
val tactic_arg_of_aterm : 'a norm -> 'a aterm -> tactic_arg
val raw_attributes_of_attributes : 'a norm ->
   'a Tactic_type.attributes ->
   Tactic_type.raw_attributes

val aterm_of_tactic_arg : 'a denorm -> tactic_arg -> 'a aterm
val attributes_of_term_attributes : 'a denorm ->
   term Tactic_type.attributes ->
   'a Tactic_type.attributes

(* Debug *)
val debug_io_tactic : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
