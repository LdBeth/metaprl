(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
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
