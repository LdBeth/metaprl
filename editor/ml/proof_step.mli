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
   (string, tactic) Hashtbl.t ->        (* Table of compiled tactics *)
   sentinal ->                          (* Sentinal to be used in the proof *)
   'a norm

val io_step_of_step : 'a denorm -> t -> 'a proof_step
val step_of_io_step : 'a norm -> 'a proof_step -> t

(*
 * Other helper functions.
 *)
val tactic_arg_of_aterm : 'a norm -> 'a aterm -> tactic_arg
val term_attributes_of_attributes : 'a norm ->
   'a Tactic_type.attributes ->
   term Tactic_type.attributes

val aterm_of_tactic_arg : 'a denorm -> tactic_arg -> 'a aterm
val attributes_of_term_attributes : 'a denorm ->
   term Tactic_type.attributes ->
   'a Tactic_type.attributes

(* Debug *)
val debug_io_tactic : bool ref

(*
 * $Log$
 * Revision 1.13  1998/07/03 22:05:20  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.12  1998/07/02 18:34:38  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.11  1998/06/09 20:51:18  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 * Revision 1.10  1998/06/01 13:52:24  jyh
 * Proving twice one is two.
 *
 * Revision 1.9  1998/05/28 13:45:55  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.8  1998/04/28 18:29:51  jyh
 * ls() works, adding display.
 *
 * Revision 1.7  1998/04/23 20:04:00  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.6  1998/04/22 22:44:21  jyh
 * *** empty log message ***
 *
 * Revision 1.5  1998/04/22 14:06:26  jyh
 * Implementing proof editor.
 *
 * Revision 1.4  1998/04/17 01:30:48  jyh
 * Editor is almost constructed.
 *
 * Revision 1.3  1998/04/13 21:10:57  jyh
 * Added interactive proofs to filter.
 *
 * Revision 1.2  1998/04/09 19:07:29  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:25  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:39  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:43  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:11  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:27  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
