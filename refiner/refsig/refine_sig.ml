(*
 * The refiner works on proof trees, which are trees of sequents.
 * A basic refinement takes a sequent (a "goal") and produces a
 * list of sequents (the "subgoals"), and an extract term.  The type
 * "tactic" packages refinements, and elements of tactics are
 * always "correct" in the sense that they can be reduced to steps
 * of primitive inferences.
 *
 * The refiner also tracks rewrites, and just as for tactics,
 * elements of type Rewrite are always "correct".
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *
 *)
open Lm_symbol

open Opname

(*
 * Raised when an incomplete proof is manupulated as if it was complete.
 *)
exception Incomplete of opname

 (*
  * A semantical correctness of a proof may depend on
  * a number of axioms refered to in a proof
  *)
type dependency =
   DepDefinition
 | DepCondRewrite
 | DepRewrite
 | DepRule

(************************************************************************
 * REFINER MODULE                                                       *
 ************************************************************************)

module type RefineSig =
sig
   type term
   type address
   type meta_term

   (*
    * Refinements are on meta-sequents,
    * which are a restricted form of meta terms,
    * having only dependent functions format.
    *
    * Each hyp is labelled by its first argument.
    *)
   type msequent

   (*
    * An ML rewrite replaces a term with another.
    * inputs: rule parameters (addrs, terms), goal, subgoal extracts
    *)
   type term_extract = int array -> term list -> term -> term list -> term

   type ml_rewrite = term -> term

   type ml_cond_rewrite =
      SymbolSet.t ->                           (* Free vars in the msequent *)
      term list ->                             (* Params *)
      term ->                                  (* Term to rewrite *)
      term * term list * term_extract          (* Extractor is returned *)

   (*
    * A condition relaces an goal with a list of subgoals,
    * and it provides a function to compute the extract.
    *)
   type ml_rule =
      int array ->                             (* sequent context addresses *)
      msequent ->                              (* goal *)
      term list ->                             (* params *)
      msequent list *  term_extract            (* subgoals, new variable names *)

   (************************************************************************
    * SENTINALS                                                            *
    ************************************************************************)

   (*
    * A checker is used to help make sure that the
    * refiner dependencies are respected.  However,
    * the real dependency checker checks the extract.
    *)
   type sentinal

   (*
    * Empty checker for just trying refinements.
    *)
   val any_sentinal : sentinal

   (*
    * Null sentinal allows no refinements.
    *)
   val null_sentinal : sentinal

   (************************************************************************
    * TACTICS                                                              *
    ************************************************************************)

   (*
    * An extract is an abstract validation that is generated during
    * proof refinement using tactics.
    *)
   type extract

   (*
    * A tactic is the reverse form of validation.
    * given a validation A -> B, that tactic would
    * prove B by asking for a subgoal A.
    *
    * Tactics operate on lists of terms.  These lists
    * represent meta-implications: the head term
    * is the goal, and the remainder are the assumptions.
    *)
   type tactic

   (* Tactic application *)
   val refine : sentinal -> tactic -> msequent -> msequent list * extract

   (* Proof composition *)
   val compose : extract -> extract list -> extract

   (* The base case tactic proves a goal by assumption *)
   val nth_hyp : int -> tactic

   (* The cut rule is primitive, but it doesn't weaken the logic *)
   val cut : term -> tactic

   val identity : sentinal -> msequent -> extract

   (* Returns the list of unproven subgoals *)
   val subgoals_of_extract : extract -> msequent list

   (*
    * For UI purposes only, we want refiner to be able to desribe
    * what it did in a proof step
    *)
   type extract_description =
      EDRule of opname * int list * term list
    | EDRewrite
    | EDCondREwrite
    | EDComposition (* any compilcated steps will fall into this category *)
    | EDNthHyp of int
    | EDCut of term
    | EDIdentity

   val describe_extract : extract -> extract_description

   (************************************************************************
    * REWRITES                                                             *
    ************************************************************************)

   (*
    * A normal rewrite can be applied to a term to rewrite it.
    *)
   type rw

   (*
    * Apply a rewrite to a subterm of the goal.
    *)
   val rwaddr : address -> rw -> rw

   (*
    * Apply the rewrite to the outermost where it does not fail.
    *)
   val rwhigher : rw -> rw

   (*
    * Convert a rewrite that likes to examine its argument.
    *)
   val rwtactic : int -> rw -> tactic

   (*
    * Composition is supplied for efficiency.
    *)
   val andthenrw : rw -> rw -> rw
   val orelserw : rw -> rw -> rw

   (************************************************************************
    * CONDITIONAL REWRITE                                                  *
    ************************************************************************)

   (*
    * A conditional rewrite is a cross between a rewrite and
    * a tactic.  An application may generate subgoals that must
    * be proved.  A conditional rewrite is valid only in a sequent
    * calculus.
    *)
   type cond_rewrite

   (*
    * Ask for the current sequent, and for the term be rewritten.
    *)
   val crwaddr : address -> cond_rewrite -> cond_rewrite

   (*
    * Apply the rewrite to the outermost terms where it does not fail.
    *)
   val crwhigher : cond_rewrite -> cond_rewrite

   (*
    * Application of a conditional rewrite.
    * In this application, the rewrite must be applied to
    * a sequent, and it returns the rewritten sequent
    * as the first subgoal.
    *)
   val crwtactic : int -> cond_rewrite -> tactic

   (*
    * Composition is supplied for efficiency.
    *)
   val candthenrw : cond_rewrite -> cond_rewrite -> cond_rewrite
   val corelserw : cond_rewrite -> cond_rewrite -> cond_rewrite

   (************************************************************************
    * META SEQUENT                                                         *
    ************************************************************************)

   (*
    * Con/de-structors.
    *)
   val mk_msequent : term -> term list -> msequent
   val dest_msequent : msequent -> term * term list
   val msequent_goal : msequent -> term
   val msequent_num_assums : msequent -> int
   val msequent_nth_assum :  msequent -> int -> term
   val msequent_free_vars : msequent -> SymbolSet.t
   val msequent_remove_redundant_hypbindings : msequent -> msequent

   (*
    * Alpha equality on sequent objects.
    *)
   val msequent_alpha_equal : msequent -> msequent -> bool

   (************************************************************************
    * REFINER INTERFACE                                                    *
    ************************************************************************)

   (*
    * A refiner is basically just a collection of rules, tactics,
    * and rewrites.
    *)
   type refiner
   type build

   val null_refiner : string -> build
   val refiner_of_build : build -> refiner

   (*
    * These are the forms created at compile time with
    * extra arguments.
    *)
   type prim_tactic = int array -> term list -> tactic
   type prim_rewrite =
      PrimRW of rw
    | CondRW of (term list -> cond_rewrite)

   (*
    * Get the term corresponding to an extract from the named proof.
    * This will fail if some of the rules are not justified
    * or if the extract is not complete.  The extract terms
    * for the arguments are included.
    *)
   val extract_term : refiner -> opname -> term list -> term
   val compute_dependencies : refiner -> opname -> (dependency * opname) list

   (*
    * Helper function - create an extract arg for an assum for which we do not care about extract
    * int arg is the number of the assum (used to create a unique variable name).
    *)
   val make_wildcard_ext_arg : int -> term -> term

   (*
    * Get a checker from the refiner.
    *)
   val sentinal_of_refiner : refiner -> sentinal

   (*
    * Get a sentinel containing all the items that can be used
    * in a proof od a given object (rule/rewrite). Can raise Not_found
    *)
   val find_sentinal : refiner -> opname -> sentinal

   (*
    * A rule is an implication on terms (the conclusion
    * is true if all the antecedents are).
    *     Args: refiner, name, addrs, params, rule
    *
    * Once a rule is defined, it can be justified as
    *    1. primitive (an term extract is given)
    *    2. derived (an extract from a proof is given)
    *    3. delayed (an extract can be computed on request)
    *)
   val create_rule : build ->
      string ->            (* name *)
      var array ->         (* addrs *)
      term list ->         (* params *)
      meta_term ->         (* rule definition *)
      prim_tactic
   val create_ml_rule : build -> string ->
      ml_rule ->           (* the rule definition *)
      prim_tactic
   val check_rule :
      string ->            (* name *)
      var array ->         (* addrs *)
      term list ->         (* params *)
      meta_term ->         (* rule definition *)
      unit
   val check_prim_rule :
      string ->            (* name *)
      var array ->         (* addrs *)
      term list ->         (* params *)
      meta_term ->         (* rule definition *)
      term list ->         (* extract args (bindings) *)
      term ->              (* extract *)
      unit

   val prim_rule : build ->
      string ->                    (* name *)
      var array ->                 (* addrs *)
      term list ->                 (* params *)
      meta_term ->                 (* rule definition *)
      term list ->                 (* extract args (bindings) *)
      term ->                      (* extract *)
      unit
   val derived_rule : build ->
      string ->                    (* name *)
      var array ->                 (* addrs *)
      term list ->                 (* params *)
      meta_term ->                 (* rule definition *)
      unit ->                      (* placeholder *)
      extract ->                   (* derived justification *)
      unit
   val delayed_rule : build ->
      string ->                    (* name *)
      var array ->                 (* addrs *)
      term list ->                 (* params *)
      meta_term ->                 (* rule definition *)
      unit ->                      (* placeholder *)
      (unit -> extract) ->         (* derived justification *)
      unit

   (*
    * Rewrites.
    *)
   val create_rewrite : build ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      prim_rewrite
   val create_input_form : build ->
      string ->            (* name *)
      bool ->              (* relaxed/strict *)
      term ->              (* redex *)
      term ->              (* contractum *)
      prim_rewrite
   val create_ml_rewrite : build -> string ->
      ml_rewrite ->        (* rewriter *)
      prim_rewrite
   val prim_rewrite : build ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      unit
   val definitional_rewrite : build ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      unit
   val derived_rewrite : build ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      extract ->           (* proof *)
      unit
   val delayed_rewrite : build ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      (unit -> extract) -> (* proof *)
      unit

   val create_cond_rewrite : build ->
      string ->            (* name *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      prim_rewrite
   val create_ml_cond_rewrite : build -> string ->
      ml_cond_rewrite ->   (* rewriter *)
      prim_rewrite
   val prim_cond_rewrite : build ->
      string ->            (* name *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      unit
   val derived_cond_rewrite : build ->
      string ->            (* name *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      extract ->           (* proof *)
      unit
   val delayed_cond_rewrite : build ->
      string ->            (* name *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      (unit -> extract) -> (* proof *)
      unit
   val check_rewrite :
      string ->            (* string *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      unit

   (*
    * HACK: truning rewrites from meta-terms into sequents,
    *)
   val mk_rewrite_hack : term -> term

   (*
    * Merge refiners.
    *)
   val label_refiner : build -> string -> refiner
   val join_refiner : build -> refiner -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
