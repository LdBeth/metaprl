(*
 * Define the proof type used by the tactics.
 * The extract type generalizes the Refine.extract
 * type so that we can have partial extract object
 * with annotations for tactic applications.
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
 * Copyright (C) 1998,1999 Jason Hickey, Cornell University
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
 *)
open Lm_symbol

open Refiner.Refiner
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError

open Refiner_sig
open Refiner_io
open Mp_resource

open Dform

(*
 * Visible types.
 *)
module type TacticTypeSig =
sig
   (*
    * These are the different types of normal arguments
    * we can pass with the tactic.
    *)
   type attribute =
      TermArg of term
    | TypeArg of term
    | IntArg of int
    | BoolArg of bool
    | StringArg of string
    | TermListArg of term list

   type attributes = (string * attribute) list

   (*
    * For efficiency, we expand a few attribute lists,
    * but we retain a general argument description.
    *)
   type arglist =
      NoneArgList               of string
    | IntArgList                of string * int
    | BoolArgList               of string * bool
    | StringArgList             of string * string
    | TermArgList               of string * term
    | IntIntArgList             of string * int * int
    | IntBoolArgList            of string * int * bool
    | IntStringArgList          of string * int * string
    | IntTermArgList            of string * int * term
    | BoolIntArgList            of string * bool * int
    | BoolBoolArgList           of string * bool * bool
    | BoolStringArgList         of string * bool * string
    | BoolTermArgList           of string * bool * term
    | StringIntArgList          of string * string * int
    | StringBoolArgList         of string * string * bool
    | StringStringArgList       of string * string * string
    | StringTermArgList         of string * string * term
    | TermIntArgList            of string * term * int
    | TermBoolArgList           of string * term * bool
    | TermStringArgList         of string * term * string
    | TermTermArgList           of string * term * term
    | GeneralArgList            of attribute array
end

(*
 * Internal type definitions.
 *)
module type TacticInternalTypeSig =
sig
   (*
    * The attribute calculations are delayed to minimize communication
    * cost.  The tactic_arg uses keys to distribute the attributes.
    * The values are stored in keys.
    *)
   type sentinal
   and attribute
   and attributes = (string * attribute) list
   and raw_attribute
   and raw_attributes = raw_attribute list
   and arglist
   and tactic
   and attribute_info

   (*
    * A tactic argument includes the msequent goal,
    * as well as the attributes.  The parents is mutable,
    * but only because it is computed lazily.
    *)
   and tactic_arg =
      { ref_goal : msequent;
        ref_label : string;
        mutable ref_parent : tactic_parent;
        ref_attributes : attribute_info;
        ref_bookmark : global_resource;
        ref_sentinal : sentinal
      }

   and tactic_parent =
      ParentNone
    | ParentLazy of tactic_arg
    | ParentSet of tactic_arg * parents

   and parents

   (*
    * An extract has these kinds:
    *   + A goal term without any justification
    *   + A step that is unjustified
    *   + A raw refine extract, saving the number of subgoals
    *   + A composition of extracts
    *   + An annotated extract
    *   + A rule box, which is a combined annotation/composition
    *)
   and extract =
      Goal of tactic_arg
    | Unjustified of tactic_arg * tactic_arg list
    | Extract of tactic_arg * tactic_arg list * Refine.extract
    | Wrapped of arglist * extract
    | Compose of compose_info
    | RuleBox of rule_info
    | Pending of pending_extract
    | Locked of extract
    | Identity of tactic_arg

   and pending_extract = unit -> extract

   and compose_info =
      { mutable comp_status : lazy_status;
        comp_goal : extract;
        comp_subgoals : extract list;
        mutable comp_leaves : lazy_leaves;
        comp_extras : extract list
      }

   and rule_info =
      { mutable rule_status : lazy_status;
        rule_string : string;
        rule_expr : (unit -> MLast.expr);
        rule_tactic : (unit -> tactic);
        mutable rule_extract_normalized : bool;
        mutable rule_extract : extract;
        rule_subgoals : extract list;
        mutable rule_leaves : lazy_leaves;
        rule_extras : extract list
      }

   and lazy_status =
      LazyStatusDelayed
    | LazyStatusBad
    | LazyStatusIncomplete
    | LazyStatusPartial
    | LazyStatusComplete

   and lazy_leaves =
      LazyLeavesDelayed
    | LazyLeaves of tactic_arg list

   (*
    * Conversions are used by the rewrite module.
    *)
   and env = tactic_arg * int * address

   and conv =
      RewriteConv of rw
    | CondRewriteConv of cond_rewrite
    | ComposeConv of conv Flist.t
    | ChooseConv of conv Flist.t
    | AddressConv of address * conv
    | FoldConv of term * conv
    | CutConv of term
    | FunConv of (env -> conv)
    | HigherConv of conv
    | IdentityConv

   module ParentTable
   : Lm_map_sig.TableSig
     with type t = parents
     with type elt = msequent
     with type data = tactic_arg
end

(*
 * Exported tactic module.
 *)
module type TacticSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Here are all the different type of tactics.
    *    1. A tactic_arg contains all the info about the argument
    *    2. An extract contains the info to produce a Refine.extract
    *    3. A pre_tactic is some precompiled info used to construct a tactic.
    *    4. A tactic is a refinement
    *)
   type tactic_arg
   type tactic
   type pre_tactic
   type sentinal
   type conv
   type attribute
   type attributes = (string * attribute) list
   type raw_attribute
   type raw_attributes = raw_attribute list
   type arglist

   (************************************************************************
    * OPERATIONS                                                           *
    ************************************************************************)

   (*
    * Build an initial argument for a proof.
    *)
   val create :
      sentinal -> msequent -> global_resource -> tactic_arg

   (*
    * Conversion between general forms and optimized forms.
    *)
   val compress_arglist : attribute list -> arglist
   val expand_arglist : arglist -> attribute list

   (*
    * Start the main loop.
    *)
   val args : unit -> (string * Arg.spec * string) list
   val main_loop : unit -> unit

   (*
    * Sentinals are computed by naming the module and
    * rule for the sentinal.
    *)
   val sentinal_of_refiner : string -> sentinal
   val sentinal_of_refiner_object : string -> string -> sentinal
   val null_sentinal : sentinal

   (*
    * Attribute creation.
    *)
   val attributes : tactic_arg -> attributes
   val raw_attributes : tactic_arg -> raw_attributes

   val term_attribute : string -> term -> raw_attribute
   val term_list_attribute : string -> term list -> raw_attribute
   val type_attribute : string -> term -> raw_attribute
   val int_attribute : string -> int -> raw_attribute
   val bool_attribute : string -> bool -> raw_attribute
   val string_attribute : string -> string -> raw_attribute

   (*
    * Lift refiner tactics into our tactic type.
    * These functions are required by the Filter_prog module.
    *)
   val compile_rule : build -> string option list -> prim_tactic -> pre_tactic
   val compile_labeled_rule : build -> prim_tactic -> pre_tactic
   val tactic_of_rule : pre_tactic -> int array -> term list -> tactic

   (*
    * Also convert rewrites.
    *)
   val tactic_of_rewrite : int -> rw -> tactic
   val tactic_of_cond_rewrite : int -> cond_rewrite -> tactic
end

(*
 * Extra functions in the Tactic module.
 *)
module type TacticInternalSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type tactic_arg
   type tactic
   type extract
   type conv
   type attribute_info
   type attribute
   type arglist
   type raw_attribute
   type raw_attributes = raw_attribute list

   (************************************************************************
    * OPERATIONS                                                           *
    ************************************************************************)

   (*
    * Construct attributes.
    *)
   val attribute_info_of_raw_attributes : raw_attributes -> attribute_info
   val update_attributes : attribute_info -> raw_attributes -> attribute_info

   val empty_attribute : attribute_info (* No attribites *)
   val squash_attributes : attribute_info -> attribute_info (* Squash away raw attributes, but keep the rest *)

   (********************************
    * PRINTING
    *)

   (* Formatting functions *)
   val format_extract : Dform.dform_base -> Rformat.buffer -> extract -> unit
   val format_arg : Dform.dform_base -> Rformat.buffer -> tactic_arg -> unit
   val format_attrs : Dform.dform_base -> Rformat.buffer -> attribute_info -> unit

   (*
    * Two tactic_arguments are equal when they have
    * equal msequent parts.  Their labels, etc, are
    * not compared.
    *)
   val tactic_arg_alpha_equal : tactic_arg -> tactic_arg -> bool
   val tactic_arg_alpha_equal_concl : tactic_arg -> tactic_arg -> bool

   (*
    * Apply a tactic.
    *)
   val refine : tactic -> tactic_arg -> tactic_arg list * extract
   val compose : extract -> extract list -> extract

   (*
    * Access to the argument.
    *)
   val goal        : tactic_arg -> term
   val msequent    : tactic_arg -> msequent
   val nth_hyp     : tactic_arg -> int -> term
   val nth_binding : tactic_arg -> int -> var
   val num_assums  : tactic_arg -> int
   val nth_assum   : tactic_arg -> int -> term
   val nth_concl   : tactic_arg -> int -> term
   val label       : tactic_arg -> string

   (*
    * Modification of the argument.
    * These are functional.
    *)
   val set_goal    : tactic_arg -> term -> tactic_arg
   val set_concl   : tactic_arg -> term -> tactic_arg
   val set_label   : tactic_arg -> string -> tactic_arg

   (*
    * Fetch attributes.
    *)
   val get_term      : tactic_arg -> string -> term
   val get_term_list : tactic_arg -> string -> term list
   val get_type      : tactic_arg -> string -> term
   val get_int       : tactic_arg -> string -> int
   val get_bool      : tactic_arg -> string -> bool
   val get_string    : tactic_arg -> string -> string
   val get_resource  : tactic_arg -> (global_resource -> 'a) -> 'a

   (*
    * Basic tactics.
    *)
   val idT : tactic
   val nthAssumT : int -> tactic
   val cutT : term -> tactic

   (* Wrap a tactic selection function *)
   val funT : (tactic_arg -> tactic) -> tactic
   val argfunT : ('a -> tactic_arg -> tactic) -> 'a -> tactic

   (*
    * Basic tacticals.
    *)
   val prefix_thenT : tactic -> tactic -> tactic
   val prefix_thenLT : tactic -> tactic list -> tactic
   val prefix_thenFLT : tactic -> (tactic_arg list -> tactic list) -> tactic
   val prefix_orelseT : tactic -> tactic -> tactic

   (*
    * Argument management.
    *)
   val setLabelT : string -> tactic
   val withTermT : string -> term -> tactic -> tactic
   val withTermListT : string -> term list -> tactic -> tactic
   val withTypeT : string -> term -> tactic -> tactic
   val withBoolT : string -> bool -> tactic -> tactic
   val withIntT : string -> int -> tactic -> tactic
   val withStringT : string -> string -> tactic -> tactic
   val wrapT : arglist -> tactic -> tactic

   (*
    * Print timing information.
    *)
   val timingT : tactic -> tactic
   val finalT : (unit -> unit) -> tactic

   (*
    * For debugging.
    *)
   val debug_arg : global_resource -> term -> tactic_arg
end

(*
 * This is an editing interface for extracts.
 *)
module type ProofSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Proof is a pointer into an extract.
    *)
   type extract
   type proof
   type tactic_arg
   type tactic
   type sentinal
   type attribute
   type attributes = (string * attribute) list
   type raw_attribute
   type raw_attributes = raw_attribute list
   type arglist

   type status =
      StatusBad
    | StatusIncomplete
    | StatusPartial
    | StatusComplete

   (*
    * An address is a integer path.
    *)
   type address = int list

   (*
    * Description of the refinement.
    *)
   type step_expr =
      ExprGoal
    | ExprIdentity
    | ExprUnjustified
    | ExprExtract of arglist
    | ExprCompose of step_expr
    | ExprWrapped of arglist
    | ExprRule of string * MLast.expr

   (*
    * Info about a step of the proof.
    * The proof in each list have the same goals.
    *)
   type step_info =
      { step_goal : proof list;
        step_expr : step_expr;
        step_subgoals : proof list list;
        step_extras : proof list
      }

   (*
    * This is the function that gets called when
    * a proof is changed.
    *)
   type update_fun = proof -> proof

   (*
    * We overload the refinement error to give the exact location of
    * the error.
    *)
   exception ExtRefineError of string * extract * refine_error
   exception ProofRefineError of string * proof * refine_error

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Make sure all the compositions are left-assotiatoive.
    * Remove unnecessary identities.
    *)
   val normalize : extract -> extract

   (********************************
    * CONSTRUCTION
    * Gets extract, normalizes it and creates a proof object
    *)
   val create : tactic_arg -> proof

   (********************************
    * DESTRUCTION
    *)

   (*
    * The proof may be just a goal term,
    * or it may have subgoals too.
    *)
   val is_leaf : proof -> bool

   (*
    * Get the goal term of the current proof.
    * Always succeeds.
    *)
   val goal : proof -> tactic_arg

   (*
    * Get the complete info for the current proof.
    * This will retirn a normalized info or raise ProofRefineError if extract is atomic.
    *)
   val info : proof -> step_info

   (*
    * Return the status of the current step of the proof.
    *)
   val status : proof -> status

   (*
    * Return the total number of rule boxes and nodes.
    *)
   val node_count : proof -> int * int

   (*
    * Check for possible proofs in the cache.
    *)
   val get_cache : proof -> proof list

   (********************************
    * NAVIGATION
    *)

   (*
    * Return the absolute address of the current step.
    *)
   val address : proof -> address

   (*
    * Return the status line, which is a combination
    * if address and status of the proof along every
    * step of the address.
    *)
   val path_status : proof -> status list

   (*
    * Addressed subgoal.  The address is the
    * path to the subgoal -- raises ProofRefineError
    * at the longest address if the address is bad.
    *)
   val index : proof -> address -> proof
   val child : proof -> int -> proof

   (*
    * Go up the proof.
    * This is the same as removing the last index from the
    * current address.
    *)
   val parent : proof -> proof

   (*
    * Return the outermost proof step.
    *)
   val root : proof -> proof

   (********************************
    * MODIFICATION
    *)

   (*
    * Post a new proof in the cache so it will be picked up by the
    * other navigators.
    *)
   val post : (unit -> proof) -> proof

   (*
    * Set the goal of the proof.
    *)
   val set_goal : update_fun -> proof -> msequent -> proof

   (*
    * Copy a proof node from one location to another.
    *)
   val copy : update_fun -> proof -> address -> address -> proof

   (*
    * Paste a proof in at the current location.
    *)
   val paste : update_fun -> proof -> proof -> proof

   (*
    * Make the current goal an assumption of the entire proof.
    *)
   val make_assum : update_fun -> proof -> proof

   (*
    * Refine the current proof.
    *)
   val refine : update_fun -> proof -> string -> MLast.expr -> tactic -> proof

   (*
    * Remove all extras from the current proof.
    *)
   val clean : update_fun -> proof -> proof

   (*
    * Remove all extracts from the proof.
    *)
   val squash : update_fun -> proof -> proof

   (*
    * "Kreitz" a proof:
    * Take the entire subtree and reduce it to a single node with
    * a sequence of tactics composed with thenLT.
    *)
   val kreitz : update_fun -> proof -> proof

   val format_proof : Dform.dform_base -> Rformat.buffer -> proof -> unit

   (********************************
    * EXPANSION
    *)

   (*
    * Reapply all the tactics in the proof, adjusting the
    * status as appropriate.  This function never fails.
    *)
   val expand : update_fun -> dform_base -> proof -> proof

   (*
    * Get the low-level Refiner format of the proof
    *)
   val refiner_extract_of_proof : proof -> Refine.extract

   (********************************
    * I/O proofs are preserved for compatibility.
    *)
   type io_proof

   val proof_of_io_proof :
      raw_attributes ->            (* Default attributes *)
      sentinal ->                  (* Proof checker *)
      global_resource ->           (* Resource bookmark *)
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      io_proof ->
      proof

   val io_proof_of_proof :
      bool ->                      (* Whether to squash away the primitive proof tree *)
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      proof ->
      io_proof

   val term_of_io_proof :
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      io_proof ->
      term

   val io_proof_of_term :
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      term ->
      io_proof

   val term_io_of_io_proof :
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      io_proof ->
      Refiner_io.TermType.term

   val io_proof_of_term_io :
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      Refiner_io.TermType.term ->
      io_proof

   (*
    * Some basic operations on io_proofs are allowed.
    *)
   val status_of_io_proof : io_proof -> status
   val node_count_of_io_proof : io_proof -> int * int

   (********************************
    * Conversion to terms.
    *)
   module ProofTerm (ToTerm : RefinerSig) :
   sig
      val to_term :
         (string -> MLast.expr) ->    (* Parser *)
         (MLast.expr -> tactic) ->    (* Evaluator *)
         proof ->
         ToTerm.TermType.term

      val of_term :
         raw_attributes ->            (* Default attributes *)
         sentinal ->                  (* Proof checker *)
         global_resource ->           (* Resource bookmark *)
         (string -> MLast.expr) ->    (* Parser *)
         (MLast.expr -> tactic) ->    (* Evaluator *)
         ToTerm.TermType.term ->      (* Term to parse *)
         proof
   end
end

(*
 * Additional operations useful in logics with sequents.
 *)
module type SequentSig =
sig
   (*
    * Types.
    *)
   type extract
   type conv
   type tactic
   type tactic_arg
   type sentinal
   type raw_attribute

   (*
    * Two tactic_arguments are equal when they have
    * equal msequent parts.  Their labels, etc, are
    * not compared.
    *)
   val tactic_arg_alpha_equal : tactic_arg -> tactic_arg -> bool
   val tactic_arg_alpha_equal_concl : tactic_arg -> tactic_arg -> bool

   (* All numbering in hypotheses and assumptions starts with 1 *)

   (*
    * Get the address of a part of the sequent.
    *)
   val clause_addr : tactic_arg -> int -> address
   val assum_clause_addr : tactic_arg -> int -> int -> address
   val get_decl_number : tactic_arg -> var -> int
   val hyp_count : tactic_arg -> int
   val assum_hyp_count : tactic_arg -> int -> int
   val get_pos_hyp_num : tactic_arg -> int -> int

   (*
    * Get the parts of the argument.
    *)
   val goal : tactic_arg -> term
   val msequent : tactic_arg -> msequent
   val args : tactic_arg -> term list
   val concl : tactic_arg -> term
   val num_assums : tactic_arg -> int
   val nth_hyp : tactic_arg -> int -> term
   val nth_binding : tactic_arg -> int -> var
   val nth_assum : tactic_arg -> int -> term
   val label : tactic_arg -> string

   (*
    * Get info about the sequent.
    *)
   val explode_sequent : tactic_arg -> TermType.esequent

   (*
    * Variables to avoid clashing with
    * This function is informal and does not have a clear semantics
    *)
   val avoid_vars : tactic_arg -> SymbolSet.t

   (*
    * Argument functions.
    *)
   val get_term_arg       : tactic_arg -> string -> term
   val get_term_list_arg  : tactic_arg -> string -> term list
   val get_type_arg       : tactic_arg -> string -> term
   val get_int_arg        : tactic_arg -> string -> int
   val get_bool_arg       : tactic_arg -> string -> bool
   val get_string_arg     : tactic_arg -> string -> string
   val get_resource_arg   : tactic_arg -> (global_resource -> 'a) -> 'a
end

(*
 * Additonal tactic operations.
 *)
module type TacticalsSig =
sig
   type tactic
   type tactic_arg
   type arglist
   type extract

   (* Trivial tactics *)
   val idT : tactic
   val failT : tactic
   val failWithT : string -> tactic
   val nthAssumT : int -> tactic
   val cutT : term -> tactic

   (* Wrap a tactic selection function *)
   val funT : (tactic_arg -> tactic) -> tactic
   val argfunT : ('a -> tactic_arg -> tactic) -> 'a -> tactic

   (* Print timing information *)
   val timingT : tactic -> tactic
   val finalT : (unit -> unit) -> tactic

   (* Allow tactic only if no subgoals *)
   val completeT : tactic -> tactic

   val progressT : tactic -> tactic

   (*
    * Repeatedly apply the tactic
    *)
   val whileProgressT : tactic -> tactic
   val untilFailT : tactic -> tactic
   val repeatT : tactic -> tactic
   val repeatForT : int -> tactic -> tactic
   val seqOnSameConclT : tactic list -> tactic

   (* Sequencing *)
   val prefix_orelseT : tactic -> tactic -> tactic
   val prefix_andalsoT : tactic -> tactic -> tactic
   val prefix_orthenT : tactic -> tactic -> tactic
   val firstT : tactic list -> tactic
   val tryT : tactic -> tactic

   val prefix_thenT : tactic -> tactic -> tactic
   val prefix_thenLT : tactic -> tactic list -> tactic
   val prefix_thenFLT : tactic -> (tactic_arg list -> tactic list) -> tactic
   val prefix_then_OnFirstT : tactic -> tactic -> tactic
   val prefix_then_OnLastT : tactic -> tactic -> tactic
   val prefix_then_OnSameConclT : tactic -> tactic -> tactic

   (*
    * Conditionals.
    *)
   val ifT : (tactic_arg -> bool) -> tactic -> tactic -> tactic
   val ifOnConclT : (term -> bool) -> tactic -> tactic -> tactic
   val ifOnHypT : (term -> bool) -> (int -> tactic) -> (int -> tactic) -> int -> tactic
   val ifThenT : (term -> bool) -> tactic -> tactic
   val ifThenOnConclT : (term -> bool) -> tactic -> tactic
   val ifThenOnHypT : (term -> bool) -> (int -> tactic) -> int -> tactic

   val whileT : (term -> bool) -> tactic -> tactic
   val untilT : (term -> bool) -> tactic -> tactic

   (*
    * Label tactics.
    *)
   val main_labels : string list
   val predicate_labels : string list

   val addHiddenLabelT : string -> tactic
   val removeHiddenLabelT : tactic
   val keepingLabelT : tactic -> tactic
   val ifLabLT : (string * tactic) list -> tactic
   val ifLabT : string -> tactic -> tactic -> tactic

   (*
    * Label tacticals.
    *)
   val prefix_thenLabLT : tactic -> (string * tactic) list -> tactic
   val prefix_thenMT : tactic -> tactic -> tactic
   val prefix_thenMLT : tactic -> tactic list -> tactic
   val prefix_thenAT : tactic -> tactic -> tactic
   val prefix_thenALT : tactic -> tactic list -> tactic
   val prefix_thenWT : tactic -> tactic -> tactic
   val prefix_thenET : tactic -> tactic -> tactic
   val prefix_thenPT : tactic -> tactic -> tactic

   val repeatMT : tactic -> tactic
   val repeatMForT : int -> tactic -> tactic
   val whileProgressMT : tactic -> tactic
   val untilFailMT : tactic -> tactic
   val seqOnMT : tactic list -> tactic
   val seqT : tactic list -> tactic
   val completeMT : tactic -> tactic
   val labProgressT : tactic -> tactic

   (*
    * Hyp and Clausal tactics.
    *)
   val onClauseT : int -> (int -> tactic) -> tactic
   val onHypT : int -> (int -> tactic) -> tactic
   val onConclT : (int -> tactic) -> tactic

   val onClausesT : int list -> (int -> tactic) -> tactic
   val onHypsT : int list -> (int -> tactic) -> tactic
   val tryOnClausesT : int list -> (int -> tactic) -> tactic
   val tryOnHypsT : int list -> (int -> tactic) -> tactic

   val onMClausesT : int list -> (int -> tactic) -> tactic
   val onMHypsT : int list -> (int -> tactic) -> tactic

   val onAllHypsT : (int -> tactic) -> tactic
   val onAllClausesT : (int -> tactic) -> tactic
   val tryOnAllHypsT : (int -> tactic) -> tactic
   val tryOnAllClausesT : (int -> tactic) -> tactic

   val onAllMHypsT : (int -> tactic) -> tactic
   val onAllMClausesOfAssumT : (int -> int -> tactic) -> int -> tactic
   val tryOnAllMHypsT : (int -> tactic) -> tactic
   val tryOnAllMClausesT : (int -> tactic) -> tactic
   val onAllAssumT : (int -> tactic) -> tactic
   val onAllMAssumT : (int -> tactic) -> tactic

   val onSomeAssumT : (int -> tactic) -> tactic
   val onSomeHypT : (int -> tactic) -> tactic

   (*
    * Proof annotation.
    *)
   val wrapT : arglist -> tactic -> tactic

   (*
    * General argument functions.
    *)
   val withTermT : string -> term -> tactic -> tactic
   val withTermListT : string -> term list -> tactic -> tactic
   val withTypeT : string -> term -> tactic -> tactic
   val withBoolT : string -> bool -> tactic -> tactic
   val withStringT : string -> string -> tactic -> tactic
   val withIntT : string -> int -> tactic -> tactic

   (*
    * Specific argument functions.
    *)
   val withT : term -> tactic -> tactic
   val withTermsT : term list -> tactic -> tactic
   val atT : term -> tactic -> tactic
   val selT : int -> tactic -> tactic
   val altT : tactic -> tactic
   val thinningT : bool -> tactic -> tactic

   val get_with_arg : tactic_arg -> term
   val get_with_args : tactic_arg -> term list
   val get_univ_arg : tactic_arg -> term
   val get_sel_arg : tactic_arg -> int
   val get_thinning_arg : tactic_arg -> bool
   val get_alt_arg : tactic_arg -> bool
end

(*
 * Rewriter operations.
 *)
module type RewriteSig =
sig
   type env
   type conv
   type tactic
   type tactic_arg

   (*
    * Create a conversion from a basic rewrite.
    * This function is required by filter_prog.
    *)
   val rewrite_of_pre_rewrite : prim_rewrite -> term list -> conv

   (*
    * Standard rewrite annotatetion processor: return a pair of the redex and the conv
    *)
   val redex_and_conv_of_rw_annotation: string -> (prim_rewrite, term * conv) Mp_resource.rw_annotation_processor
end

module type RewriteInternalSig =
sig
   type env
   type conv
   type tactic
   type tactic_arg

   (*
    * Operations on the environment mirror operations from Sequent.
    *)
   val env_term : env -> term
   val env_arg : env -> tactic_arg

   (*
    * Apply a rewrite.
    *)
   val rw : conv -> int -> address -> tactic

   (*
    * Sequencing.
    *)
   val prefix_thenC : conv -> conv -> conv
   val prefix_orelseC : conv -> conv -> conv

   (*
    * Identity.
    *)
   val idC : conv

   (*
    * Pull out the argument.
    *)
   val funC : (env -> conv) -> conv

   val termC : (term -> conv) -> conv

   (*
    * Apply a conversion at an address.
    *)
   val addrC : int list -> conv -> conv
   val addrLiteralC : address -> conv -> conv

   (*
    * Apply a conversion at the outermost terms where it does not fail.
    *)
   val higherC : conv -> conv

   (*
    * Two versions of cut.
    * foldC t conv: cuts in the new term t, and uses conv to
    *    solve the resulting goal.
    * cutC t: just cuts in the new goal
    *)
   val foldC : term -> conv -> conv
   val cutC : term -> conv

   (*
    * Create a fold operation automatically.
    *)
   val makeFoldC : term -> conv -> conv

   (* Subterm application. *)
   val allSubC : conv -> conv

   (*
    * Informal rewriting.
    * Create an input form.  This is a rewrite with
    * no justification.
    *)
   val create_iform : string -> bool -> term -> term -> conv

   (*
    * Debugging.
    *)
   val apply_rewrite : Mp_resource.bookmark -> conv -> term -> term
end

(*
 * Abstraction layer over rewrite_type.
 *)
module type ConversionalsSig =
sig
   type env
   type conv
   type tactic_arg
   type tactic

   (*
    * Environment.
    *)
   val env_term : env -> term
   val env_arg : env -> tactic_arg

   (*
    * All rewrites are wrapped by the rewrite function.
    * The argument is the hyp number, or concl to apply to.
    *)
   val rw : conv -> int -> tactic
   val rwc : conv -> int -> int -> tactic
   val rwAll : conv -> tactic
   val rwcAll : conv -> int -> tactic
   val rwAllAll : conv -> tactic

   val rwh : conv -> int -> tactic
   val rwch : conv -> int -> int -> tactic
   val rwhAll : conv -> tactic
   val rwchAll : conv -> int -> tactic
   val rwhAllAll : conv -> tactic

   val rwa : conv list -> int -> tactic
   val rwca : conv list -> int -> int -> tactic
   val rwaAll : conv list -> tactic
   val rwcaAll : conv list -> int -> tactic
   val rwaAllAll : conv list -> tactic

   val prefix_thenC : conv -> conv -> conv
   val prefix_orelseC : conv -> conv -> conv
   val addrC : int list -> conv -> conv
   val idC : conv
   val foldC : term -> conv -> conv
   val makeFoldC : term -> conv -> conv
   val cutC : term -> conv
   val funC : (env -> conv) -> conv
   val termC : (term -> conv) -> conv

   (************************************************************************
    * SEARCH                                                               *
    ************************************************************************)

   (*
    * Fail with a message.
    *)
   val failC : conv
   val failWithC : string -> conv

   (*
    * Try a conversion.
    *)
   val tryC : conv -> conv

   (*
    * Subterm application.
    *)
   val someSubC : conv -> conv
   val allSubC : conv -> conv

   (*
    * First term, leftmost, outermost.
    *)
   val higherC : conv -> conv

   (*
    * First term, leftmost, innermost.
    *)
   val lowerC : conv -> conv

   (*
    * Sweep the rewrite up from the leaves to the root.
    *)
   val sweepUpC : conv -> conv

   (*
    * Sweep down from the root to the leaves.
    *)
   val sweepDnC : conv -> conv

   (*
    * Same, but don't allow failures.
    *)
   val sweepUpFailC : conv -> conv
   val sweepDnFailC : conv -> conv

   (*
    * Use the first conversion that works.
    *)
   val firstC : conv list -> conv

   val applyAllC : conv list -> conv

   (*
    * Repeat the conversion until nothing more happens.
    *)

   val whileProgressC : conv -> conv
   val untilFailC : conv -> conv
   val repeatC : conv -> conv
   val repeatForC : int -> conv -> conv

   (*
    * Informal rewriting.
    * Create an input form.  This is a rewrite with
    * no justification.
    *)
   val create_iform : string -> bool -> term -> term -> conv

   (*
    * Debugging.
    *)
   val apply_rewrite : Mp_resource.bookmark -> conv -> term -> term
end

(*
 * Print errors.
 *)
module type TacticExnSig =
sig

   val format_exn : dform_base -> buffer -> exn -> unit

   val print : dform_base -> ('a -> 'b) -> 'a -> 'b
   val print_exn : dform_base -> out_channel -> exn -> 'a
end

(*
 * Proof conversion.
 *)
module type ConvertProofSig =
sig
   type t
   type raw
   type cooked

   val to_raw  : t -> string -> cooked -> raw
   val of_raw  : t -> string -> raw -> cooked
   val to_term : t -> string -> cooked -> term
   val of_term : t -> string -> term -> cooked
   val to_term_io : t -> string -> cooked -> Refiner_io.TermType.term
   val of_term_io : t -> string -> Refiner_io.TermType.term -> cooked
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)