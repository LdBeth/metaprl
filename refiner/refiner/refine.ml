(*
 * The refiner deals with proofs and functions on them.
 * We have the following objects in a refiner:
 *    + validation: a validation is a function on proofs
 *       for instance:
 *           f: (H, x:A, y:B, J[pair(x, y)] >> C[pair(x, y)]) -->
 *               (H, x:A, J[x] >> C[x])
 *        this declares "f" to be a validation, which is a function
 *        that takes a proof of the first sequent, and produces a
 *        proof of the second.  These validations can have
 *        arbitrary arity.
 *    + extract: an extract is a form of validation generated
 *          during proof refinement using tactics.
 *    + tactic: a tactic is a "reverse" application of a
 *      validation.  That is, given a validation f: A --> B,
 *      to produce a proof of B, all that is necessary is to
 *      produce a proof of A (modus ponens).
 *
 *    + rewrite: a rewrite can be reduced to an equivalence
 *      of terms in any context:
 *         f: A <--> B
 *      declares a rewrite that will convert an A to a B, or
 *      vice versa in any context.  This is the same as the
 *      validation:
 *         f: C:[A] <--> C:[B]
 *
 *    + cond_rewrite: conditional rewrite that requires
 *      a proof to be valid.  For instance,
 *         p: (x in A # B) --> (pair(x.1, x.2) <--> x)
 *      this rewrite can only be applied in a sequent
 *      calculus, and it means:
 *         p: (H >> x in A # B) --> (C:[pair(x.1, x.2)] <--> C:[x])
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

INCLUDE "refine_error.mlh"

open Printf
open Mp_debug
open String_set

open Opname
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_subst_sig
open Term_addr_sig
open Term_meta_sig
open Term_shape_sig
open Refine_error_sig
open Rewrite_sig
open Refine_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Refine%t"

let debug_refiner =
   create_debug (**)
      { debug_name = "refine";
        debug_description = "Display refinement operations";
        debug_value = false
      }

let debug_sentinal =
   create_debug (**)
      { debug_name = "sentinal";
        debug_description = "Display sentinal operations";
        debug_value = false
      }

let debug_rewrites =
   create_debug (**)
      { debug_name = "rewrites";
        debug_description = "Display rewrite applications";
        debug_value = false
      }

module Refine (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term' = TermType.term'
    with type term = TermType.term
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals
    with type bound_term' = TermType.bound_term'
    with type bound_term = TermType.bound_term)
   (TermMan : TermManSig
    with type hypothesis = Term.hypothesis
    with type esequent = TermType.esequent
    with type term = TermType.term)
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (TermAddr : TermAddrSig
    with type term = TermType.term)
   (TermMeta : TermMetaSig
    with type term = TermType.term)
   (TermShape : TermShapeSig
    with type term = TermType.term)
   (Rewrite : RewriteSig
    with type term = TermType.term
    with type address = TermAddr.address)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = TermAddr.address
    with type meta_term = TermMeta.meta_term) =
struct
   open TermType
   open Term
   open TermMan
   open TermSubst
   open TermAddr
   open TermMeta
   open TermShape
   open Rewrite
   open RefineError

   type term = TermType.term
   type address = TermAddr.address
   type meta_term = TermMeta.meta_term

   (*
    * Refinements are on meta-sequents,
    * which are a restricted form of meta terms,
    * having only dependent functions format.
    *
    * Each hyp is labelled by its first argument.
    *)
   type msequent_free_vars =
      FreeVarsDelayed
    | FreeVars of StringSet.t

   type msequent =
      { mutable mseq_vars : msequent_free_vars;
        mseq_goal : term;
        mseq_hyps : term list
      }

   (*
    * An ML rewrite replaces a term with another.
    *)
   type ml_extract = term list -> term list -> term

   type ml_rewrite = term -> term

   type ml_cond_rewrite =
      StringSet.t ->                                 (* Free vars in the msequent *)
      term list ->                                   (* Params *)
      term ->                                        (* Term to rewrite *)
      term * term list * ml_extract   (* Extractor is returned *)

   (*
    * A condition relaces an goal with a list of subgoals,
    * and it provides a function to compute the extract.
    *)
   type ml_rule =
      int array ->                                   (* sequent context addresses *)
      msequent ->                                    (* goal *)
      term list ->                                   (* params *)
      msequent list * ml_extract      (* subgoals, new variable names *)

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * A proof has either been computed,
    * or the computation is delayed.
    *)
   type 'a proof =
      Extracted of 'a
    | Defined
    | Delayed of (unit -> 'a)

   (*
    * An extract summarizes a validation that is generated by a tactic.
    *
    * The extract type is a tree of terms.  The substitution is
    * delayed, since in most cases the extract term is never
    * computed.
    *
    * The refiner describes the rule that was applied, and
    * in most cases we also list the params to the rule that
    * was applied so that the validation can be called if
    * necessary.  The head rule of the refiner is the applied
    * rule.
    *)
   type extract =
      { ext_goal : msequent;
        ext_just : ext_just;
        ext_subgoals : msequent list
      }

   and rw_extract =
      { rw_goal : term;
        rw_just : rewrite_just;
        rw_subgoal : term
      }

   and crw_extract =
      { crw_goal : term;
        crw_just : cond_rewrite_just;
        crw_subgoal_term : term;
        crw_subgoals : cond_rewrite_subgoals
      }

   and ext_just =
      SingleJust of single_just
    | MLJust of single_just * ml_extract
    | RewriteJust of msequent * rewrite_just * msequent
    | CondRewriteJust of msequent * cond_rewrite_just * msequent list
    | ComposeJust of ext_just * ext_just list
    | NthHypJust of msequent * int
    | CutJust of cut_just
    | Identity

   and atomic_just =
      { just_goal : msequent;
        just_addrs : int array;
        just_params : term list;
        just_refiner : opname;
        just_subgoals : msequent list
      }
   and single_just = atomic_just

   and cut_just =
      { cut_goal : msequent;
        cut_hyp : term;
        cut_lemma : msequent;
        cut_then : msequent
      }

   and rewrite_just =
      RewriteHere of term * opname * term
    | RewriteML of term * opname * term
    | RewriteReverse of rewrite_just
    | RewriteCompose of rewrite_just * rewrite_just
    | RewriteAddress of term * address * rewrite_just * term
    | RewriteHigher of term * rewrite_just list * term

   and cond_rewrite_subgoals =
      CondRewriteSubgoalsAddr of address * cond_rewrite_subgoals
    | CondRewriteSubgoalsList of cond_rewrite_subgoals list
    | CondRewriteSubgoals of term list

   and cond_rewrite_just =
      CondRewriteHere of cond_rewrite_here
    | CondRewriteML of cond_rewrite_here * ml_extract
    | CondRewriteReverse of cond_rewrite_just
    | CondRewriteCompose of cond_rewrite_just * cond_rewrite_just
    | CondRewriteAddress of term * address * cond_rewrite_just * term
    | CondRewriteHigher of term * cond_rewrite_just list * term

   and cond_rewrite_here =
      { cjust_goal : term;
        cjust_params : term list;
        cjust_refiner : opname;
        cjust_subgoal_term : term;
        cjust_subgoals : term list
      }

   (*
    * A refiner contains the following items:
    *    + theorems: terms that are true in a sequent calculus
    *    + rules: implications on proofs
    *    + rewrite: term equivalences in any context
    *    + ml versions of the above
    *
    * refiners can be combined using PairRefiner.
    *)
   and refiner =
      NullRefiner

    | RuleRefiner of rule_refiner
    | PrimRuleRefiner of prim_rule_refiner
    | MLRuleRefiner of ml_rule_refiner

    | RewriteRefiner of rewrite_refiner
    | PrimRewriteRefiner of prim_rewrite_refiner
    | MLRewriteRefiner of ml_rewrite_refiner
    | DefinitionalRewriteRefiner of rewrite_refiner

    | CondRewriteRefiner of cond_rewrite_refiner
    | PrimCondRewriteRefiner of prim_cond_rewrite_refiner
    | MLCondRewriteRefiner of ml_cond_rewrite_refiner

    | PairRefiner of refiner * refiner
    | ListRefiner of refiner list
    | LabelRefiner of string * refiner

   and rule_refiner =
      { rule_name : opname;
        rule_count : int;
        rule_rule : msequent;
        rule_refiner : refiner
      }
   and prim_rule_refiner =
      { mutable prule_proof : (term list -> term list -> term) proof;
        prule_rule : rule_refiner;
        prule_refiner : refiner
      }
   and ml_rule_refiner =
      { ml_rule_name : opname;
        ml_rule_info : ml_rule;
        ml_rule_refiner : refiner
      }

   and rewrite_refiner =
      { rw_name : opname;
        rw_rewrite : term * term;
        rw_refiner : refiner
      }
   and prim_rewrite_refiner =
      { mutable prw_proof : unit proof;
        prw_rewrite : rewrite_refiner;
        prw_refiner : refiner
      }
   and ml_rewrite_refiner =
      { ml_rw_name : opname;
        ml_rw_info : ml_rewrite;
        ml_rw_refiner : refiner
      }

   and cond_rewrite_refiner =
      { crw_name : opname;
        crw_count : int;
        crw_rewrite : term list * term * term;
        crw_refiner : refiner
      }
   and prim_cond_rewrite_refiner =
      { mutable pcrw_proof : unit proof;
        pcrw_rewrite : cond_rewrite_refiner;
        pcrw_refiner : refiner
      }
   and ml_cond_rewrite_refiner =
      { ml_crw_name : opname;
        ml_crw_info : ml_cond_rewrite;
        ml_crw_refiner : refiner
      }

   (*
    * A Build has a reference to a refiner,
    * and the opname of this module.
    *)
   type build =
      { build_opname : opname;
        mutable build_refiner : refiner
      }

   (*
    * A hashtable is constructed for looking up justifications.
    *)
   type hash =
      { hash_rewrite : (opname, prim_rewrite_refiner) Hashtbl.t;
        hash_cond_rewrite : (opname, prim_cond_rewrite_refiner) Hashtbl.t;
        hash_rule : (opname, prim_rule_refiner) Hashtbl.t;
        hash_refiner : (opname, refiner) Hashtbl.t
      }

   type find =
      { find_rewrite : opname -> prim_rewrite_refiner;
        find_cond_rewrite : opname -> prim_cond_rewrite_refiner;
        find_rule : opname -> prim_rule_refiner;
        find_refiner : opname -> refiner
      }

   type check =
      { check_rewrite : rewrite_refiner -> prim_rewrite_refiner;
        check_cond_rewrite : cond_rewrite_refiner -> prim_cond_rewrite_refiner;
        check_rule : rule_refiner -> prim_rule_refiner
      }

   (*
    * This has a similar function.
    * It checks to see if justifications are justifiable.
    *)
   type sentinal =
      { sent_input_form : opname -> unit;
        sent_rewrite : rewrite_refiner -> unit;
        sent_ml_rewrite : ml_rewrite_refiner -> unit;
        sent_cond_rewrite : cond_rewrite_refiner -> unit;
        sent_ml_cond_rewrite : ml_cond_rewrite_refiner -> unit;
        sent_rule : rule_refiner -> unit;
        sent_ml_rule : ml_rule_refiner -> unit
      }

   (*
    * The tactic type is the basic refinement type, and every
    * element of tactic always produces "correct" refinements
    * by construction.  In other words, only primitive rules can
    * be directly injected into the tactic type, and all else is
    * by composition.
    *)
   type tactic = sentinal -> msequent -> msequent list * ext_just

   (*
    * A rewrite replaces a term with another term.
    *)
   type rw_arg1 = sentinal and rw_arg2 = term and rw_val = term * rewrite_just
   type rw = rw_arg1 -> rw_arg2 -> rw_val

   (*
    * A conditional rewrite takes a goal, then applies the rewrite
    * and generates subgoals.  The first argument is the sequent
    * the rewrite is being applied to, and the second is the
    * particular subterm to be rewritted.
    *)
   type crw_arg1 = sentinal and crw_arg2 = StringSet.t and crw_arg3 = term
   type crw_val = term * cond_rewrite_subgoals * cond_rewrite_just
   type cond_rewrite = crw_arg1 -> crw_arg2 -> crw_arg3 -> crw_val

   (*
    * These are the forms created at compile time.
    *)
   type prim_tactic = int array -> term list -> tactic
   type prim_cond_rw = term list -> cond_rewrite
   type prim_rewrite =
      PrimRW of rw
    | CondRW of prim_cond_rw

   (*
    * Extract decription for UI purposes.
    *)
   type extract_description =
       EDRule of opname * int array * term list
     | EDRewrite
     | EDCondREwrite
     | EDComposition (* any compilcated steps will fall into this category *)
     | EDNthHyp of int
     | EDCut of term
     | EDIdentity

   (************************************************************************
    * SEQUENT OPERATIONS                                                   *
    ************************************************************************)

   let dest_msequent mseq =
      mseq.mseq_goal, mseq.mseq_hyps

   let msequent_goal mseq = mseq.mseq_goal

   let msequent_nth_assum mseq i = List.nth mseq.mseq_hyps (pred i)

   let msequent_num_assums mseq = List.length mseq.mseq_hyps

   let msequent_free_vars mseq =
      match mseq.mseq_vars with
         FreeVars vars ->
            vars
       | FreeVarsDelayed ->
            let { mseq_goal = goal; mseq_hyps = hyps } = mseq in
            let vars = free_vars_terms (goal :: hyps) in
               mseq.mseq_vars <- FreeVars vars;
               vars
   
   let remove_red_hbs t =
      if is_sequent_term t then
         let eseq = explode_sequent t in
         let hyps = SeqHyp.to_list eseq.sequent_hyps in
         let hyps' = remove_redundant_hypbindings hyps (SeqGoal.to_list eseq.sequent_goals) in
            if (hyps==hyps') then t else
               mk_sequent_term {eseq with sequent_hyps = SeqHyp.of_list hyps' }
      else
         t

   let msequent_remove_redundant_hypbindings mseq = {
      mseq with
      mseq_goal = remove_red_hbs mseq.mseq_goal;
      mseq_hyps = List_util.smap remove_red_hbs mseq.mseq_hyps
   }

   let mk_msequent goal subgoals =
      { mseq_goal = remove_red_hbs goal;
        mseq_hyps = List_util.smap remove_red_hbs subgoals;
        mseq_vars = FreeVarsDelayed
      }

    (*
     * Check that all the hyps in the list are equal.
     *)
   let equal_hyps hyps t =
      let check hyps' =
         List.for_all2 alpha_equal hyps' hyps
      in
         List.for_all check t

   (*
    * Compare two sequents for alpha eqivalence.
    *)
   let msequent_alpha_equal seq1 seq2 =
      if seq1 == seq2 then
         (* This is the common case *)
         true
      else
         let { mseq_goal = goal1; mseq_hyps = hyps1 } = seq1 in
         let { mseq_goal = goal2; mseq_hyps = hyps2 } = seq2 in
         let rec compare = function
            hyp1::hyps1, hyp2::hyps2 ->
               alpha_equal hyp1 hyp2 & compare (hyps1, hyps2)
          | [], [] ->
               true
          | _ ->
               false
         in
            alpha_equal goal1 goal2 & compare (hyps1, hyps2)

   (*
    * Split the goals from the hyps.
    *)
   let rec split_msequent_list = function
      { mseq_goal = goal; mseq_hyps = hyps }::t ->
         let goals, hypsl = split_msequent_list t in
            goal :: goals, hyps :: hypsl
    | [] ->
         [], []

   (************************************************************************
    * TACTICS                                                              *
    ************************************************************************)

   (*
    * Refinement is just application.
    * The application is doubled: the first argument is
    * for type tactic, and the second is for type safe_tactic.
    *)
   let refine sent (tac : tactic) (seq : msequent) =
      let subgoals, just = tac sent seq in
         subgoals, { ext_goal = seq; ext_just = just; ext_subgoals = subgoals }

   (*
    * The base tactic proves by assumption.
    *)
   let nth_hyp i sent seq =
      let { mseq_goal = goal; mseq_hyps = hyps } = seq in
         try
            if alpha_equal (List.nth hyps i) goal then
               [], NthHypJust (seq, i)
            else
               REF_RAISE(RefineError ("nth_hyp", StringError "hyp mismatch"))
         with
            Failure "nth" ->
               REF_RAISE(RefineError ("nth_hyp", IntError i))

   (*
    * Cut rule.
    *)
   let cut t sent seq =
      let { mseq_hyps = hyps; mseq_goal = goal } = seq in
      let cut_lemma = { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = t } in
      let cut_then = { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps @ [t]; mseq_goal = goal } in
      let cut_info = { cut_goal = seq; cut_hyp = t; cut_lemma = cut_lemma; cut_then = cut_then } in
         [cut_lemma; cut_then], CutJust cut_info

   let subgoals_of_extract ext = ext.ext_subgoals

   (*
    * Compose two extracts.
    * The subgoals of the first must match with the goals of the second.
    *)
   let compose ext extl =
      let subgoals = List.map (fun ext -> ext.ext_goal) extl in
      let _ =
         if not (List_util.for_all2 msequent_alpha_equal ext.ext_subgoals subgoals) then
            REF_RAISE(RefineError ("compose", StringError "goal mismatch"))
      in {
         ext with
         ext_just = ComposeJust (ext.ext_just, List.map (fun ext -> ext.ext_just) extl);
         ext_subgoals = List_util.flat_map (fun ext -> ext.ext_subgoals) extl
      }

   let identity goal =
      { ext_goal = goal; ext_just = Identity; ext_subgoals = [goal] }

   (************************************************************************
    * REGULAR REWRITES                                                     *
    ************************************************************************)

   (*
    * Apply a rewrite to a term.
    *)
   let rw_refine sent (rw : rw) (t : term) =
      let t', just = rw sent t in
         t', { rw_goal = t; rw_just = just; rw_subgoal = t' }

   (*
    * Compose two rewrites.  The subterm of the first
    * must match the goal of the second.
    *)
   let rw_compose ext1 addr ext2 =
      let { rw_goal = goal1; rw_just = just1; rw_subgoal = goal2 } = ext1 in
      let { rw_goal = goal3; rw_just = just2; rw_subgoal = goal4 } = ext2 in
         if alpha_equal (term_subterm goal2 addr) goal3 then
            { rw_goal = goal1;
              rw_just = RewriteCompose (just1, just2);
              rw_subgoal = replace_subterm goal2 addr goal4
            }
         else
            REF_RAISE(RefineError ("rw_compose_rw", StringError "terms do not match"))

   (*
    * Reverse the rewrite
    *)
   let rw_reverse { rw_goal = goal; rw_just = just; rw_subgoal = subgoal } =
      { rw_goal = subgoal; rw_just = RewriteReverse just; rw_subgoal = goal}

   (*
    * Turn it into a regular extract.
    *)
   let extract_of_rw_extract mseq i rw_ext =
      let { mseq_hyps = hyps; mseq_goal = goal } = mseq in
      let goal1 =
         if i = 0 then
            goal
         else if i <= List.length hyps then
            List.nth hyps i
         else
            REF_RAISE(RefineError ("extract_of_rw_extract", StringIntError ("hyp number is out of range", i)))
      in
      let { rw_goal = goal2; rw_just = just; rw_subgoal = subgoal } = rw_ext in
         if alpha_equal goal2 goal1 then
            let subgoal =
               if i = 0 then
                  { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = subgoal }
               else
                  { mseq_vars = FreeVarsDelayed; mseq_hyps = List_util.replace_nth (pred i) subgoal hyps; mseq_goal = goal }
            in
               { ext_goal = mseq;
                 ext_just = RewriteJust (mseq, just, subgoal);
                 ext_subgoals = [subgoal]
               }
         else
            REF_RAISE(RefineError ("extract_of_rw_extract", StringError "goals do not match"))

   (*
    * Turn the rewrite into a tactic.
    *)
   let rwtactic i rw sent mseq =
      let { mseq_hyps = hyps; mseq_goal = goal } = mseq in
      let t =
         if i = 0 then
            goal
         else if i <= List.length hyps then
            List.nth hyps (pred i)
         else
            REF_RAISE(RefineError ("rwtactic", StringIntError ("hyp number is out of range", i)))
      in
      let t', just = rw sent t in
      let subgoal =
         if i = 0 then
            { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = t' }
         else
            { mseq_vars = FreeVarsDelayed; mseq_hyps = List_util.replace_nth (pred i) t' hyps; mseq_goal = goal }
      in
         [subgoal], RewriteJust (mseq, just, subgoal)

   (*
    * Apply a rewrite at an address.
    *)
   let rwaddr addr rw sent t =
      let t', just = apply_fun_arg_at_addr (rw sent) addr t in
         t', RewriteAddress (t, addr, just, t')

   (*
    * Apply the rewrite to the outermost terms where it
    * does not fail.
    *)
   let rwhigher rw sent t =
      let t', justs = apply_fun_higher (rw sent) t in
         t', RewriteHigher (t, justs, t')

   (*
    * Composition is supplied for efficiency.
    *)
   let andthenrw rw1 rw2 sent t =
      let t', just =
         IFDEF VERBOSE_EXN THEN
            try rw1 sent t with
               RefineError (name, x) ->
                  raise (RefineError ("andthenrw", GoalError (name, x)))
         ELSE
            rw1 sent t
         ENDIF
      in
      let t'', just' =
         IFDEF VERBOSE_EXN THEN
            try rw2 sent t' with
               RefineError (name, x) ->
                  raise (RefineError ("andthenrw", SecondError (name, x)))
         ELSE
            rw2 sent t'
         ENDIF
      in
         t'', RewriteCompose (just, just')

   let orelserw rw1 rw2 sent t =
      IFDEF VERBOSE_EXN THEN
         try rw1 sent t with
            RefineError (name1, x) ->
               try rw2 sent t with
                  RefineError (name2, y) ->
                     raise (RefineError ("orelserw", PairError (name1, x, name2, y)))
      ELSE
         try rw1 sent t with
            _ ->
               rw2 sent t
      ENDIF

   (************************************************************************
    * CONDITIONAL REWRITES                                                 *
    ************************************************************************)

   (*
    * Replace the subgoals in the sequent.
    * We have to rename variables to avoid capture,
    * so we need to calculate the binding occurrences to the
    * term in question, and then rename to avoid capture in the goal.
    *)
   let replace_subgoals mseq subgoals =
      let { mseq_hyps = hyps; mseq_goal = seq } = mseq in

      (*
       * We have to rename sequent vars when we substitute into the goal.
       *)
      let replace_subgoal addr t' =
         (* Compute the extra binding variables in the clause *)
         (* HACK!!! This should go away once we implement the crw mechanism properly *)
         let seqtest = TermAddr.replace_subterm seq addr t' in
         let addr' = TermAddr.clause_address_of_address addr in
         let ttst = term_subterm seqtest addr' in
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refine.replace_subgoal %a@%s with %a\n\tTest term: %a%t" print_term seq (TermAddr.string_of_address addr) print_term t' print_term ttst eflush;
         ENDIF;
         if String_set.StringSet.cardinal (free_vars_set ttst) < String_set.StringSet.cardinal (free_vars_set t') then
            REF_RAISE(RefineError ("Refine.replace_subgoals", GoalError("Invalid context for conditional rewrite application",AddressError(addr,seq))));

         (* Now we can replace the goal without fear *)
         let seq = replace_goal seq t' in
            { mseq_vars = FreeVarsDelayed;
              mseq_hyps = hyps;
              mseq_goal = seq
            }
      in

      (*
       * Collect all the subgoals that were given by the conditional
       * rewrite.
       *)
      let rec replace addr subgoals = function
         CondRewriteSubgoalsList subgoals' ->
            List.fold_left (replace addr) subgoals subgoals'
       | CondRewriteSubgoalsAddr (addr', subgoal) ->
            replace (TermAddr.compose_address addr addr') subgoals subgoal
       | CondRewriteSubgoals terms ->
            List.fold_left (fun subgoals t -> replace_subgoal addr t :: subgoals) subgoals terms
      in
         replace (TermAddr.make_address []) [] subgoals

   (*
    * Apply a conditional rewrite.
    *)
   let crw_refine sent (crw : cond_rewrite) mseq t =
      let t', subgoals, just = crw sent (msequent_free_vars mseq) t in
         t', { crw_goal = t;
               crw_just = just;
               crw_subgoal_term = t';
               crw_subgoals = subgoals
         }

   (*
    * Apply a conditional rewrite.
    *)
   let crwtactic i (crw : cond_rewrite) (sent : sentinal) (seq : msequent) =
      let { mseq_goal = goal; mseq_hyps = hyps } = seq in
      let t =
         if i = 0 then
            goal
         else if i <= List.length hyps then
            List.nth hyps (pred i)
         else
            REF_RAISE(RefineError ("crwtactic", StringIntError ("hyp is out of range", i)))
      in
      IFDEF VERBOSE_EXN THEN
         if !debug_rewrites then
            eprintf "crwtactic applied to %a%t" print_term t eflush;
      ENDIF;
      let t', subgoals, just = crw sent (msequent_free_vars seq) t in
      let subgoal =
         if i = 0 then
            { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = t' }
         else
            { mseq_vars = FreeVarsDelayed; mseq_hyps = List_util.replace_nth (pred i) t' hyps; mseq_goal = goal }
      in
      let subgoals = subgoal :: replace_subgoals seq subgoals in
         subgoals, CondRewriteJust (seq, just, subgoals)

   (*
    * Compose the conditional extracts.
    *)
   let crw_compose ext1 addr ext2 =
      let { crw_goal = goal1;
            crw_just = just1;
            crw_subgoal_term = goal2;
            crw_subgoals = subgoals1
          } = ext1
      in
      let { crw_goal = goal3;
            crw_just = just2;
            crw_subgoal_term = goal4;
            crw_subgoals = subgoals2
          } = ext2
      in
         if alpha_equal (term_subterm goal2 addr) goal3 then
            { crw_goal = goal1;
              crw_just = CondRewriteCompose (just1, just2);
              crw_subgoal_term = replace_subterm goal2 addr goal4;
              crw_subgoals = CondRewriteSubgoalsList [subgoals1; CondRewriteSubgoalsAddr (addr, subgoals2)]
            }
         else
            REF_RAISE(RefineError ("crw_compose", StringError "terms do not match"))

   (*
    * Reverse the conditional rewrite.
    *)
   let crw_reverse
       { crw_goal = goal;
         crw_just = just;
         crw_subgoal_term = subgoal;
         crw_subgoals = subgoals
       } =
      { crw_goal = subgoal;
        crw_just = CondRewriteReverse just;
        crw_subgoal_term = goal;
        crw_subgoals = subgoals
      }

   (*
    * Build an extract from the conditional extract.
    *)
   let extract_of_crw_extract mseq i
       { crw_goal = goal;
         crw_just = just;
         crw_subgoal_term = subgoal;
         crw_subgoals = subgoals
       } =
      let { mseq_hyps = hyps; mseq_goal = goal' } = mseq in
      let t =
         if i = 0 then
            goal'
         else if i <= List.length hyps then
            List.nth hyps (pred i)
         else
            REF_RAISE(RefineError ("extract_of_crw_extract", StringIntError ("hyp is out of range", i)))
      in
         if alpha_equal goal t then
            let subgoal =
               if i = 0 then
                  { mseq_vars = FreeVarsDelayed; mseq_hyps = hyps; mseq_goal = subgoal }
               else
                  { mseq_vars = FreeVarsDelayed; mseq_hyps = List_util.replace_nth (pred i) subgoal hyps; mseq_goal = goal' }
            in
            let subgoals = subgoal :: replace_subgoals mseq subgoals in
               { ext_goal = mseq;
                 ext_just = CondRewriteJust (mseq, just, subgoals);
                 ext_subgoals = subgoals
               }
         else
            REF_RAISE(RefineError ("extract_of_crw_extract", StringError "terms do not match"))

   (*
    * Apply the rewrite to an addressed term.
    *)
   let crwaddr addr (crw: cond_rewrite) sent bvars t =
      LETMACRO BODY =
         let t', (subgoals, just) =
            let f sent bvars t =
               let t, subgoals, just = crw sent bvars t in
                  t, (subgoals, just)
            in
               apply_var_fun_arg_at_addr (f sent) addr bvars t
         in
            t', CondRewriteSubgoalsAddr (addr, subgoals), CondRewriteAddress (t, addr, just, t')
      IN
      IFDEF VERBOSE_EXN THEN
         try BODY
         with
            RefineError (name, x) ->
               raise (RefineError ("crwaddr", RewriteAddressError (addr, name, x)))
      ELSE
         BODY
      ENDIF

   (*
    * Apply the rewrite at the outermost terms where it does not fail.
    *)
   let crwhigher (crw: cond_rewrite) sent bvars t =
      let t', args =
         let f sent bvars t =
            let t, subgoals, just = crw sent bvars t in
               t, (subgoals, just)
         in
            apply_var_fun_higher (f sent) bvars t
      in
      let subgoals, just = List.split args in
         t', CondRewriteSubgoalsList subgoals, CondRewriteHigher (t, just, t')

   (*
    * Composition is supplied for efficiency.
    *)
   let candthenrw crw1 crw2 sent bvars t =
      let t', subgoals, just =
         IFDEF VERBOSE_EXN THEN
            try crw1 sent bvars t with
               RefineError (name, x) ->
                  raise (RefineError ("candthenrw", GoalError (name, x)))
         ELSE
            crw1 sent bvars t
         ENDIF
      in
      let t'', subgoals', just' =
         IFDEF VERBOSE_EXN THEN
            try crw2 sent bvars t' with
               RefineError (name, x) ->
                  raise (RefineError ("candthenrw", SecondError (name, x)))
         ELSE
            crw2 sent bvars t'
         ENDIF
      in
         t'', CondRewriteSubgoalsList [subgoals; subgoals'], CondRewriteCompose (just, just')

   let corelserw crw1 crw2 sent bvars t =
      IFDEF VERBOSE_EXN THEN
         try crw1 sent bvars t with
            RefineError (name1, x) ->
               try crw2 sent bvars t with
                  RefineError (name2, y) ->
                     raise (RefineError ("corelserw", PairError (name1, x, name2, y)))
      ELSE
         try crw1 sent bvars t with
            _ ->
               crw2 sent bvars t
      ENDIF

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Empty refiner.
    *)
   let null_refiner name =
      { build_opname = mk_opname name nil_opname;
        build_refiner = NullRefiner
      }

   let refiner_of_build build =
      build.build_refiner

   (*
    * Combine the refiners into a single refiner.
    *)
   let join_refiner build ref1 =
      build.build_refiner <- PairRefiner (build.build_refiner, ref1)

   (*
    * Label a refiner with the name of the module.
    *)
   let label_refiner build name =
      let refiner = LabelRefiner (name, build.build_refiner) in
         build.build_refiner <- refiner;
         refiner

   (*
    * Search for an axiom by name.
    *)
   let find_refiner refiner name =
      let rec search refiners = function
         NullRefiner ->
            refiners, None
       | RuleRefiner { rule_name = n; rule_refiner = next }
       | RewriteRefiner { rw_name = n; rw_refiner = next }
       | CondRewriteRefiner { crw_name = n; crw_refiner = next } as r ->
            if n = name then
               refiners, Some r
            else
               search refiners next
       | DefinitionalRewriteRefiner { rw_refiner = next } 
       | PrimRuleRefiner { prule_refiner = next }
       | PrimRewriteRefiner { prw_refiner = next }
       | PrimCondRewriteRefiner { pcrw_refiner = next } ->
            search refiners next
       | MLRuleRefiner { ml_rule_name = n; ml_rule_refiner = next }
       | MLRewriteRefiner { ml_rw_name = n; ml_rw_refiner = next }
       | MLCondRewriteRefiner { ml_crw_name = n; ml_crw_refiner = next } ->
            if n = name then
               REF_RAISE(RefineError (string_of_opname n, StringError "ML rules/rewrites can't be justified"))
            else
               search refiners next
       | LabelRefiner (_, next) as r ->
            if List.memq r refiners then
               refiners, None
            else
               search (r :: refiners) next
       | PairRefiner (next1, next2) ->
            begin
               match search refiners next1 with
                  refiners, None ->
                     search refiners next2
                | x ->
                     x
            end
       | ListRefiner refiners' ->
            let rec search' refiners = function
               refiner :: tl ->
                  begin
                     match search refiners refiner with
                        refiners, None ->
                           search' refiners tl
                      | x ->
                           x
                  end
             | [] ->
                  refiners, None
            in
               search' refiners refiners'
      in
         match search [] refiner with
            _, Some v ->
               v
          | _ ->
               raise Not_found

   (************************************************************************
    * EXTRACTION                                                           *
    ************************************************************************)

   (*
    * Extract decription for UI purposes.
    *)
   let describe_extract ext =
      match ext.ext_just with
         SingleJust j | MLJust (j, _) ->
            EDRule (j.just_refiner, j.just_addrs, j.just_params)
       | RewriteJust _ -> EDRewrite
       | CondRewriteJust _ -> EDCondREwrite
       | ComposeJust _ -> EDComposition
       | NthHypJust (_, i) -> EDNthHyp i
       | CutJust j -> EDCut j.cut_hyp
       | Identity -> EDIdentity

   (*
    * When an term is calculated from an extract, we have to search
    * for the justifications in the current refiner.  We save them
    * in a hashtable by their names and their types.
    *)
   let hash_refiner refiner =
      let def_shapes = Hashtbl.create 19 in
      let rewrites = Hashtbl.create 19 in
      let cond_rewrites = Hashtbl.create 19 in
      let axioms = Hashtbl.create 19 in
      let rules = Hashtbl.create 19 in
      let refiners = Hashtbl.create 19 in
      let maybe_add hash name info =
         if not (Hashtbl.mem hash name) then
            Hashtbl.add hash name info
      in
      let rec insert refiners' refiner =
         match refiner with
            PrimRuleRefiner prule ->
               let name = prule.prule_rule.rule_name in
                  maybe_add rules name prule;
                  maybe_add refiners name refiner;
                  insert refiners' prule.prule_refiner
          | PrimRewriteRefiner prw ->
               let name = prw.prw_rewrite.rw_name in
                  maybe_add rewrites name prw;
                  maybe_add refiners name refiner;
                  insert refiners' prw.prw_refiner
          | DefinitionalRewriteRefiner rw ->
               let redex, contractum = rw.rw_rewrite in
               let shape = shape_of_term redex in
               if Hashtbl.mem def_shapes shape then
                  REF_RAISE(RefineError("definitional rewrite",StringTermError("shape is already defined",redex)));
               Hashtbl.add def_shapes shape rw;
               let name = rw.rw_name in
                  maybe_add rewrites name {
                     prw_rewrite = rw;
                     prw_refiner = refiner;
                     prw_proof = Defined;
                  };
                  maybe_add refiners name refiner;
                  insert refiners' rw.rw_refiner
          | PrimCondRewriteRefiner pcrw ->
               let name = pcrw.pcrw_rewrite.crw_name in
                  maybe_add cond_rewrites name pcrw;
                  maybe_add refiners name refiner;
                  insert refiners' pcrw.pcrw_refiner
          | RuleRefiner { rule_refiner = next }
          | RewriteRefiner { rw_refiner = next }
          | CondRewriteRefiner { crw_refiner = next }
          | MLRewriteRefiner { ml_rw_refiner = next }
          | MLCondRewriteRefiner { ml_crw_refiner = next }
          | MLRuleRefiner { ml_rule_refiner = next } ->
               insert refiners' next
          | LabelRefiner (_, next) as r ->
               if List.memq r refiners' then
                  refiners'
               else
                  insert (r :: refiners') next
          | PairRefiner (next1, next2) ->
               insert (insert refiners' next1) next2
          | ListRefiner refiners'' ->
               List.fold_left insert refiners' refiners''
          | NullRefiner ->
               refiners'
      in
      let _ = insert [] refiner in
         { hash_rule = rules;
           hash_rewrite = rewrites;
           hash_cond_rewrite = cond_rewrites;
           hash_refiner = refiners
         }

   (*
    * Lookup values in the hashtable, or print error messages.
    *)
   let find_of_hash { hash_rule = rules;
                      hash_rewrite = rewrites;
                      hash_cond_rewrite = cond_rewrites;
                      hash_refiner = refiners
       } =
      let find_rule name =
         try Hashtbl.find rules name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "rule is not justified"))
      in
      let find_rewrite name =
         try Hashtbl.find rewrites name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "rewrite is not justified"))
      in
      let find_cond_rewrite name =
         try Hashtbl.find cond_rewrites name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "cond_rewrite is not justified"))
      in
      let find_refiner name =
         try Hashtbl.find refiners name with
            Not_found ->
               REF_RAISE(RefineError (string_of_opname name, StringError "refiner is not justified"))
      in
         { find_rule = find_rule;
           find_rewrite = find_rewrite;
           find_cond_rewrite = find_cond_rewrite;
           find_refiner = find_refiner
         }

   (*
    * Also sent the matching.
    *)
   let check_of_find { find_rule = find_rule;
                       find_rewrite = find_rewrite;
                       find_cond_rewrite = find_cond_rewrite
       } =
      let check_rule rl =
         let { rule_name = name } = rl in
         let prule = find_rule name in
            if prule.prule_rule == rl then
               prule
            else
               REF_RAISE(RefineError (string_of_opname name, StringError "rule proof does not match"))
      in
      let check_rewrite rw =
         let { rw_name = name } = rw in
         let prw = find_rewrite name in
            if prw.prw_rewrite == rw then
               prw
            else
               REF_RAISE(RefineError (string_of_opname name, StringError "rewrite proof does not match"))
      in
      let check_cond_rewrite crw =
         let { crw_name = name } = crw in
         let pcrw = find_cond_rewrite name in
            if pcrw.pcrw_rewrite == crw then
               pcrw
            else
               REF_RAISE(RefineError (string_of_opname name, StringError "cond_rewrite proof does not match"))
      in
         { check_rule = check_rule;
           check_rewrite = check_rewrite;
           check_cond_rewrite = check_cond_rewrite
         }

   (*
    * Get the extract term for an item.
    *)
   let rule_proof prule =
      match prule.prule_proof with
         Extracted t ->
            t
       | Defined ->
            raise(Invalid_argument "Refine.rule_proof")
       | Delayed f ->
            let t = f () in
               prule.prule_proof <- Extracted t;
               t

   let rewrite_proof prw =
      match prw.prw_proof with
         Extracted _ | Defined ->
            ()
       | Delayed f ->
            prw.prw_proof <- Extracted (f ())

   let cond_rewrite_proof pcrw =
      match pcrw.pcrw_proof with
         Extracted () ->
            ()
       | Defined ->
            raise(Invalid_argument "Refine.rule_proof")
       | Delayed f ->
            pcrw.pcrw_proof <- Extracted (f ())

   (*
    * Expand the extracts of the components.
    *)
   let check = function
      PrimRuleRefiner prule ->
         let _ = rule_proof prule in ()
    | PrimRewriteRefiner prw ->
         rewrite_proof prw
    | PrimCondRewriteRefiner pcrw ->
         cond_rewrite_proof pcrw
    | _ ->
         ()

   (*
    * Check for a valid rewrite justification.
    *)
   let rec check_rewrite_just check = function
      RewriteHere (_, op, _)
    | RewriteML (_, op, _) ->
         check op
    | RewriteReverse just
    | RewriteAddress (_, _, just, _) ->
         check_rewrite_just check just
    | RewriteCompose (just1, just2) ->
         check_rewrite_just check just1;
         check_rewrite_just check just2
    | RewriteHigher (_, justs, _) ->
         List.iter (check_rewrite_just check) justs

   (*
    * Get the subgoal count of a step in the extract.
    *)
   let rec just_subgoal_count = function
      SingleJust { just_subgoals = subgoals }
    | MLJust ({ just_subgoals = subgoals }, _) ->
         List.length subgoals
    | RewriteJust _ ->
         1
    | CondRewriteJust (_, cond, _) ->
         cond_rewrite_just_subgoal_count cond
    | ComposeJust (_, justl) ->
         List.fold_left (fun count just -> count + just_subgoal_count just) 0 justl
    | NthHypJust _ ->
         0
    | CutJust _ ->
         2
    | Identity ->
         1

   and cond_rewrite_just_subgoal_count = function
      CondRewriteHere { cjust_subgoals = subgoals }
    | CondRewriteML ({ cjust_subgoals = subgoals }, _) ->
         List.length subgoals
    | CondRewriteReverse just
    | CondRewriteCompose (_, just)
    | CondRewriteAddress (_, _, just, _) ->
         cond_rewrite_just_subgoal_count just
    | CondRewriteHigher (_, justs, _) ->
         List.fold_left (fun count just -> count + cond_rewrite_just_subgoal_count just) 0 justs

   (*
    * Get the term from an extract.
    * This will fail if some of the rules are not justified.
    *)
   let term_of_extract refiner ext (args : term list) =
      if ext.ext_subgoals <> [] then
         raise (Invalid_argument "Refine.term_of_extract: called on an unfinished proof");
      if (List.length ext.ext_goal.mseq_hyps) <> (List.length args) then
         raise (Invalid_argument "Refine.term_of_extract: wrong number of term arguments");
      let find = find_of_hash (hash_refiner refiner) in
      (* XXX BUG: We never call check_rewrite/check_cond_rewrite, but we should *)
      let { check_rule = find_rule;
            check_rewrite = find_rewrite;
            check_cond_rewrite = find_cond_rewrite
          } = check_of_find find
      in
      let check_rewrite just =
         check_rewrite_just (fun opname -> ignore (find.find_refiner opname)) just
      in
      (* XXX HACK: this approach of building a closure on-the-fly is very inefficient *)
      let rec construct (rest : (term list -> term) list) = function
         SingleJust { just_params = params; just_refiner = name } ->
             let rule =
               (* XXX Nogin: I am not sure this code is correct/best way of doing it *)
               match find.find_refiner name with
                  RuleRefiner r -> find_rule r
                | PrimRuleRefiner r -> r
                | _ ->
                     raise (Invalid_argument("Refine.term_of_extract: extract refers to a non-rule: " ^ (string_of_opname name)))
               in
                  fun args -> rule_proof rule params (all_args args rest)
       | ComposeJust (just, justl) ->
            construct (partition_rest rest justl) just
       | MLJust ({ just_params = params; just_refiner = name }, f) ->
            fun args -> f params (all_args args rest)
       | RewriteJust (_, just, _) ->
            check_rewrite just;
            List.hd rest
       | Identity ->
            List.hd rest
       | NthHypJust (_, i) ->
            fun args -> List.nth args i
       | CondRewriteJust (_, just, _) ->
            (* XXX BUG: just needs to be checked! *)
            List.hd rest
       | CutJust _ ->
            match rest with
               [cut_lemma; cut_then] ->
                  fun args -> cut_then (args @ [cut_lemma args])
             | _ ->
                  raise (Invalid_argument "Refine.term_of_extract: cut extract is ill-formed")

      and all_args args rest =
         List.map (fun r -> r args) rest

      and partition_rest rest = function
         just :: justl ->
            let count = just_subgoal_count just in
            let rest, restl = List_util.split_list count rest in
               (construct rest just) :: partition_rest restl justl
       | [] ->
            if rest <> [] then
               raise (Invalid_argument "Refine.term_of_extract: combination extract is too long");
            []
      in
         try construct [] ext.ext_just args
         with Not_found | Failure _ ->
            raise (Invalid_argument "Refine.term_of_extract: ill-formed extract (bug!)")

   (*
    * An empty sentinal for trying refinements.
    *)
   let any_sentinal =
      let null _ =
         ()
      in
         { sent_input_form = null;
           sent_rewrite = null;
           sent_ml_rewrite = null;
           sent_cond_rewrite = null;
           sent_ml_cond_rewrite = null;
           sent_rule = null;
           sent_ml_rule = null
         }

   let null_sentinal =
      let null _ =
         raise (RefineError ("Refine", StringError "refinements are not allowed with the null sentinal"))
      in
         { sent_input_form = null;
           sent_rewrite = null;
           sent_ml_rewrite = null;
           sent_cond_rewrite = null;
           sent_ml_cond_rewrite = null;
           sent_rule = null;
           sent_ml_rule = null
         }

   (*
    * The sentinal uses a hashtable to lookup valid inferences.
    *)
   let sentinal_of_refiner refiner =
      let rewrites = Hashtbl.create 19 in
      let ml_rewrites = Hashtbl.create 19 in
      let cond_rewrites = Hashtbl.create 19 in
      let ml_cond_rewrites = Hashtbl.create 19 in
      let rules = Hashtbl.create 19 in
      let ml_rules = Hashtbl.create 19 in
      let rec insert refiners = function
         PrimRuleRefiner { prule_refiner = next }
       | PrimRewriteRefiner { prw_refiner = next }
       | PrimCondRewriteRefiner { pcrw_refiner = next } ->
            insert refiners next
       | RuleRefiner r ->
            let { rule_name = name; rule_refiner = next } = r in
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add rule %s%t" (string_of_opname name) eflush
               ENDIF;
               Hashtbl.add rules name r;
               insert refiners next
       | RewriteRefiner rw | DefinitionalRewriteRefiner rw ->
            let { rw_name = name; rw_refiner = next } = rw in
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add rewrite %s%t" (string_of_opname name) eflush
               ENDIF;
               Hashtbl.add rewrites name rw;
               insert refiners next
       | MLRewriteRefiner mlrw ->
            let { ml_rw_name = name; ml_rw_refiner = next } = mlrw in
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add ML rewrite %s%t" (string_of_opname name) eflush;
               ENDIF;
               Hashtbl.add ml_rewrites name mlrw;
               insert refiners next
       | CondRewriteRefiner crw ->
            let { crw_name = name; crw_refiner = next } = crw in
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add cond_rewrite %s%t" (string_of_opname name) eflush
               ENDIF;
               Hashtbl.add cond_rewrites name crw;
               insert refiners next
       | MLCondRewriteRefiner mlrw ->
            let { ml_crw_name = name; ml_crw_refiner = next } = mlrw in
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add ML rewrite %s%t" (string_of_opname name) eflush;
               ENDIF;
               Hashtbl.add ml_cond_rewrites name mlrw;
               insert refiners next
       | MLRuleRefiner mlrule ->
            let { ml_rule_name = opname; ml_rule_refiner = next } = mlrule in
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "sentinal_of_refiner: add ML rule %s%t" (string_of_opname opname) eflush
               ENDIF;
               Hashtbl.add ml_rules opname mlrule;
               insert refiners next
       | LabelRefiner (_, next) as r ->
            if List.memq r refiners then
               refiners
            else
               insert (r :: refiners) next
       | PairRefiner (next1, next2) ->
            insert (insert refiners next1) next2
       | ListRefiner refiners' ->
            List.fold_left insert refiners refiners'
       | NullRefiner ->
            refiners
      in
      let _ = insert [] refiner in
      let check_sentinal table name v =
         if try Hashtbl.find table name == v with Not_found -> false then
            IFDEF VERBOSE_EXN THEN
               if !debug_sentinal then
                  eprintf "check_sentinal: found %s%t" (string_of_opname name) eflush
            ENDIF
         else
            begin
               eprintf "check_sentinal: failed %s%t" (string_of_opname name) eflush;
               REF_RAISE(RefineError
                            ("check_sentinal",
                             StringStringError ("rule is not valid in this context", (string_of_opname name))))
            end
      in
      let check_rule rule = check_sentinal rules rule.rule_name rule in
      let check_ml_rule ml_rule =
         let opname = ml_rule.ml_rule_name in
            if try Hashtbl.find ml_rules opname == ml_rule with Not_found -> false then
               IFDEF VERBOSE_EXN THEN
                  if !debug_sentinal then
                     eprintf "check_ml_rule: found rule %s%t" (string_of_opname opname) eflush
               ENDIF
            else
               begin
                  eprintf "check_ml_rule: sentinal failed: %s%t" (string_of_opname opname) eflush;
                  REF_RAISE(RefineError ("check_ml_rule",
                                         StringStringError ("ML rule is not valid in this context",
                                                            string_of_opname opname)))
               end
      in
      let check_rewrite rw = check_sentinal rewrites rw.rw_name rw in
      let check_ml_rewrite mlrw = check_sentinal ml_rewrites mlrw.ml_rw_name mlrw in
      let check_cond_rewrite crw = check_sentinal cond_rewrites crw.crw_name crw in
      let check_ml_cond_rewrite mlrw = check_sentinal ml_cond_rewrites mlrw.ml_crw_name mlrw in
      let check_input_form name =
         raise (RefineError ("check_input_form", StringStringError ("input forms can't be used in a proof", string_of_opname name)))
      in
         { sent_input_form = check_input_form;
           sent_rewrite = check_rewrite;
           sent_ml_rewrite = check_ml_rewrite;
           sent_cond_rewrite = check_cond_rewrite;
           sent_ml_cond_rewrite = check_ml_cond_rewrite;
           sent_rule = check_rule;
           sent_ml_rule = check_ml_rule
         }

   (************************************************************************
    * RULE                                                                 *
    ************************************************************************)

   (*
    * Create a rule from a meta-term.
    * We allow first-order rules (T -> ... -> T)
    * where each T must be a term, and the arity is arbitrary,
    * and there are no dependencies.
    *)
   let add_rule build name addrs params mterm =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_rule: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
      let terms = unzip_mimplies mterm in
      let subgoals, goal = List_util.split_last terms in
      let seq = mk_msequent goal subgoals in
      let rw = Rewrite.term_rewrite Strict addrs (goal :: params) subgoals in
      let opname = mk_opname name opname in
      let ref_rule =
         { rule_name = opname;
           rule_count = List.length subgoals;
           rule_rule = seq;
           rule_refiner = refiner
         }
      in
      let refiner' = RuleRefiner ref_rule in
      let tac addrs params sent mseq =
         let vars = msequent_free_vars mseq in
         let subgoals = apply_rewrite rw (addrs, vars) mseq.mseq_goal params in
         let make_subgoal subgoal =
            { mseq_vars = FreeVars vars; mseq_goal = subgoal; mseq_hyps = mseq.mseq_hyps }
         in
         let subgoals = List.map make_subgoal subgoals in
         let just =
            SingleJust { just_goal = mseq;
                         just_addrs = addrs;
                         just_params = params;
                         just_refiner = opname;
                         just_subgoals = subgoals
            }
         in
            sent.sent_rule ref_rule;
            subgoals, just
      in
         refiner', (tac : prim_tactic)

   (*
    * Sentinel of a rule/rewrite.
    *)
   let find_sentinal refiner opname =
      match find_refiner refiner opname with
         RuleRefiner { rule_refiner = r }
       | RewriteRefiner { rw_refiner = r }
       | CondRewriteRefiner { crw_refiner = r } ->
            sentinal_of_refiner r
       | _ ->
            (* Only the above can be user-provable and can be returned by find_sentinel *)
            raise (Invalid_argument "find_sentinal")

   (*
    * Theorem for a previous theorem or rule.
    * We once again use the rewriter to compute the
    * extract.  The subextracts are shaped into a
    * term of the form:
    *    lambda(a. lambda(b. ... cons(arg1; cons(arg2; ... cons(argn, nil)))))
    *)
   let compute_rule_ext name params args result =
   (* BUG!!!!!!!!!!!
    * This code was completely wrong (see BUGS 4.4 and 4.10) and was producing stupid
      errors.
      It is disabled for now
    *) (*
      (* Create redex term *)
      let l = Array.length vars in
      let create_redex vars args =
         let args' = mk_xlist_term args in
         let rec aux j =
            if j < l then
               mk_xbind_term vars.(j) (aux (j + 1))
            else
               args'
         in
            aux 0
      in
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.compute_rule_ext: %s: %a + params -> %a%t" name print_term (create_redex vars args) print_term result eflush
      ENDIF;
      let rw = Rewrite.term_rewrite Strict empty_args_spec (create_redex vars args :: params) [result] in
      let compute_ext vars params args =
         match apply_rewrite rw empty_args (create_redex vars args) params with
            [c], x when Array.length x = 0 ->
               c
          | _ ->
               raise (Invalid_argument "Refine.add_prim_theorem.compute_ext: faulty extract")
      in
         compute_ext
   *)
   fun params args ->
      (* Return dummy answer *)
      result

   let add_prim_rule build name params args result =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_prim_theorem: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            RuleRefiner r ->
               let compute_ext = compute_rule_ext name params args result in
                  PrimRuleRefiner { prule_proof = Extracted compute_ext;
                                    prule_rule = r;
                                    prule_refiner = refiner
                  }
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a rule"))

   let add_delayed_rule build name params args ext =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.delayed_rule: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            RuleRefiner r ->
               let compute_ext () =
                  let ext = ext () in
                  let { rule_rule = goal } = r in
                  let { ext_goal = goal'; ext_subgoals = subgoals } = ext in
                  let _ =
                     if not (msequent_alpha_equal goal' goal) or subgoals <> [] then
                        REF_RAISE(RefineError (name, StringError "extract does not match"))
                  in
                  let t = term_of_extract refiner ext args in
                     compute_rule_ext name params args t
               in
                  PrimRuleRefiner { prule_proof = Delayed compute_ext;
                                    prule_rule = r;
                                    prule_refiner = refiner
                  }
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a rule"))

   (*
    * An ML rule
    *)
   let add_ml_rule build name rw =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_ml_rule: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
      let opname = mk_opname name opname in
      let ref_rule =
         { ml_rule_name = opname;
           ml_rule_info = rw;
           ml_rule_refiner = refiner
         }
      in
      let tac addrs params sent mseq =
         let subgoals, ext = rw addrs mseq params in
         let just =
            MLJust ({ just_goal = mseq;
                      just_addrs = addrs;
                      just_params = params;
                      just_refiner = opname;
                      just_subgoals = subgoals
                    }, ext)
         in
            sent.sent_ml_rule ref_rule;
            subgoals, just
      in
         MLRuleRefiner ref_rule, (tac : prim_tactic)

   (*
    * Just do the checking.
    *)
   let check_rule name addrs params mterm =
      let terms = unzip_mimplies mterm in
      let subgoals, goal = List_util.split_last terms in
      let vars = free_vars_terms terms in
         ignore (Rewrite.term_rewrite Strict addrs (goal::params) subgoals);
         List.iter (fun p -> if is_var_term p && not (StringSet.mem vars (dest_var p)) then
            REF_RAISE(RefineError("check_rule", StringStringError("Unused parameter", dest_var p)))) params

   (************************************************************************
    * REWRITE                                                              *
    ************************************************************************)

   (*
    * See if the rewrite will compile.
    *)
   let check_rewrite name params subgoals redex contractum =
      ignore(Rewrite.term_rewrite Strict empty_args_spec (redex::params) (contractum::subgoals))

   (*
    * Create a simple rewrite from a meta-term.
    * The rewrite must be a MetaIff.
    *)
   let add_rewrite build name redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
      let rw = Rewrite.term_rewrite Strict empty_args_spec [redex] [contractum] in
      let opname = mk_opname name opname in
      let ref_rewrite =
         { rw_name = opname;
           rw_rewrite = redex, contractum;
           rw_refiner = refiner
         }
      in
      let refiner' = RewriteRefiner ref_rewrite in
      let rw sent t =
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refiner: applying simple rewrite %s to %a%t" name print_term t eflush;
         ENDIF;
         match apply_rewrite rw empty_args t [] with
            [t'] ->
               sent.sent_rewrite ref_rewrite;
               t', RewriteHere (t, opname, t')
          | [] ->
               raise (Failure "Refine.add_rewrite: no contracta")
          | _ ->
               raise (Failure "Refine.add_rewrite: multiple contracta")
      in
         refiner', (rw : rw)

   (*
    * Input forms are like rewrites,
    * but they don't get added to the refiner,
    * so they will fail if you every try to use
    * them in a proof.  Use any_sentinal for input_forms.
    *)
   let add_input_form build name strict redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_input_form: %s%t" name eflush
      ENDIF;
      let { build_opname = opname } = build in
      let strictp = if strict then Strict else Relaxed in
      let rw = Rewrite.term_rewrite strictp empty_args_spec [redex] [contractum] in
      let opname = mk_opname name opname in
      let rw sent t =
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refiner: applying input form %s to %a%t" name print_term t eflush;
         ENDIF;
         match apply_rewrite rw empty_args t [] with
            [t'] ->
               sent.sent_input_form opname;
               t', RewriteHere (t, opname, t')
          | [] ->
               raise (Failure "Refine.add_input_form: no contracta")
          | _ ->
               raise (Failure "Refine.add_input_form: multiple contracta")
      in
         PrimRW rw

   let add_prim_rewrite build name redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_prim_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            RewriteRefiner rw ->
               let { rw_rewrite = redex', contractum' } = rw in
               let term1 = mk_xlist_term [redex; contractum] in
               let term2 = mk_xlist_term [redex'; contractum'] in
                  if alpha_equal term1 term2 then
                     PrimRewriteRefiner { prw_rewrite = rw;
                                          prw_refiner = refiner;
                                          prw_proof = Extracted ()
                     }
                  else
                     REF_RAISE(RefineError (name, StringError "rewrite mismatch"))
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a rewrite"))

   let rec check_bound_vars bvars = function
      [] ->
         ()
    | v::ts ->
         let v = dest_var v in
            if List.mem v bvars then
                check_bound_vars (List_util.remove v bvars) ts
            else
               REF_RAISE(RefineError ("definitional rewrite", RewriteFreeSOVar v))
   
   let check_def_bterm bt =
      let bt = dest_bterm bt in
         check_bound_vars bt.bvars (snd (dest_so_var bt.bterm))

   let add_def_rewrite build name redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_def_rewrite: %s%t" name eflush
      ENDIF;
      let _ = List.iter check_def_bterm (dest_term redex).term_terms in
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            RewriteRefiner rw ->
               let { rw_rewrite = redex', contractum' } = rw in
               let term1 = mk_xlist_term [redex; contractum] in
               let term2 = mk_xlist_term [redex'; contractum'] in
                  if alpha_equal term1 term2 then
                     DefinitionalRewriteRefiner rw
                  else
                     REF_RAISE(RefineError (name, StringError "rewrite mismatch"))
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a rewrite"))

   let add_delayed_rewrite build name redex contractum ext =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_delayed_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            RewriteRefiner rw ->
               let compute_ext () =
                  let { rw_rewrite = redex, contractum } = rw in
                  let ext = ext () in
                  let { ext_goal = goal; ext_subgoals = subgoals } = ext in
                  let t =
                     match goal, subgoals with
                        { mseq_goal = goal; mseq_hyps = [] },
                        [{ mseq_goal = subgoal; mseq_hyps = [] }] ->
                           if alpha_equal goal redex & alpha_equal subgoal contractum then
                              term_of_extract refiner ext []
                           else
                              REF_RAISE(RefineError (name, StringError "extract does not match"))
                      | _ ->
                           REF_RAISE(RefineError (name, StringError "bogus proof"))
                  in
                     ()
               in
                  PrimRewriteRefiner { prw_proof = Delayed compute_ext;
                                       prw_rewrite = rw;
                                       prw_refiner = refiner
                  }
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a rewrite"))

   (*
    * An ML rewrite.
    *)
   let add_ml_rewrite build name rw =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_ml_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
      let opname = mk_opname name opname in
      let ref_rw =
         { ml_rw_name = opname;
           ml_rw_info = rw;
           ml_rw_refiner = refiner
         }
      in
      let refiner' = MLRewriteRefiner ref_rw in
      let rw (sent : sentinal) (t : term) =
         let t' = rw t in
            sent.sent_ml_rewrite ref_rw;
            t', RewriteML (t, opname, t')
      in
         refiner', (rw : rw)

   (************************************************************************
    * CONDITIONAL REWRITE                                                  *
    ************************************************************************)

   (*
    * Conditional rewrite.
    *)
   let add_cond_rewrite build name params subgoals redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_cond_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
      let rw = Rewrite.term_rewrite Strict empty_args_spec (redex::params) (contractum :: subgoals) in
      let opname = mk_opname name opname in
      let ref_crw =
         { crw_name = opname;
           crw_count = List.length subgoals;
           crw_rewrite = subgoals, redex, contractum;
           crw_refiner = refiner
         }
      in
      let refiner' = CondRewriteRefiner ref_crw in
      let rw' params (sent : sentinal) (bvars : StringSet.t) t =
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrites then
               eprintf "Refiner: applying conditional rewrite %s to %a with bvars = [%a] %t" name print_term t print_string_list (StringSet.elements bvars) eflush;
         ENDIF;
         match apply_rewrite rw ([||], bvars) t params with
            (t' :: subgoals) ->
               sent.sent_cond_rewrite ref_crw;
                  t',
                  CondRewriteSubgoals subgoals,
                  CondRewriteHere { cjust_goal = t;
                                    cjust_params = params;
                                    cjust_refiner = opname;
                                    cjust_subgoal_term = t';
                                    cjust_subgoals = subgoals
                  }
             | [] ->
                  raise (Failure "Refine.add_cond_rewrite: no contracta")
      in
         refiner', (rw' : prim_cond_rw)

   let add_prim_cond_rewrite build name params subgoals redex contractum =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_prim_cond_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            CondRewriteRefiner crw ->
               let { crw_rewrite = subgoals', redex', contractum' } = crw in
               let term1 = mk_xlist_term (redex :: contractum :: subgoals) in
               let term2 = mk_xlist_term (redex' ::  contractum' :: subgoals') in
                  if alpha_equal term1 term2 then
                     PrimCondRewriteRefiner { pcrw_proof = Extracted ();
                                              pcrw_rewrite = crw;
                                              pcrw_refiner = refiner
                     }
                  else
                     REF_RAISE(RefineError (name, StringError "not a conditional rewrite"))
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a conditional rewrite"))

   let add_delayed_cond_rewrite build name params subgoals redex contractum ext =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_delayed_cond_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
         match find_refiner refiner (mk_opname name opname) with
            CondRewriteRefiner crw ->
               let compute_ext () =
                  let ext = ext () in
                  let { ext_goal = goal; ext_subgoals = subgoals' } = ext in
                  let { mseq_goal = goal; mseq_hyps = goal_hyps } = goal in
                  let subgoals', sub_hyps = split_msequent_list subgoals' in
                  let { crw_rewrite = subgoals, redex, contractum } = crw in
                  let redex = replace_goal goal redex in
                  let contractum = replace_goal goal contractum in
                  let subgoals = List.map (replace_goal goal) subgoals in
                     if equal_hyps goal_hyps sub_hyps &
                        List_util.for_all2 alpha_equal (redex :: contractum :: subgoals) (goal :: subgoals)
                     then
                        ignore (term_of_extract refiner ext [])
                     else
                        REF_RAISE(RefineError (name, StringError "derivation does not match"))
               in
                  PrimCondRewriteRefiner { pcrw_proof = Delayed compute_ext;
                                           pcrw_rewrite = crw;
                                           pcrw_refiner = refiner
                  }
          | _ ->
               REF_RAISE(RefineError (name, StringError "not a conditional rewrite"))

   (*
    * An ML rewrite.
    *)
   let add_ml_cond_rewrite build name rw =
      IFDEF VERBOSE_EXN THEN
         if !debug_refiner then
            eprintf "Refiner.add_cond_rewrite: %s%t" name eflush
      ENDIF;
      let { build_opname = opname; build_refiner = refiner } = build in
      let opname = mk_opname name opname in
      let ref_crw =
         { ml_crw_name = opname;
           ml_crw_info = rw;
           ml_crw_refiner = refiner
         }
      in
      let refiner' = MLCondRewriteRefiner ref_crw in
      let rw params (sent : sentinal) (bvars : StringSet.t) t =
         let t', subgoals, ext = rw bvars params t in
            sent.sent_ml_cond_rewrite ref_crw;
            t',
            CondRewriteSubgoals subgoals,
            CondRewriteML ({ cjust_goal = t;
                             cjust_params = params;
                             cjust_refiner = opname;
                             cjust_subgoal_term = t';
                             cjust_subgoals = subgoals
                           }, ext)
      in
         refiner', (rw : prim_cond_rw)

   (************************************************************************
    * API FUNCTIONS                                                        *
    ************************************************************************)

   (*
    * Rules.
    *)
   let create_rule build name addrs params mterm =
      let refiner, tac = add_rule build name addrs params mterm in
         build.build_refiner <- refiner;
         tac

   let prim_rule build name params args result =
      build.build_refiner <-  add_prim_rule build name params args result

   let delayed_rule build name params args extf =
      build.build_refiner <- add_delayed_rule build name params args extf

   let derived_rule build name params args ext =
      let extf () = ext in
      let refiner = add_delayed_rule build name params args extf in
         check refiner;
         build.build_refiner <- refiner

   let create_ml_rule build name mlr =
      let refiner, tac = add_ml_rule build name mlr in
         build.build_refiner <- refiner;
         tac

   (*
    * Rewrites.
    *)
   let create_rewrite build name redex contractum =
      let refiner, rw = add_rewrite build name redex contractum in
         build.build_refiner <- refiner;
         PrimRW rw

   let create_input_form = add_input_form

   let prim_rewrite build name redex contractum =
      build.build_refiner <- add_prim_rewrite build name redex contractum

   let definitional_rewrite build name redex contractum =
      build.build_refiner <- add_def_rewrite build name redex contractum

   let delayed_rewrite build name redex contractum extf =
      build.build_refiner <- add_delayed_rewrite build name redex contractum extf

   let derived_rewrite build name redex contractum ext =
      let extf () = ext in
      let refiner = add_delayed_rewrite build name redex contractum extf in
         check refiner;
         build.build_refiner <- refiner

   let create_ml_rewrite build name rw =
      let refiner, rw' = add_ml_rewrite build name rw in
         build.build_refiner <- refiner;
         PrimRW rw'

   (*
    * Condiitional rewrites.
    *)
   let create_cond_rewrite build name params args redex contractum =
      let refiner, rw = add_cond_rewrite build name params args redex contractum in
         build.build_refiner <- refiner;
         CondRW rw

   let prim_cond_rewrite build name params args redex contractum =
      build.build_refiner <- add_prim_cond_rewrite build name params args redex contractum

   let delayed_cond_rewrite build name params args redex contractum extf =
      build.build_refiner <- add_delayed_cond_rewrite build name params args redex contractum extf

   let derived_cond_rewrite build name params args redex contractum ext =
      let extf () = ext in
      let refiner = add_delayed_cond_rewrite build name params args redex contractum extf in
         check refiner;
         build.build_refiner <- refiner

   let create_ml_cond_rewrite build name rw =
      let refiner, rw' = add_ml_cond_rewrite build name rw in
         build.build_refiner <- refiner;
         CondRW rw'

end

