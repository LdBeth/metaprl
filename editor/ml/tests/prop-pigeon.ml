(*
 * Display all the elements in a particular theory.
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
 * Copyright C 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or at your option any later version.
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

include Itt_theory

open Printf

open Mp_debug

open Splay_table
open String_set

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Tactic_type
open Tacticals
open Conversionals
open Var
open Base_dtactic
open Base_auto_tactic

open Itt_equal
open Itt_logic
open Itt_struct

(************************************************************************
 * SPECIALIZED DECISION PROCEDURE                                       *
 ************************************************************************)

(*
 * Table for associating pigeon locations with hyp numbers.
 *)
module StringTableBase =
struct
   type elt = string
   type set = StringSet.t
   type data = int

   let union = StringSet.union
   let compare _ s1 s2 = Pervasives.compare s1 s2
   let append l1 l2 = l1 @ l2
   let print _ _ _ = ()
end

module PigeonTable = MakeTable (StringTableBase)

(*
 * Proving well-formedness.
 *)
let prove_wf = autoT
(*
let rec prove_wf p =
   let t = Sequent.concl p in
   let t = dest_type_term t in
      (if is_var_term t then
          univTypeT << univ[1:l] >> thenT trivialT
       else
          dT 0 thenT prove_wf) p
*)

(*
 * Step 5: prove a disjunct.
 *)
let rec prove_disjunct pigeons p =
   let t = Sequent.concl p in
      (if is_or_term t then
          (selT 1 (dT 0) thenMT prove_disjunct pigeons thenWT prove_wf)
          orelseT (selT 2 (dT 0) thenMT prove_disjunct pigeons thenWT prove_wf)
       else
          try
             nthHypT (PigeonTable.find pigeons (dest_var t))
          with
             Not_found ->
                raise (RefineError ("prove_disjunct", StringError "disjunct not provable"))) p

(*
 * Step 4: prove one of the negations.
 *)
let rec prove_negation pigeons i p =
   ((dT i thenT prove_disjunct pigeons) orelseT prove_negation pigeons (pred i)) p

(*
 * Step 3: forward chain through the possible pigeon locations.
 *)
let rec forward_chain pigeons i p =
   let _, t = Sequent.nth_hyp p i in
      (if is_implies_term t then
         let t, _ = dest_implies t in
            try
               let j = PigeonTable.find pigeons (dest_var t) in
                  dT i thenLT [nthHypT j; forward_chain pigeons (pred i)]
            with
               Not_found ->
                  forward_chain pigeons (pred i)
      else if is_univ_term t then
         idT
      else
         forward_chain pigeons (pred i)) p

(*
 * Step 2: collect the pigeon placements.
 *)
let rec collect_pigeons pigeons i p =
   let _, t = Sequent.nth_hyp p i in
      if is_var_term t then
         collect_pigeons (PigeonTable.add pigeons (dest_var t) i) (pred i) p
      else
         pigeons

(*
 * Step 1: decompose all the disjunctions.
 *)
let rec decompose_disjuncts i p =
   let _, t = Sequent.nth_hyp p i in
      (if is_or_term t then
          dT i thenT decompose_disjuncts i
       else if is_var_term t then
          decompose_disjuncts (pred i)
       else
          idT) p

(*
 * Put it all together.
 *)
let prove3T pigeons p =
   prove_negation pigeons (Sequent.hyp_count p) p

let prove2T p =
   let length = Sequent.hyp_count p in
   let pigeons = collect_pigeons (PigeonTable.create StringSet.empty) length p in
      (forward_chain pigeons length thenT prove3T pigeons) p

let proveT p =
   let length = Sequent.hyp_count p in
      (decompose_disjuncts length thenT prove2T) p

(************************************************************************
 * GENERAL CONSTRUCTIVE PROCEDURE                                       *
 ************************************************************************)

(*
 * Dyckoff's decision procedure for intuitionistic propositional
 * logic.  This is only a little different from the procedure
 * in itt_prop_decide.  We apply autoT to well-formedness subgoals
 * to knock them off as soon as possible.
 *)
let debug_prop_decide =
  create_debug (**)
     { debug_name = "prop_decide";
       debug_description = "show propDecide operations";
       debug_value = false
     }

(* Like onSomeHyp, but works backwards. *)
let revOnSomeHypT tac p =
   let rec aux i =
      if i = 1 then
         tac i
      else if i > 1 then
         tac i orelseT (aux (pred i))
      else
         idT
   in
      aux (Sequent.hyp_count p) p

(* Operate on all non-wf subgoals *)
let ifNotWT tac p =
   (if Sequent.label p = "wf" then
       autoT
    else
       tac) p

(* Select a clause in the disjunction *)
let rec dorT i =
   if i = 1 then
      idT
   else
      selT 2 (dT 0) thenMT dorT (pred i)

(* Decompose a nested disjunction *)
let rec dest_nested_or t =
   if is_or_term t then
      let h, t = dest_or t in
         h :: dest_nested_or t
   else
      [t]

let is_hyp_term p t =
   let rec search i length =
      if i > length then
         false
      else
         try
            let _, t' = Sequent.nth_hyp p i in
               if alpha_equal t t' then
                  true
               else
                  search (succ i) length
         with
            RefineError _ ->
               search (succ i) length
   in
      search 1 (Sequent.hyp_count p)

(* Term classes *)
let is_imp_and_term term =
   is_implies_term term & is_and_term (term_subterm term (make_address [0]))

let is_imp_or_term term =
   is_implies_term term & is_or_term (term_subterm term (make_address [0]))

let is_imp_imp_term term =
   is_implies_term term & is_implies_term (term_subterm term (make_address [0]))

interactive imp_and_rule 'H 'J 'u :
   sequent [squash] { 'H; x: "and"{'C; 'D} => 'B; 'J['x] >- "type"{'C} } -->
   sequent [squash] { 'H; x: "and"{'C; 'D} => 'B; 'J['x] >- "type"{'D} } -->
   sequent ['ext] { 'H; x: "and"{'C; 'D} => 'B; 'J['x];
                     u: 'C => 'D => 'B >- 'T['x] } -->
   sequent ['ext] { 'H; x: "and"{'C; 'D} => 'B; 'J['x] >- 'T['x] }

interactive imp_or_rule 'H 'J 'u 'v :
   sequent [squash] { 'H; x: "or"{'C; 'D} => 'B; 'J['x] >- "type"{'C} } -->
   sequent [squash] { 'H; x: "or"{'C; 'D} => 'B; 'J['x] >- "type"{'D} } -->
   sequent ['ext] { 'H; x: "or"{'C; 'D} => 'B; 'J['x];
                     u: 'C => 'B; v: 'D => 'B >- 'T['x] } -->
   sequent ['ext] { 'H; x: "or"{'C; 'D} => 'B; 'J['x] >- 'T['x] }

interactive imp_imp_rule 'H 'J 'u :
   sequent [squash] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x] >- "type"{'C} } -->
   sequent [squash] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x] >- "type"{'D} } -->
   sequent ['ext] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x];
                     u: 'D => 'B >- 'T['x] } -->
   sequent ['ext] { 'H; x: "implies"{'C; 'D} => 'B; 'J['x] >- 'T['x] }

(* Create a tactic for the X-implication-elimination. *)
let d_and_impT i p =
   if i = 0 then
      raise (RefineError ("d_and_impT", StringError "no introduction form"))
   else
      let u = maybe_new_vars1 p "u" in
      let j, k = Sequent.hyp_indices p i in
         (imp_and_rule j k u
          thenLT [autoT (* addHiddenLabelT "wf" *);
                  autoT (* addHiddenLabelT "wf" *);
                  thinT i]) p

let d_or_impT i p =
   if i = 0 then
      raise (RefineError ("d_or_impT", StringError "no introduction form"))
   else
      let u, v = maybe_new_vars2 p "u" "v" in
      let j, k = Sequent.hyp_indices p i in
         (imp_or_rule j k u v
          thenLT [addHiddenLabelT "wf";
                  addHiddenLabelT "wf";
                  thinT i]) p

let d_imp_impT i p =
   if i = 0 then
      raise (RefineError ("d_and_impT", StringError "no introduction form"))
   else
      let u = maybe_new_vars1 p "u" in
      let j, k = Sequent.hyp_indices p i in
         (imp_and_rule j k u
          thenLT [addHiddenLabelT "wf";
                  addHiddenLabelT "wf";
                  thinT i]) p

(* Try to decompose a hypothesis *)
let rec decompPropDecideHyp1T max_depth count i p =
   let term = snd (Sequent.nth_hyp p i) in
      (if is_false_term term then
          dT i
       else if is_and_term term or is_or_term term then
          dT i thenT ifNotWT (internalPropDecideT max_depth count)
       else if is_imp_and_term term then
          (* {C & D => B} => {C => D => B} *)
          d_and_impT i thenT ifNotWT (internalPropDecideT max_depth count)
       else if is_imp_imp_term term then
          (* {(C => D) => B} => {D => B} *)
          d_imp_impT i thenT ifNotWT (internalPropDecideT max_depth count)
       else
          (* Nothing recognized, try to see if we're done. *)
          nthHypT i) p

and decompPropDecideHyp2T max_depth count i p =
   let term = snd (Sequent.nth_hyp p i) in
      (if is_imp_or_term term then
          (* {C or D => B} => {(C => B) & (D => B)} *)
          d_or_impT i thenT ifNotWT (internalPropDecideT max_depth count)
       else
          failT) p

and decompPropDecideHyp3T max_depth count i p =
   let term = snd (Sequent.nth_hyp p i) in
      (if is_implies_term term then
          let t, _ = dest_implies term in
             if is_hyp_term p t then
                dT i thenT thinT i thenT ifNotWT (internalPropDecideT max_depth count)
             else
                failT
       else
          failT) p

(* Decompose the goal *)
and decompPropDecideConclT max_depth count p =
   let goal = Sequent.concl p in
      (if is_or_term goal then
          (selT 1 (dT 0) thenT ifNotWT (internalPropDecideT max_depth count))
          orelseT (selT 2 (dT 0) thenT ifNotWT (internalPropDecideT max_depth count))
       else if is_and_term goal or is_implies_term goal then
          dT 0 thenT ifNotWT (internalPropDecideT max_depth count)
       else
          trivialT) p

(* Prove the proposition - internal version that does not handle negation *)
and internalPropDecideT max_depth count p =
   if count = max_depth then
      raise (RefineError ("internalPropDecideT", StringIntError ("depth bound exceeded", count)))
   else
      let count = succ count in
         (trivialT
          orelseT revOnSomeHypT (decompPropDecideHyp1T max_depth count)
          orelseT revOnSomeHypT (decompPropDecideHyp2T max_depth count)
          orelseT revOnSomeHypT (decompPropDecideHyp3T max_depth count)
          orelseT decompPropDecideConclT max_depth count) p

(* Convert all "not X" terms to "X => False" *)
let notToImpliesFalseC =
   sweepUpC (unfold_not andthenC fold_implies andthenC (addrC [1] fold_false))

(*
 * Toplevel tactic:
 * Unfold all negations, then run Dyckoff's algorithm.
 *)
let rec iterativeDeepeningPropDecideT max_depth p =
   eprintf "iterativeDeepeningPropDecideT: depth = %d%t" max_depth eflush;
   (internalPropDecideT max_depth 0 orelseT iterativeDeepeningPropDecideT (succ max_depth)) p

let propDecideT =
   onAllClausesT (fun i -> tryT (rw notToImpliesFalseC i))
   thenT iterativeDeepeningPropDecideT 12

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)

(************************************************************************
 * PROBLEMS                                                             *
 ************************************************************************)

interactive pigeon2 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x0_2};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x0_1};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x1_2};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x1_1};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_2};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_1};
	h0: 'x0_1 or 'x0_2;
	h1: 'x1_1 or 'x1_2;
	h2: 'x2_1 or 'x2_2
	>- void
}

interactive pigeon3 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x0_2 or 'x0_3};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x0_1 or 'x0_3};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x0_1 or 'x0_2};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x1_2 or 'x1_3};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x1_1 or 'x1_3};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x1_1 or 'x1_2};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x2_2 or 'x2_3};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x2_1 or 'x2_3};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x2_1 or 'x2_2};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_2 or 'x3_3};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_1 or 'x3_3};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_1 or 'x3_2};
	h0: 'x0_1 or 'x0_2 or 'x0_3;
	h1: 'x1_1 or 'x1_2 or 'x1_3;
	h2: 'x2_1 or 'x2_2 or 'x2_3;
	h3: 'x3_1 or 'x3_2 or 'x3_3
	>- void
}

interactive pigeon4 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x0_4: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x1_4: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x2_4: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	x3_4: univ[1:l];
	x4_1: univ[1:l];
	x4_2: univ[1:l];
	x4_3: univ[1:l];
	x4_4: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x0_2 or 'x0_3 or 'x0_4};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x0_1 or 'x0_3 or 'x0_4};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x0_1 or 'x0_2 or 'x0_4};
	h0_4: 'x0_4 => "not"{. 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x0_1 or 'x0_2 or 'x0_3};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x1_2 or 'x1_3 or 'x1_4};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x1_1 or 'x1_3 or 'x1_4};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x1_1 or 'x1_2 or 'x1_4};
	h1_4: 'x1_4 => "not"{. 'x0_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x1_1 or 'x1_2 or 'x1_3};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x4_1 or 'x2_2 or 'x2_3 or 'x2_4};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x4_2 or 'x2_1 or 'x2_3 or 'x2_4};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x4_3 or 'x2_1 or 'x2_2 or 'x2_4};
	h2_4: 'x2_4 => "not"{. 'x0_4 or 'x1_4 or 'x3_4 or 'x4_4 or 'x2_1 or 'x2_2 or 'x2_3};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x4_1 or 'x3_2 or 'x3_3 or 'x3_4};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x4_2 or 'x3_1 or 'x3_3 or 'x3_4};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x4_3 or 'x3_1 or 'x3_2 or 'x3_4};
	h3_4: 'x3_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x4_4 or 'x3_1 or 'x3_2 or 'x3_3};
	h4_1: 'x4_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_2 or 'x4_3 or 'x4_4};
	h4_2: 'x4_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_1 or 'x4_3 or 'x4_4};
	h4_3: 'x4_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_1 or 'x4_2 or 'x4_4};
	h4_4: 'x4_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_1 or 'x4_2 or 'x4_3};
	h0: 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4;
	h1: 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4;
	h2: 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4;
	h3: 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4;
	h4: 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4
	>- void
}

(*
interactive pigeon5 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x0_4: univ[1:l];
	x0_5: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x1_4: univ[1:l];
	x1_5: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x2_4: univ[1:l];
	x2_5: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	x3_4: univ[1:l];
	x3_5: univ[1:l];
	x4_1: univ[1:l];
	x4_2: univ[1:l];
	x4_3: univ[1:l];
	x4_4: univ[1:l];
	x4_5: univ[1:l];
	x5_1: univ[1:l];
	x5_2: univ[1:l];
	x5_3: univ[1:l];
	x5_4: univ[1:l];
	x5_5: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x0_1 or 'x0_3 or 'x0_4 or 'x0_5};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x0_1 or 'x0_2 or 'x0_4 or 'x0_5};
	h0_4: 'x0_4 => "not"{. 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_5};
	h0_5: 'x0_5 => "not"{. 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x1_1 or 'x1_3 or 'x1_4 or 'x1_5};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x1_1 or 'x1_2 or 'x1_4 or 'x1_5};
	h1_4: 'x1_4 => "not"{. 'x0_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_5};
	h1_5: 'x1_5 => "not"{. 'x0_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x2_1 or 'x2_3 or 'x2_4 or 'x2_5};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x2_1 or 'x2_2 or 'x2_4 or 'x2_5};
	h2_4: 'x2_4 => "not"{. 'x0_4 or 'x1_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_5};
	h2_5: 'x2_5 => "not"{. 'x0_5 or 'x1_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x4_1 or 'x5_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x4_2 or 'x5_2 or 'x3_1 or 'x3_3 or 'x3_4 or 'x3_5};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x4_3 or 'x5_3 or 'x3_1 or 'x3_2 or 'x3_4 or 'x3_5};
	h3_4: 'x3_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x4_4 or 'x5_4 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_5};
	h3_5: 'x3_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x4_5 or 'x5_5 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4};
	h4_1: 'x4_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x5_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5};
	h4_2: 'x4_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x5_2 or 'x4_1 or 'x4_3 or 'x4_4 or 'x4_5};
	h4_3: 'x4_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x5_3 or 'x4_1 or 'x4_2 or 'x4_4 or 'x4_5};
	h4_4: 'x4_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x5_4 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_5};
	h4_5: 'x4_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x5_5 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4};
	h5_1: 'x5_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5};
	h5_2: 'x5_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_1 or 'x5_3 or 'x5_4 or 'x5_5};
	h5_3: 'x5_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_1 or 'x5_2 or 'x5_4 or 'x5_5};
	h5_4: 'x5_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_5};
	h5_5: 'x5_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4};
	h0: 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5;
	h1: 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5;
	h2: 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5;
	h3: 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5;
	h4: 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5;
	h5: 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5
	>- void
}

interactive pigeon6 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x0_4: univ[1:l];
	x0_5: univ[1:l];
	x0_6: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x1_4: univ[1:l];
	x1_5: univ[1:l];
	x1_6: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x2_4: univ[1:l];
	x2_5: univ[1:l];
	x2_6: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	x3_4: univ[1:l];
	x3_5: univ[1:l];
	x3_6: univ[1:l];
	x4_1: univ[1:l];
	x4_2: univ[1:l];
	x4_3: univ[1:l];
	x4_4: univ[1:l];
	x4_5: univ[1:l];
	x4_6: univ[1:l];
	x5_1: univ[1:l];
	x5_2: univ[1:l];
	x5_3: univ[1:l];
	x5_4: univ[1:l];
	x5_5: univ[1:l];
	x5_6: univ[1:l];
	x6_1: univ[1:l];
	x6_2: univ[1:l];
	x6_3: univ[1:l];
	x6_4: univ[1:l];
	x6_5: univ[1:l];
	x6_6: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x0_1 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x0_1 or 'x0_2 or 'x0_4 or 'x0_5 or 'x0_6};
	h0_4: 'x0_4 => "not"{. 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_5 or 'x0_6};
	h0_5: 'x0_5 => "not"{. 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_6};
	h0_6: 'x0_6 => "not"{. 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x1_1 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x1_1 or 'x1_2 or 'x1_4 or 'x1_5 or 'x1_6};
	h1_4: 'x1_4 => "not"{. 'x0_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_5 or 'x1_6};
	h1_5: 'x1_5 => "not"{. 'x0_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_6};
	h1_6: 'x1_6 => "not"{. 'x0_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x2_1 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x2_1 or 'x2_2 or 'x2_4 or 'x2_5 or 'x2_6};
	h2_4: 'x2_4 => "not"{. 'x0_4 or 'x1_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_5 or 'x2_6};
	h2_5: 'x2_5 => "not"{. 'x0_5 or 'x1_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_6};
	h2_6: 'x2_6 => "not"{. 'x0_6 or 'x1_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x3_1 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x3_1 or 'x3_2 or 'x3_4 or 'x3_5 or 'x3_6};
	h3_4: 'x3_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_5 or 'x3_6};
	h3_5: 'x3_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_6};
	h3_6: 'x3_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5};
	h4_1: 'x4_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x5_1 or 'x6_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6};
	h4_2: 'x4_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x5_2 or 'x6_2 or 'x4_1 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6};
	h4_3: 'x4_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x5_3 or 'x6_3 or 'x4_1 or 'x4_2 or 'x4_4 or 'x4_5 or 'x4_6};
	h4_4: 'x4_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x5_4 or 'x6_4 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_5 or 'x4_6};
	h4_5: 'x4_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x5_5 or 'x6_5 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_6};
	h4_6: 'x4_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x5_6 or 'x6_6 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5};
	h5_1: 'x5_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x6_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6};
	h5_2: 'x5_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x6_2 or 'x5_1 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6};
	h5_3: 'x5_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x6_3 or 'x5_1 or 'x5_2 or 'x5_4 or 'x5_5 or 'x5_6};
	h5_4: 'x5_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x6_4 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_5 or 'x5_6};
	h5_5: 'x5_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x6_5 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_6};
	h5_6: 'x5_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x6_6 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5};
	h6_1: 'x6_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6};
	h6_2: 'x6_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_1 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6};
	h6_3: 'x6_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_1 or 'x6_2 or 'x6_4 or 'x6_5 or 'x6_6};
	h6_4: 'x6_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_5 or 'x6_6};
	h6_5: 'x6_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_6};
	h6_6: 'x6_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5};
	h0: 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6;
	h1: 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6;
	h2: 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6;
	h3: 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6;
	h4: 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6;
	h5: 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6;
	h6: 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6
	>- void
}

interactive pigeon7 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x0_4: univ[1:l];
	x0_5: univ[1:l];
	x0_6: univ[1:l];
	x0_7: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x1_4: univ[1:l];
	x1_5: univ[1:l];
	x1_6: univ[1:l];
	x1_7: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x2_4: univ[1:l];
	x2_5: univ[1:l];
	x2_6: univ[1:l];
	x2_7: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	x3_4: univ[1:l];
	x3_5: univ[1:l];
	x3_6: univ[1:l];
	x3_7: univ[1:l];
	x4_1: univ[1:l];
	x4_2: univ[1:l];
	x4_3: univ[1:l];
	x4_4: univ[1:l];
	x4_5: univ[1:l];
	x4_6: univ[1:l];
	x4_7: univ[1:l];
	x5_1: univ[1:l];
	x5_2: univ[1:l];
	x5_3: univ[1:l];
	x5_4: univ[1:l];
	x5_5: univ[1:l];
	x5_6: univ[1:l];
	x5_7: univ[1:l];
	x6_1: univ[1:l];
	x6_2: univ[1:l];
	x6_3: univ[1:l];
	x6_4: univ[1:l];
	x6_5: univ[1:l];
	x6_6: univ[1:l];
	x6_7: univ[1:l];
	x7_1: univ[1:l];
	x7_2: univ[1:l];
	x7_3: univ[1:l];
	x7_4: univ[1:l];
	x7_5: univ[1:l];
	x7_6: univ[1:l];
	x7_7: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x0_1 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x0_1 or 'x0_2 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7};
	h0_4: 'x0_4 => "not"{. 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_5 or 'x0_6 or 'x0_7};
	h0_5: 'x0_5 => "not"{. 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_6 or 'x0_7};
	h0_6: 'x0_6 => "not"{. 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_7};
	h0_7: 'x0_7 => "not"{. 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x1_1 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x1_1 or 'x1_2 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7};
	h1_4: 'x1_4 => "not"{. 'x0_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_5 or 'x1_6 or 'x1_7};
	h1_5: 'x1_5 => "not"{. 'x0_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_6 or 'x1_7};
	h1_6: 'x1_6 => "not"{. 'x0_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_7};
	h1_7: 'x1_7 => "not"{. 'x0_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x2_1 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x2_1 or 'x2_2 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7};
	h2_4: 'x2_4 => "not"{. 'x0_4 or 'x1_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_5 or 'x2_6 or 'x2_7};
	h2_5: 'x2_5 => "not"{. 'x0_5 or 'x1_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_6 or 'x2_7};
	h2_6: 'x2_6 => "not"{. 'x0_6 or 'x1_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_7};
	h2_7: 'x2_7 => "not"{. 'x0_7 or 'x1_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x3_1 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x3_1 or 'x3_2 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7};
	h3_4: 'x3_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_5 or 'x3_6 or 'x3_7};
	h3_5: 'x3_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_6 or 'x3_7};
	h3_6: 'x3_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_7};
	h3_7: 'x3_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6};
	h4_1: 'x4_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7};
	h4_2: 'x4_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x4_1 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7};
	h4_3: 'x4_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x4_1 or 'x4_2 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7};
	h4_4: 'x4_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_5 or 'x4_6 or 'x4_7};
	h4_5: 'x4_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_6 or 'x4_7};
	h4_6: 'x4_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_7};
	h4_7: 'x4_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6};
	h5_1: 'x5_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x6_1 or 'x7_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7};
	h5_2: 'x5_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x6_2 or 'x7_2 or 'x5_1 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7};
	h5_3: 'x5_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x6_3 or 'x7_3 or 'x5_1 or 'x5_2 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7};
	h5_4: 'x5_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x6_4 or 'x7_4 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_5 or 'x5_6 or 'x5_7};
	h5_5: 'x5_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x6_5 or 'x7_5 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_6 or 'x5_7};
	h5_6: 'x5_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x6_6 or 'x7_6 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_7};
	h5_7: 'x5_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x6_7 or 'x7_7 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6};
	h6_1: 'x6_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x7_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7};
	h6_2: 'x6_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x7_2 or 'x6_1 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7};
	h6_3: 'x6_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x7_3 or 'x6_1 or 'x6_2 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7};
	h6_4: 'x6_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x7_4 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_5 or 'x6_6 or 'x6_7};
	h6_5: 'x6_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x7_5 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_6 or 'x6_7};
	h6_6: 'x6_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x7_6 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_7};
	h6_7: 'x6_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x7_7 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6};
	h7_1: 'x7_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7};
	h7_2: 'x7_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_1 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7};
	h7_3: 'x7_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_1 or 'x7_2 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7};
	h7_4: 'x7_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_5 or 'x7_6 or 'x7_7};
	h7_5: 'x7_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_6 or 'x7_7};
	h7_6: 'x7_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_7};
	h7_7: 'x7_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6};
	h0: 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7;
	h1: 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7;
	h2: 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7;
	h3: 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7;
	h4: 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7;
	h5: 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7;
	h6: 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7;
	h7: 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7
	>- void
}

interactive pigeon8 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x0_4: univ[1:l];
	x0_5: univ[1:l];
	x0_6: univ[1:l];
	x0_7: univ[1:l];
	x0_8: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x1_4: univ[1:l];
	x1_5: univ[1:l];
	x1_6: univ[1:l];
	x1_7: univ[1:l];
	x1_8: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x2_4: univ[1:l];
	x2_5: univ[1:l];
	x2_6: univ[1:l];
	x2_7: univ[1:l];
	x2_8: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	x3_4: univ[1:l];
	x3_5: univ[1:l];
	x3_6: univ[1:l];
	x3_7: univ[1:l];
	x3_8: univ[1:l];
	x4_1: univ[1:l];
	x4_2: univ[1:l];
	x4_3: univ[1:l];
	x4_4: univ[1:l];
	x4_5: univ[1:l];
	x4_6: univ[1:l];
	x4_7: univ[1:l];
	x4_8: univ[1:l];
	x5_1: univ[1:l];
	x5_2: univ[1:l];
	x5_3: univ[1:l];
	x5_4: univ[1:l];
	x5_5: univ[1:l];
	x5_6: univ[1:l];
	x5_7: univ[1:l];
	x5_8: univ[1:l];
	x6_1: univ[1:l];
	x6_2: univ[1:l];
	x6_3: univ[1:l];
	x6_4: univ[1:l];
	x6_5: univ[1:l];
	x6_6: univ[1:l];
	x6_7: univ[1:l];
	x6_8: univ[1:l];
	x7_1: univ[1:l];
	x7_2: univ[1:l];
	x7_3: univ[1:l];
	x7_4: univ[1:l];
	x7_5: univ[1:l];
	x7_6: univ[1:l];
	x7_7: univ[1:l];
	x7_8: univ[1:l];
	x8_1: univ[1:l];
	x8_2: univ[1:l];
	x8_3: univ[1:l];
	x8_4: univ[1:l];
	x8_5: univ[1:l];
	x8_6: univ[1:l];
	x8_7: univ[1:l];
	x8_8: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x0_1 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x0_1 or 'x0_2 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8};
	h0_4: 'x0_4 => "not"{. 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8};
	h0_5: 'x0_5 => "not"{. 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_6 or 'x0_7 or 'x0_8};
	h0_6: 'x0_6 => "not"{. 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_7 or 'x0_8};
	h0_7: 'x0_7 => "not"{. 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_8};
	h0_8: 'x0_8 => "not"{. 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x1_1 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x1_1 or 'x1_2 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8};
	h1_4: 'x1_4 => "not"{. 'x0_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8};
	h1_5: 'x1_5 => "not"{. 'x0_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_6 or 'x1_7 or 'x1_8};
	h1_6: 'x1_6 => "not"{. 'x0_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_7 or 'x1_8};
	h1_7: 'x1_7 => "not"{. 'x0_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_8};
	h1_8: 'x1_8 => "not"{. 'x0_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x2_1 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x2_1 or 'x2_2 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8};
	h2_4: 'x2_4 => "not"{. 'x0_4 or 'x1_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8};
	h2_5: 'x2_5 => "not"{. 'x0_5 or 'x1_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_6 or 'x2_7 or 'x2_8};
	h2_6: 'x2_6 => "not"{. 'x0_6 or 'x1_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_7 or 'x2_8};
	h2_7: 'x2_7 => "not"{. 'x0_7 or 'x1_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_8};
	h2_8: 'x2_8 => "not"{. 'x0_8 or 'x1_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x3_1 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x3_1 or 'x3_2 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8};
	h3_4: 'x3_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8};
	h3_5: 'x3_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_6 or 'x3_7 or 'x3_8};
	h3_6: 'x3_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_7 or 'x3_8};
	h3_7: 'x3_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_8};
	h3_8: 'x3_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7};
	h4_1: 'x4_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8};
	h4_2: 'x4_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x4_1 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8};
	h4_3: 'x4_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x4_1 or 'x4_2 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8};
	h4_4: 'x4_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8};
	h4_5: 'x4_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_6 or 'x4_7 or 'x4_8};
	h4_6: 'x4_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_7 or 'x4_8};
	h4_7: 'x4_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_8};
	h4_8: 'x4_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7};
	h5_1: 'x5_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8};
	h5_2: 'x5_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x5_1 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8};
	h5_3: 'x5_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x5_1 or 'x5_2 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8};
	h5_4: 'x5_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8};
	h5_5: 'x5_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_6 or 'x5_7 or 'x5_8};
	h5_6: 'x5_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_7 or 'x5_8};
	h5_7: 'x5_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_8};
	h5_8: 'x5_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7};
	h6_1: 'x6_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x7_1 or 'x8_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8};
	h6_2: 'x6_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x7_2 or 'x8_2 or 'x6_1 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8};
	h6_3: 'x6_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x7_3 or 'x8_3 or 'x6_1 or 'x6_2 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8};
	h6_4: 'x6_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x7_4 or 'x8_4 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8};
	h6_5: 'x6_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x7_5 or 'x8_5 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_6 or 'x6_7 or 'x6_8};
	h6_6: 'x6_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x7_6 or 'x8_6 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_7 or 'x6_8};
	h6_7: 'x6_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x7_7 or 'x8_7 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_8};
	h6_8: 'x6_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x7_8 or 'x8_8 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7};
	h7_1: 'x7_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x8_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8};
	h7_2: 'x7_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x8_2 or 'x7_1 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8};
	h7_3: 'x7_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x8_3 or 'x7_1 or 'x7_2 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8};
	h7_4: 'x7_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x8_4 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8};
	h7_5: 'x7_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x8_5 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_6 or 'x7_7 or 'x7_8};
	h7_6: 'x7_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x8_6 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_7 or 'x7_8};
	h7_7: 'x7_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x8_7 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_8};
	h7_8: 'x7_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x8_8 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7};
	h8_1: 'x8_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8};
	h8_2: 'x8_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_1 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8};
	h8_3: 'x8_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_1 or 'x8_2 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8};
	h8_4: 'x8_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8};
	h8_5: 'x8_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_6 or 'x8_7 or 'x8_8};
	h8_6: 'x8_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_7 or 'x8_8};
	h8_7: 'x8_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_8};
	h8_8: 'x8_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7};
	h0: 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8;
	h1: 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8;
	h2: 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8;
	h3: 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8;
	h4: 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8;
	h5: 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8;
	h6: 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8;
	h7: 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8;
	h8: 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8
	>- void
}

interactive pigeon9 'H :
   sequent ['ext] { 'H;
	x0_1: univ[1:l];
	x0_2: univ[1:l];
	x0_3: univ[1:l];
	x0_4: univ[1:l];
	x0_5: univ[1:l];
	x0_6: univ[1:l];
	x0_7: univ[1:l];
	x0_8: univ[1:l];
	x0_9: univ[1:l];
	x1_1: univ[1:l];
	x1_2: univ[1:l];
	x1_3: univ[1:l];
	x1_4: univ[1:l];
	x1_5: univ[1:l];
	x1_6: univ[1:l];
	x1_7: univ[1:l];
	x1_8: univ[1:l];
	x1_9: univ[1:l];
	x2_1: univ[1:l];
	x2_2: univ[1:l];
	x2_3: univ[1:l];
	x2_4: univ[1:l];
	x2_5: univ[1:l];
	x2_6: univ[1:l];
	x2_7: univ[1:l];
	x2_8: univ[1:l];
	x2_9: univ[1:l];
	x3_1: univ[1:l];
	x3_2: univ[1:l];
	x3_3: univ[1:l];
	x3_4: univ[1:l];
	x3_5: univ[1:l];
	x3_6: univ[1:l];
	x3_7: univ[1:l];
	x3_8: univ[1:l];
	x3_9: univ[1:l];
	x4_1: univ[1:l];
	x4_2: univ[1:l];
	x4_3: univ[1:l];
	x4_4: univ[1:l];
	x4_5: univ[1:l];
	x4_6: univ[1:l];
	x4_7: univ[1:l];
	x4_8: univ[1:l];
	x4_9: univ[1:l];
	x5_1: univ[1:l];
	x5_2: univ[1:l];
	x5_3: univ[1:l];
	x5_4: univ[1:l];
	x5_5: univ[1:l];
	x5_6: univ[1:l];
	x5_7: univ[1:l];
	x5_8: univ[1:l];
	x5_9: univ[1:l];
	x6_1: univ[1:l];
	x6_2: univ[1:l];
	x6_3: univ[1:l];
	x6_4: univ[1:l];
	x6_5: univ[1:l];
	x6_6: univ[1:l];
	x6_7: univ[1:l];
	x6_8: univ[1:l];
	x6_9: univ[1:l];
	x7_1: univ[1:l];
	x7_2: univ[1:l];
	x7_3: univ[1:l];
	x7_4: univ[1:l];
	x7_5: univ[1:l];
	x7_6: univ[1:l];
	x7_7: univ[1:l];
	x7_8: univ[1:l];
	x7_9: univ[1:l];
	x8_1: univ[1:l];
	x8_2: univ[1:l];
	x8_3: univ[1:l];
	x8_4: univ[1:l];
	x8_5: univ[1:l];
	x8_6: univ[1:l];
	x8_7: univ[1:l];
	x8_8: univ[1:l];
	x8_9: univ[1:l];
	x9_1: univ[1:l];
	x9_2: univ[1:l];
	x9_3: univ[1:l];
	x9_4: univ[1:l];
	x9_5: univ[1:l];
	x9_6: univ[1:l];
	x9_7: univ[1:l];
	x9_8: univ[1:l];
	x9_9: univ[1:l];
	h0_1: 'x0_1 => "not"{. 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8 or 'x0_9};
	h0_2: 'x0_2 => "not"{. 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x0_1 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8 or 'x0_9};
	h0_3: 'x0_3 => "not"{. 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x0_1 or 'x0_2 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8 or 'x0_9};
	h0_4: 'x0_4 => "not"{. 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8 or 'x0_9};
	h0_5: 'x0_5 => "not"{. 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_6 or 'x0_7 or 'x0_8 or 'x0_9};
	h0_6: 'x0_6 => "not"{. 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_7 or 'x0_8 or 'x0_9};
	h0_7: 'x0_7 => "not"{. 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_8 or 'x0_9};
	h0_8: 'x0_8 => "not"{. 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_9};
	h0_9: 'x0_9 => "not"{. 'x1_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8};
	h1_1: 'x1_1 => "not"{. 'x0_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8 or 'x1_9};
	h1_2: 'x1_2 => "not"{. 'x0_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x1_1 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8 or 'x1_9};
	h1_3: 'x1_3 => "not"{. 'x0_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x1_1 or 'x1_2 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8 or 'x1_9};
	h1_4: 'x1_4 => "not"{. 'x0_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8 or 'x1_9};
	h1_5: 'x1_5 => "not"{. 'x0_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_6 or 'x1_7 or 'x1_8 or 'x1_9};
	h1_6: 'x1_6 => "not"{. 'x0_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_7 or 'x1_8 or 'x1_9};
	h1_7: 'x1_7 => "not"{. 'x0_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_8 or 'x1_9};
	h1_8: 'x1_8 => "not"{. 'x0_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_9};
	h1_9: 'x1_9 => "not"{. 'x0_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8};
	h2_1: 'x2_1 => "not"{. 'x0_1 or 'x1_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8 or 'x2_9};
	h2_2: 'x2_2 => "not"{. 'x0_2 or 'x1_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x2_1 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8 or 'x2_9};
	h2_3: 'x2_3 => "not"{. 'x0_3 or 'x1_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x2_1 or 'x2_2 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8 or 'x2_9};
	h2_4: 'x2_4 => "not"{. 'x0_4 or 'x1_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8 or 'x2_9};
	h2_5: 'x2_5 => "not"{. 'x0_5 or 'x1_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_6 or 'x2_7 or 'x2_8 or 'x2_9};
	h2_6: 'x2_6 => "not"{. 'x0_6 or 'x1_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_7 or 'x2_8 or 'x2_9};
	h2_7: 'x2_7 => "not"{. 'x0_7 or 'x1_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_8 or 'x2_9};
	h2_8: 'x2_8 => "not"{. 'x0_8 or 'x1_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_9};
	h2_9: 'x2_9 => "not"{. 'x0_9 or 'x1_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8};
	h3_1: 'x3_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8 or 'x3_9};
	h3_2: 'x3_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x3_1 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8 or 'x3_9};
	h3_3: 'x3_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x3_1 or 'x3_2 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8 or 'x3_9};
	h3_4: 'x3_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8 or 'x3_9};
	h3_5: 'x3_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_6 or 'x3_7 or 'x3_8 or 'x3_9};
	h3_6: 'x3_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_7 or 'x3_8 or 'x3_9};
	h3_7: 'x3_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_8 or 'x3_9};
	h3_8: 'x3_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_9};
	h3_9: 'x3_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8};
	h4_1: 'x4_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8 or 'x4_9};
	h4_2: 'x4_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x4_1 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8 or 'x4_9};
	h4_3: 'x4_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x4_1 or 'x4_2 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8 or 'x4_9};
	h4_4: 'x4_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8 or 'x4_9};
	h4_5: 'x4_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_6 or 'x4_7 or 'x4_8 or 'x4_9};
	h4_6: 'x4_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_7 or 'x4_8 or 'x4_9};
	h4_7: 'x4_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_8 or 'x4_9};
	h4_8: 'x4_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_9};
	h4_9: 'x4_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x3_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8};
	h5_1: 'x5_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8 or 'x5_9};
	h5_2: 'x5_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x5_1 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8 or 'x5_9};
	h5_3: 'x5_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x5_1 or 'x5_2 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8 or 'x5_9};
	h5_4: 'x5_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8 or 'x5_9};
	h5_5: 'x5_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_6 or 'x5_7 or 'x5_8 or 'x5_9};
	h5_6: 'x5_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_7 or 'x5_8 or 'x5_9};
	h5_7: 'x5_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_8 or 'x5_9};
	h5_8: 'x5_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_9};
	h5_9: 'x5_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8};
	h6_1: 'x6_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x7_1 or 'x8_1 or 'x9_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8 or 'x6_9};
	h6_2: 'x6_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x7_2 or 'x8_2 or 'x9_2 or 'x6_1 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8 or 'x6_9};
	h6_3: 'x6_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x7_3 or 'x8_3 or 'x9_3 or 'x6_1 or 'x6_2 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8 or 'x6_9};
	h6_4: 'x6_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x7_4 or 'x8_4 or 'x9_4 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8 or 'x6_9};
	h6_5: 'x6_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x7_5 or 'x8_5 or 'x9_5 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_6 or 'x6_7 or 'x6_8 or 'x6_9};
	h6_6: 'x6_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x7_6 or 'x8_6 or 'x9_6 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_7 or 'x6_8 or 'x6_9};
	h6_7: 'x6_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x7_7 or 'x8_7 or 'x9_7 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_8 or 'x6_9};
	h6_8: 'x6_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x7_8 or 'x8_8 or 'x9_8 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_9};
	h6_9: 'x6_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x7_9 or 'x8_9 or 'x9_9 or 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8};
	h7_1: 'x7_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x8_1 or 'x9_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8 or 'x7_9};
	h7_2: 'x7_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x8_2 or 'x9_2 or 'x7_1 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8 or 'x7_9};
	h7_3: 'x7_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x8_3 or 'x9_3 or 'x7_1 or 'x7_2 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8 or 'x7_9};
	h7_4: 'x7_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x8_4 or 'x9_4 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8 or 'x7_9};
	h7_5: 'x7_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x8_5 or 'x9_5 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_6 or 'x7_7 or 'x7_8 or 'x7_9};
	h7_6: 'x7_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x8_6 or 'x9_6 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_7 or 'x7_8 or 'x7_9};
	h7_7: 'x7_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x8_7 or 'x9_7 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_8 or 'x7_9};
	h7_8: 'x7_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x8_8 or 'x9_8 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_9};
	h7_9: 'x7_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x8_9 or 'x9_9 or 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8};
	h8_1: 'x8_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x9_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8 or 'x8_9};
	h8_2: 'x8_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x9_2 or 'x8_1 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8 or 'x8_9};
	h8_3: 'x8_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x9_3 or 'x8_1 or 'x8_2 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8 or 'x8_9};
	h8_4: 'x8_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x9_4 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8 or 'x8_9};
	h8_5: 'x8_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x9_5 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_6 or 'x8_7 or 'x8_8 or 'x8_9};
	h8_6: 'x8_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x9_6 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_7 or 'x8_8 or 'x8_9};
	h8_7: 'x8_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x9_7 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_8 or 'x8_9};
	h8_8: 'x8_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x9_8 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_9};
	h8_9: 'x8_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x9_9 or 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8};
	h9_1: 'x9_1 => "not"{. 'x0_1 or 'x1_1 or 'x2_1 or 'x3_1 or 'x4_1 or 'x5_1 or 'x6_1 or 'x7_1 or 'x8_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_8 or 'x9_9};
	h9_2: 'x9_2 => "not"{. 'x0_2 or 'x1_2 or 'x2_2 or 'x3_2 or 'x4_2 or 'x5_2 or 'x6_2 or 'x7_2 or 'x8_2 or 'x9_1 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_8 or 'x9_9};
	h9_3: 'x9_3 => "not"{. 'x0_3 or 'x1_3 or 'x2_3 or 'x3_3 or 'x4_3 or 'x5_3 or 'x6_3 or 'x7_3 or 'x8_3 or 'x9_1 or 'x9_2 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_8 or 'x9_9};
	h9_4: 'x9_4 => "not"{. 'x0_4 or 'x1_4 or 'x2_4 or 'x3_4 or 'x4_4 or 'x5_4 or 'x6_4 or 'x7_4 or 'x8_4 or 'x9_1 or 'x9_2 or 'x9_3 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_8 or 'x9_9};
	h9_5: 'x9_5 => "not"{. 'x0_5 or 'x1_5 or 'x2_5 or 'x3_5 or 'x4_5 or 'x5_5 or 'x6_5 or 'x7_5 or 'x8_5 or 'x9_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_6 or 'x9_7 or 'x9_8 or 'x9_9};
	h9_6: 'x9_6 => "not"{. 'x0_6 or 'x1_6 or 'x2_6 or 'x3_6 or 'x4_6 or 'x5_6 or 'x6_6 or 'x7_6 or 'x8_6 or 'x9_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_7 or 'x9_8 or 'x9_9};
	h9_7: 'x9_7 => "not"{. 'x0_7 or 'x1_7 or 'x2_7 or 'x3_7 or 'x4_7 or 'x5_7 or 'x6_7 or 'x7_7 or 'x8_7 or 'x9_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_8 or 'x9_9};
	h9_8: 'x9_8 => "not"{. 'x0_8 or 'x1_8 or 'x2_8 or 'x3_8 or 'x4_8 or 'x5_8 or 'x6_8 or 'x7_8 or 'x8_8 or 'x9_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_9};
	h9_9: 'x9_9 => "not"{. 'x0_9 or 'x1_9 or 'x2_9 or 'x3_9 or 'x4_9 or 'x5_9 or 'x6_9 or 'x7_9 or 'x8_9 or 'x9_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_8};
	h0: 'x0_1 or 'x0_2 or 'x0_3 or 'x0_4 or 'x0_5 or 'x0_6 or 'x0_7 or 'x0_8 or 'x0_9;
	h1: 'x1_1 or 'x1_2 or 'x1_3 or 'x1_4 or 'x1_5 or 'x1_6 or 'x1_7 or 'x1_8 or 'x1_9;
	h2: 'x2_1 or 'x2_2 or 'x2_3 or 'x2_4 or 'x2_5 or 'x2_6 or 'x2_7 or 'x2_8 or 'x2_9;
	h3: 'x3_1 or 'x3_2 or 'x3_3 or 'x3_4 or 'x3_5 or 'x3_6 or 'x3_7 or 'x3_8 or 'x3_9;
	h4: 'x4_1 or 'x4_2 or 'x4_3 or 'x4_4 or 'x4_5 or 'x4_6 or 'x4_7 or 'x4_8 or 'x4_9;
	h5: 'x5_1 or 'x5_2 or 'x5_3 or 'x5_4 or 'x5_5 or 'x5_6 or 'x5_7 or 'x5_8 or 'x5_9;
	h6: 'x6_1 or 'x6_2 or 'x6_3 or 'x6_4 or 'x6_5 or 'x6_6 or 'x6_7 or 'x6_8 or 'x6_9;
	h7: 'x7_1 or 'x7_2 or 'x7_3 or 'x7_4 or 'x7_5 or 'x7_6 or 'x7_7 or 'x7_8 or 'x7_9;
	h8: 'x8_1 or 'x8_2 or 'x8_3 or 'x8_4 or 'x8_5 or 'x8_6 or 'x8_7 or 'x8_8 or 'x8_9;
	h9: 'x9_1 or 'x9_2 or 'x9_3 or 'x9_4 or 'x9_5 or 'x9_6 or 'x9_7 or 'x9_8 or 'x9_9
	>- void
}
*)


(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
