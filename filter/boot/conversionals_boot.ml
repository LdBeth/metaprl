(*
 * These are the basic rewriting operations.
 *
 * We execute the operations outside the refiner.
 * After the refinement is done, we construct the
 * rewrite tree.
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

open Lm_debug
open Printf

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Tactic_boot_sig
open Tactic_boot
open Rewrite_boot
open Sequent_boot

open Tacticals_boot

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Conversionals_boot%t"

let debug_conv =
   create_debug (**)
      { debug_name = "conv";
        debug_description = "display conversion operation";
        debug_value = false
      }

let debug_reduce =
   create_debug (**)
      { debug_name = "reduce";
        debug_description = "display reductions";
        debug_value = false
      }

module Conversionals =
struct
   type env = TacticInternalType.env
   type conv = TacticInternalType.conv
   type tactic_arg = TacticInternalType.tactic_arg
   type tactic = TacticInternalType.tactic

   let env_term = RewriteInternal.env_term
   let env_arg = RewriteInternal.env_arg

   let prefix_thenC = RewriteInternal.prefix_thenC
   let prefix_orelseC = RewriteInternal.prefix_orelseC
   let addrC = RewriteInternal.addrC
   let addrLiteralC = RewriteInternal.addrLiteralC
   let idC = RewriteInternal.idC
   let foldC = RewriteInternal.foldC
   let makeFoldC = RewriteInternal.makeFoldC
   let cutC = RewriteInternal.cutC
   let funC = RewriteInternal.funC
   let termC = RewriteInternal.termC

   (************************************************************************
    * SEARCH                                                               *
    ************************************************************************)

   (*
    * Failure.
    *)

   let failWithC s =
      funC (fun _ -> raise (RefineError ("failWithC", StringError s)))

   let failC  =  failWithC "Fail"

   (*
    * Trial.
    *)
   let tryC rw =
      prefix_orelseC rw idC

   (*
    * First subterm that works.
    *)
   let someSubC conv =
      let someSubCE env =
         let t = env_term env in
            if is_sequent_term t then
               (* For sequents, apply to all the hyps, goals, and arg *)
               let { sequent_hyps = hyps;
                     sequent_goals = goals
                   } = explode_sequent t
               in
               let hyp_count = SeqHyp.length hyps in
               let goal_count = SeqGoal.length goals in
               let rec subGoalC conv i =
                  if i = goal_count then
                     addrLiteralC arg_addr conv
                  else
                     prefix_orelseC (addrLiteralC (nth_concl_addr t i) conv) (subGoalC conv (i + 1))
               in
               let rec subHypC conv i =
                  if i = hyp_count then
                     subGoalC conv 0
                  else
                     prefix_orelseC (addrLiteralC (nth_hyp_addr t i) conv) (subHypC conv (i + 1))
               in
                  subHypC conv 0
            else
               (* A normal term *)
               let count = subterm_count t in
               let rec subC i =
                  if i = count then
                     funC (fun _ -> raise (RefineError ("subC", StringError "all subterms failed")))
                  else
                     prefix_orelseC (addrC [i] conv) (subC (i + 1))
               in
                  subC 0
      in
         funC someSubCE

   (*
    * Apply to all subterms.
    *)
   let allSubC conv =
      let allSubCE conv env =
         let t = env_term env in
            if is_sequent_term t then
               (* For sequents, apply to all the hyps, goals, and arg *)
               let { sequent_hyps = hyps;
                     sequent_goals = goals
                   } = explode_sequent t
               in
               let hyp_count = SeqHyp.length hyps in
               let goal_count = SeqGoal.length goals in
               let rec subGoalC conv i =
                  if i = goal_count then
                     addrLiteralC arg_addr conv
                  else
                     prefix_thenC (addrLiteralC (nth_concl_addr t i) conv) (subGoalC conv (i + 1))
               in
               let rec subHypC conv i =
                  if i = hyp_count then
                     subGoalC conv 0
                  else
                     prefix_thenC (addrLiteralC (nth_hyp_addr t i) conv) (subHypC conv (i + 1))
               in
                  subHypC conv 0
            else
               (* A normal term *)
               let count = subterm_count t in
               let rec subC conv count i =
                  if i = count then
                     idC
                  else
                     prefix_thenC (addrC [i] conv) (subC conv count (i + 1))
               in
                  subC conv count 0
      in
         funC (allSubCE conv)

   (*
    * Outermost terms.
    * HigherC has been moved into the refiner
    * for efficiency.
    *)
   let higherC = RewriteInternal.higherC

   (*
    * Apply to leftmost-innermost term.
    *)
   let rec lowerC rw =
      let lowerCE e =
         prefix_orelseC (someSubC (lowerC rw)) rw
      in
         funC lowerCE

   (*
    * Apply to all terms possible from innermost to outermost.
    *)
   let rec sweepUpC rw =
      let sweepUpCE e =
         prefix_thenC (allSubC (sweepUpC rw)) (tryC rw)
      in
         funC sweepUpCE

   let rec sweepDnC rw =
      let sweepDnCE e =
         prefix_thenC (tryC rw) (allSubC (sweepDnC rw))
      in
         funC sweepDnCE

   (*
    * These are the same but don't allow failure.
    *)
   let rec sweepUpFailC rw =
      let sweepUpCE e =
         prefix_thenC (allSubC (sweepUpFailC rw)) rw
      in
         funC sweepUpCE

   let rec sweepDnFailC rw =
      let sweepDnCE e =
         prefix_thenC rw (allSubC (sweepDnFailC rw))
      in
         funC sweepDnCE

   (*
    * Use the first conversion that works.
    *)
   let rec firstC = function
      [conv] ->
         conv
    | conv :: t ->
         prefix_orelseC conv (firstC t)
    | [] ->
         raise (RefineError ("firstC", StringError "empty argument list"))

   (*
    * Apply all to all terms the first conversion that works
    *)
   let applyAllC convs =
      higherC (sweepUpC (firstC convs))

   (*
    * Repeat the conversion until nothing more happens.
    *)
   let whileProgressC conv =
      let repeatCE env =
         let rec repeat t env =
            let t' = env_term env in
               if alpha_equal t t' then
                  idC
               else
                  prefix_thenC conv (funC (repeat t'))
         in
         let t = env_term env in
            prefix_thenC conv (funC (repeat t))
      in
         funC repeatCE

   (*
    * Repeat the conversion until fails nothing more happens.
    *)
   let repeatC conv = whileProgressC (tryC conv)

   (*
    * Repeat the conversion until fails.
    *)
   let untilFailC conv =
      let rec aux env =
         (tryC (prefix_thenC conv (funC aux)))
      in
         funC aux

   let rec repeatForC i conv =
      if i < 0 then
         raise (Invalid_argument "repeatForC: the argument should be not negative")
      else
         let repeatForCE env =
            if i = 0 then
               idC
            else
               prefix_thenC conv (repeatForC (i - 1) conv)
         in
            funC repeatForCE


   let rwc conv assum clause =
      Tacticals.funT
      (fun p -> RewriteInternal.rw conv assum (Sequent.assum_clause_addr p assum clause))

   let rwcAll conv assum  =
(*    RewriteInternal.rw conv assum (TermAddr.make_address []) *)
      Tacticals.onAllMClausesOfAssumT (rwc conv) assum

   let rw conv clause =
      rwc conv 0 clause

   let rwAll conv   =
      rwcAll conv 0

   let rwAllAll conv   =
      Tacticals.prefix_thenT Tacticals.removeHiddenLabelT
      (Tacticals.prefix_thenMT (Tacticals.onAllMAssumT (rwcAll conv)) (rwAll conv))

   let rwh conv = rw (higherC conv)
   let rwch conv = rwc (higherC conv)
   let rwhAll conv = rwAll (higherC conv)
   let rwchAll conv = rwcAll (higherC conv)
   let rwhAllAll conv = rwAllAll (higherC conv)

   let rwa convs = rw (applyAllC convs)
   let rwca convs = rwc (applyAllC convs)
   let rwaAll convs = rwAll (applyAllC convs)
   let rwcaAll convs = rwcAll (applyAllC convs)
   let rwaAllAll convs = rwAllAll (applyAllC convs)

   (*
    * Conversionals.
    *)
   let create_iform = RewriteInternal.create_iform
   let apply_rewrite = RewriteInternal.apply_rewrite
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
