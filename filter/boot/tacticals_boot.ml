(*
 * Some basic tacticals.
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
 *
 *)

open Printf
open Mp_debug

open Refiner.Refiner.Refine
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Tactic_boot
open Sequent_boot

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Tacticals_boot%t"

module Tacticals =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type tactic = TacticInternalType.tactic
   type tactic_arg = TacticInternalType.tactic_arg
   type tactic_value = TacticInternalType.tactic_value
   type extract = TacticInternalType.extract
   type arglist = TacticType.arglist

   (************************************************************************
    * TRIVIAL TACTICS                                                      *
    ************************************************************************)

   (* Trivial tactics *)
   let idT =
      TacticInternal.idT

   let failT p =
      raise (RefineError ("failT", StringError "Fail"))

   let failWithT s p =
      raise (RefineError ("failWithT", StringError s))

   let timingT =
      TacticInternal.timingT

   let finalT =
      TacticInternal.finalT

   let nthAssumT =
      TacticInternal.nthAssumT

   let cutT =
      TacticInternal.cutT

   (************************************************************************
    * SEQUENCING                                                           *
    ************************************************************************)

   let prefix_orelseT = TacticInternal.prefix_orelseT
   let prefix_andalsoT = TacticInternal.prefix_thenT
   let prefix_thenT = TacticInternal.prefix_thenT
   let prefix_thenLT = TacticInternal.prefix_thenLT
   let prefix_thenFLT = TacticInternal.prefix_thenFLT

   let tryT tac =
      prefix_orelseT tac idT

   let prefix_orthenT tac1 tac2 =
      prefix_orelseT (prefix_thenT tac1 (tryT tac2)) tac2

   let rec firstT = function
      [tac] ->
         tac
    | tac :: tactl ->
         prefix_orelseT tac (firstT tactl)
    | [] ->
         (fun p -> raise (RefineError ("firstT", StringError "no tactics")))

   let prefix_then_OnFirstT tac1 tac2 =
      let aux = function
         p::l ->
            let tac2' = tac2 p in
            let rec dup_id = function
               x::t ->
                  idT x :: dup_id t
             | [] ->
                  []
            in
               tac2' :: dup_id l
       | [] ->
            []
      in
         prefix_thenFLT tac1 aux

   let prefix_then_OnLastT tac1 tac2 =
      let rec aux = function
         [p] ->
            [tac2 p]
       | p::t ->
            idT p :: aux t
       | [] ->
            []
      in
         prefix_thenFLT tac1 aux

   let prefix_then_OnSameConclT tac1 tac2 =
      let first p =
         let t = Sequent.concl p in
         let second p =
            (if alpha_equal t (Sequent.concl p) then
                tac2
             else
                idT) p
         in
            prefix_thenT tac1 second p
      in
         first

   (************************************************************************
    * PROGRESS                                                             *
    ************************************************************************)

   (* Allow tactic only if no subgoals *)
   let completeT tac =
      prefix_thenT tac (failWithT "completeT")

   (*
    * Apply the tactic and fail if there is only
    * one subgoal and it is the same.
    *)
   let progressT tac =
      let aux p =
         let t = Sequent.goal p in
         let tac' p =
            match p with
               [p] ->
                  if alpha_equal (Sequent.goal p) t then
                     raise (RefineError ("progressT", StringError "no progress"))
                  else
                     [idT p]
             | _ ->
                  List.map idT p
         in
            prefix_thenFLT tac tac' p
      in
         aux

   (*
    * Repeat, spreading out over subgoals.
    * Stop if there is no progress.
    *)
   let whileProgressT tac =
      let rec aux t p =
         let t' = Sequent.goal p in
            if alpha_equal t t' then
               idT p
            else
               prefix_thenT tac (aux t') p
      in
      let start p =
         let t = Sequent.goal p in
            prefix_thenT tac (aux t) p
      in
         start


   (*
    * Repeat, spreading out over subgoals.
    * Stop when the tactic fails.
    *)
   let untilFailT tac =
      let rec aux p =
          (tryT (prefix_thenT tac aux)) p
      in
         aux

   (*
    * Repeat, spreading out over subgoals.
    * Stop if there is no progress or the tactic fails.
    *)
   let repeatT tac = whileProgressT (tryT tac)

   (*
    * Repeat a tactic for a fixed number of times.
    *)
   let repeatForT i tac =
      if i < 0 then
         raise (Invalid_argument "repeatForT: the argument should be not negative")
      else if i = 0 then
         idT
      else
         let rec aux i =
            if i = 1 then
               tac
            else
               prefix_thenT tac (aux (i - 1))
         in
            aux i

   (*
    * Seuqence the tacs.
    *)
   let rec seqT = function
      [tac] ->
         tac
    | tac::tactl ->
         prefix_thenT tac (seqT tactl)
    | [] ->
         idT

   (*
    * List is a list version of the then tactic, but only
    * applies to goals with the same conclusion.
    *)
   let seqOnSameConclT = function
      [] ->
         idT
    | tacs ->
         let start p =
            (* Save the first conclusion *)
            let t = Sequent.concl p in
            let rec aux tacs p =
               (* Recurse through the tactics *)
               (match tacs with
                   tac::tactl ->
                      let t' = Sequent.concl p in
                         if alpha_equal t t' then
                            prefix_thenT tac (aux tactl)
                         else
                            idT
                 | [] -> idT) p
            in
               aux tacs p
         in
            start

   (************************************************************************
    * CONDITIONALS                                                         *
    ************************************************************************)

   (*
    * Conditionals.
    *)
   let ifT pred tac1 tac2 p =
      (if pred p then
          tac1
       else
          tac2) p

   let ifOnConclT pred =
      ifT (function p -> pred (Sequent.concl p))

   let ifOnHypT pred tac1 tac2 i p =
      (if pred (snd (Sequent.nth_hyp p i)) then
          tac1
       else
          tac2) i p

   let ifThenT pred tac1 =
      ifT (function p -> pred (Sequent.goal p)) tac1 idT

   let ifThenOnConclT pred tac =
      let failT = failWithT "ifThenOnConclT" in
         ifOnConclT pred tac failT

   let ifThenOnHypT pred tac i =
      let failT _ = failWithT "ifThenOnHypT" in
         ifOnHypT pred tac failT i

   let whileT pred tac =
      let rec aux p =
         tryT (ifThenT pred (prefix_thenT (progressT tac) aux)) p
      in
         aux

   let untilT pred =
      whileT (function p -> not (pred p))

   (************************************************************************
    * LABEL TACTICS                                                        *
    ************************************************************************)

   (*
    * Label tactics.
    *)
   let main_labels =
      ["main";
       "upcase";
       "downcase";
       "basecase";
       "truecase";
       "falsecase";
       "subterm"]

   let predicate_labels =
      ["set predicate";
       "rewrite subgoal";
       "assertion";
       "antecedent"]

   (*
    * Add a label attribute.
    *)
   let addHiddenLabelT = TacticInternal.setLabelT

   let removeHiddenLabelT =
      addHiddenLabelT "main"

   let keepingLabelT tac p =
      let label = Sequent.label p in
         (prefix_thenT tac (addHiddenLabelT label)) p

   (*
    * Conditional on label.
    *)
   let ifLabLT tacs p =
      let lab = Sequent.label p in
         try
            let tac = List.assoc lab tacs in
               tac p
         with
            Not_found ->
               idT p

   let ifLabT lab tac1 tac2 p =
      let lab' = Sequent.label p in
         (if lab = lab' then
             tac1
          else
             tac2) p

   let ifMT tac p =
      (if List.mem (Sequent.label p) main_labels then
          tac
       else
          idT) p

   let ifWT tac p =
      (if (Sequent.label p) = "wf" then
          tac
       else
          idT) p

   let ifET tac p =
      (if Sequent.label p = "equality" then
          tac
       else
          idT) p

   let ifAT tac p =
      (if List.mem (Sequent.label p) main_labels then
          idT
       else
          tac) p

   let ifPT tac p =
      (if List.mem (Sequent.label p) predicate_labels then
          tac
       else
          idT) p

   (*
    * Label tacticals.
    *)
   let prefix_thenLabLT tac1 tacs =
      prefix_thenT tac1 (ifLabLT tacs)

   let prefix_thenMT tac1 tac2 =
      prefix_thenT tac1 (ifMT tac2)

   let prefix_thenAT tac1 tac2 =
      prefix_thenT tac1 (ifAT tac2)

   let prefix_thenWT tac1 tac2 =
      prefix_thenT tac1 (ifWT tac2)

   let prefix_thenET tac1 tac2 =
      prefix_thenT tac1 (ifET tac2)

   let prefix_thenPT tac1 tac2 =
      prefix_thenT tac1 (ifPT tac2)

   (*
    * Apply the tactic list only to the specified subgoals.
    *)
   let thenLLT pred tac1 tacs =
      let rec aux ts = function
         p::ps ->
            if pred (Sequent.label p) then
               match ts with
                  tac::tactl ->
                     (tac p)::(aux tactl ps)
                | [] ->
                     raise (RefineError ("thenMLT", StringError "argument mismatch"))
            else
               (idT p)::(aux ts ps)
       | [] ->
            match ts with
               [] -> []
             | _ ->
                  raise (RefineError ("thenMLT", StringError "argument mismatch"))
      in
         prefix_thenFLT tac1 (aux tacs)

   let prefix_thenMLT =
      thenLLT (function l -> List.mem l main_labels)

   let prefix_thenALT =
      thenLLT (function l -> not (List.mem l main_labels))

   (************************************************************************
    * LABEL PROGRESS                                                       *
    ************************************************************************)

   (*
    * Repeat only on main subgoals.
    *)
   let whileProgressMT tac =
      let rec aux t p =
         let t' = Sequent.goal p in
            if alpha_equal t t' then
               idT p
            else
               prefix_thenMT tac (aux t') p
      in
      let start p =
         let t = Sequent.goal p in
            prefix_thenMT tac (aux t) p
      in
         start

   let repeatMT tac =  whileProgressMT (tryT tac)

   let untilFailMT tac =
      let rec aux p =
          (tryT (prefix_thenMT tac aux)) p
      in
         aux



   (*
    * Repeat a fixed number of times on main subgoals.
    *)
   let repeatMForT i tac =
      if i = 0 then
         idT
      else
         let rec aux i =
            if i = 1 then
               tac
            else
               prefix_thenMT tac (aux (pred i))
         in
            aux i

   (*
    * Sequence tactics on main subgoals.
    *)
   let rec seqOnMT = function
      [tac] ->
         tac
    | tac::tactl ->
         prefix_thenMT tac (seqOnMT tactl)
    | [] ->
         idT

   (*
    * Make sure no main subgoals.
    *)
   let completeMT tac =
      prefix_thenMT tac (failWithT "completeMT")

   (*
    * Note changes of label, as well as changes in sequent.
    *)
   let labProgressT tac =
      let aux p =
         let t = Sequent.goal p in
         let lab = Sequent.label p in
         let aux' p' =
            match p' with
               [p''] ->
                  let t' = Sequent.goal p'' in
                  let lab' = Sequent.label p'' in
                     [(if alpha_equal t t' & lab = lab' then
                          idT
                       else
                          failWithT "progressT") p'']
             | _ ->
                  List.map idT p'
         in
            prefix_thenFLT tac aux' p
      in
         aux

   (************************************************************************
    * HYP AND CLAUSE                                                       *
    ************************************************************************)

   (*
    * Renumbering.
    *)
   let onClauseT i tac p =
      tac i p

   let onHypT = onClauseT

   let onConclT tac = tac 0

   (*
    * Repeat tactic on all subgoals.
    *)
   let onClausesT clauses tac =
      let rec aux = function
         [i] ->
            onClauseT i tac
       | i::t ->
            prefix_thenT (onClauseT i tac) (aux t)
       | [] ->
            idT
      in
         aux clauses

   let onHypsT = onClausesT

   (*
    * Repeat tactic on main subgoals.
    *)
   let onMClausesT clauses tac =
      let rec aux = function
         [i] ->
            onClauseT i tac
       | i::t ->
            prefix_thenMT (onClauseT i tac) (aux t)
       | [] ->
            idT
      in
         aux clauses

   let onMHypsT = onMClausesT

   (*
    * Work on all hyps.
    *)
   let onAllHypsT tac p =
      let rec aux i =
         if i = 1 then
            tac i
         else if i > 1 then
            prefix_thenT (tac i) (aux (i - 1))
         else
            idT
      in
         aux (Sequent.hyp_count p) p

   (*
    * Include conclusion.
    *)
   let onAllClausesT tac =
      prefix_thenT (onAllHypsT tac) (onConclT tac)

   (*
    * Try forms.
    *)
   let tryOnAllHypsT tac =
      onAllHypsT (function i -> tryT (tac i))

   let tryOnAllClausesT tac =
      onAllClausesT (function i -> tryT (tac i))

   (*
    * Labelled forms.
    *)
   let onAllMHypsT tac p =
      let rec aux i =
         if i = 1 then
            tac i
         else if i > 1 then
            prefix_thenMT (tac i) (aux (i - 1))
         else
            idT
      in
         aux (Sequent.hyp_count p) p

   let onAllMClausesT tac =
      prefix_thenMT (onAllMHypsT tac) (onConclT tac)

   let tryOnAllMHypsT tac =
      onAllMHypsT (function i -> tryT (tac i))

   let tryOnAllMClausesT tac =
      onAllMClausesT (function i -> tryT (tac i))


   (*
    * These tactics are useful for trivial search.
    *)
   let onSomeAssumT tac p =
      let rec some i assums p =
         match assums with
            [_] ->
               tac i p
          | _ :: assums ->
               prefix_orelseT (tac i) (some (i + 1) assums) p
          | [] ->
               raise (RefineError ("onSomeAssumT", StringError "no assumptions"))
      in
      let _, assums = dest_msequent (Sequent.msequent p) in
         some 1 assums p

   (*
    * Make sure one of the hyps works.
    *)
   let rec onSomeHypAux tac i p =
      if i = 1 then
         tac 1 p
      else if i > 1 then
         prefix_orelseT (tac i) (onSomeHypAux tac (pred i)) p
      else
         idT p

   let onSomeHypT tac p =
      onSomeHypAux tac (Sequent.hyp_count p) p

   (*
    * Variable name addressing.
    *)
   let onVarT v tac p =
      let i =
         try Sequent.get_decl_number p v with
            Not_found ->
               raise (RefineError ("onVarT", StringStringError (v, "variable not found")))
      in
         tac i p

   (************************************************************************
    * ARGUMENTS                                                            *
    ************************************************************************)

   let wrapT         = TacticInternal.wrapT

   let withTermT     = TacticInternal.withTermT
   let withTermListT = TacticInternal.withTermListT
   let withTypeT     = TacticInternal.withTypeT
   let withBoolT     = TacticInternal.withBoolT
   let withStringT   = TacticInternal.withStringT
   let withIntT      = TacticInternal.withIntT
   let withSubstT    = TacticInternal.withSubstT

   (*
    * Term arguments.
    *)
   let withTermsT = withTermListT "with"
   let withT t = withTermsT [t]

   (*
    * Other arguments.
    *)
   let usingT    = withSubstT
   let atT       = withTypeT "univ"
   let selT      = withIntT  "sel"
   let altT      = withBoolT "alt" true
   let thinningT = withBoolT "thin"

   let get_with_args p =
      Sequent.get_term_list_arg p "with"

   let get_with_arg p =
      match get_with_args p with
         t :: _ ->
            t
       | [] ->
            raise (RefineError ("get_with_arg", StringError "no arguments"))

   let get_univ_arg arg =
      Sequent.get_type_arg arg "univ"

   let get_sel_arg arg =
      Sequent.get_int_arg arg "sel"

   let get_thinning_arg arg =
      try Sequent.get_bool_arg arg "thin" with
         RefineError _ ->
            true

   let get_alt_arg arg =
      try Sequent.get_bool_arg arg "alt" with
         RefineError _ ->
            false
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "camlp4o.run"
 * End:
 * -*-
 *)
