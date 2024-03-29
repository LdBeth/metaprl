(*
 * Some basic tacticals.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and Caltech
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

open Lm_debug

open Term_sig
open Refiner.Refiner
open Refiner.Refiner.Refine
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError
open Refiner.Refiner.TermType

open Tactic_boot
open Sequent_boot
(* open Tactic_boot_sig *)

(*
 * Debug statement.
 *)
let () =
   show_loading "Loading Tacticals_boot"

(* unused
let debug_subgoals =
   create_debug (**)
      { debug_name = "subgoals";
        debug_description = "Report subgoals observed with may be some additional info";
        debug_value = false
      }
*)

(* let debug_profile_tactics = load_debug "profile_tactics" *)

module Tacticals =
struct
   module TacticalsTypes = TacticInternalType

   (************************************************************************
    * TRIVIAL TACTICS                                                      *
    ************************************************************************)

   (* Trivial tactics *)
   let idT = TacticInternal.idT
   let timingT = TacticInternal.timingT
   let finalT = TacticInternal.finalT
   let nthAssumT = TacticInternal.nthAssumT
   let cutT = TacticInternal.cutT
   let funT = TacticInternal.funT
   let argfunT = TacticInternal.argfunT
   let argfun2T = TacticInternal.argfun2T

   let failT =
      funT (fun _ -> raise (RefineError ("failT", StringError "Fail")))

   let failWithT s =
      funT (fun _ -> raise (RefineError ("failWithT", StringError s)))

   (************************************************************************
    * SEQUENCING                                                           *
    ************************************************************************)

   let prefix_orelseT = TacticInternal.prefix_orelseT
   let prefix_andalsoT = TacticInternal.prefix_thenT
   let prefix_thenT = TacticInternal.prefix_thenT
   let prefix_thenLocalLabelT = TacticInternal.prefix_thenLocalLabelT
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
         raise (RefineError ("firstT", StringError "no tactics"))

   let prefix_then_OnFirstT =
      let rec dup_id = function
         x::t ->
            idT :: dup_id t
       | [] ->
            []
      in let aux tac2 = function
         p::l ->
            tac2 :: dup_id l
       | [] ->
            []
      in
         fun tac1 tac2 -> prefix_thenFLT tac1 (aux tac2)

   let prefix_then_OnLastT tac1 tac2 =
      let rec aux = function
         [p] ->
            [tac2]
       | p::t ->
            idT :: aux t
       | [] ->
            []
      in
         prefix_thenFLT tac1 aux

   let prefix_then_OnSameConclT tac1 tac2 =
      let first p =
         let t = Sequent.concl p in
         let second p =
            if alpha_equal t (Sequent.concl p) then
                tac2
             else
                idT
         in
            prefix_thenT tac1 (funT second)
      in
         funT first

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
         let rec tac' = function
            [] -> []
          | p' :: pp ->
               if Sequent.tactic_arg_alpha_equal p' p then
                  raise (RefineError ("progressT", StringError "no progress"))
               else
                  idT :: (tac' pp)
         in
            prefix_thenFLT tac tac'
      in
         funT aux

   (*
    * Repeat, spreading out over subgoals.
    * Stop if there is no progress.
    *)
   let whileProgressT tac =
      let rec aux t p =
         let t' = Sequent.goal p in
            if alpha_equal t t' then
               idT
            else
               prefix_thenT tac (funT (aux t'))
      in
      let start p =
         let t = Sequent.goal p in
            prefix_thenT tac (funT (aux t))
      in
         funT start

   (*
    * Repeat, spreading out over subgoals.
    * Stop when the tactic fails.
    *)
   let untilFailT tac =
      let rec aux p =
          tryT (prefix_thenT tac (funT aux))
      in
         funT aux

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
               prefix_thenT tac (aux (pred i))
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
            let rec aux = function
               tac::tactl ->
                  funT (fun p ->
                        (* Recurse through the tactics *)
                        let t' = Sequent.concl p in
                           if alpha_equal t t' then
                              prefix_thenT tac (aux tactl)
                           else
                              idT)
             | [] -> idT
            in
               aux tacs
         in
            funT start

   (************************************************************************
    * CONDITIONALS                                                         *
    ************************************************************************)

   (*
    * Conditionals.
    *)
   let ifT pred tac1 tac2 =
      funT (fun p -> if pred p then tac1 else tac2)

   let ifOnConclT pred =
      ifT (fun p -> pred (Sequent.concl p))

   let ifOnHypT pred tac1 tac2 i =
      funT (fun p ->
         (if pred (Sequent.nth_hyp p i) then tac1 else tac2) i)

   let ifThenT pred tac1 =
      ifT (fun p -> pred (Sequent.goal p)) tac1 idT

   let ifThenOnConclT pred tac =
      let failT = failWithT "ifThenOnConclT" in
         ifOnConclT pred tac failT

   let ifThenOnHypT pred tac i =
      let failT _ = failWithT "ifThenOnHypT" in
         ifOnHypT pred tac failT i

   let whileT pred tac =
      let rec aux p =
         tryT (ifThenT pred (prefix_thenT (progressT tac) (funT aux)))
      in
         funT aux

   let untilT pred =
      whileT (fun p -> not (pred p))

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
       "base";
       "step";
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

   let keepingLabelT tac =
      funT (fun p -> prefix_thenT tac (addHiddenLabelT (Sequent.label p)))

   (*
    * Conditional on label.
    *)
   let ifLabLT tacs =
      funT (fun p ->
         let lab = Sequent.label p in
            match List.assoc_opt lab tacs with
               Some a -> a
             | None -> idT)

   let ifLabT lab tac1 tac2 =
      funT (fun p -> if lab = Sequent.label p then tac1 else tac2)

   let ifMT tac =
      funT (fun p -> if List.mem (Sequent.label p) main_labels then tac else idT)

   let wfLabel="wf"

   let ifWT tac =
      ifLabT wfLabel tac idT

   let eqLabel="equality"

   let ifET tac =
      ifLabT eqLabel tac idT

   let ifAT tac =
      funT (fun p -> if List.mem (Sequent.label p) main_labels then idT else tac)

   let ifPT tac =
      funT (fun p -> if List.mem (Sequent.label p) predicate_labels then tac else idT)

   (*
    * Label tacticals.
    *)
   let emptyLabel=""

   let ifLabelPredT pred tac1' tac2' = funT (fun p ->
      if pred (Sequent.label p) then
         tac1'
      else
         tac2')

   let isEmptyOrMainLabel l =
      l = emptyLabel || List.mem l main_labels

   let isAuxLabel l = not (isEmptyOrMainLabel l)

(* unused
   let isWFLabel l =
      l = wfLabel

   let isEqualityLabel l =
      l = eqLabel

   let isPredicateLabel l =
      List.mem l predicate_labels
*)

   let prefix_thenMT tac1 tac2 =
      prefix_thenLocalLabelT tac1 (ifLabelPredT isEmptyOrMainLabel tac2 idT)

(* unused
   let prefix_thenMElseT tac1 tac2 tac3 =
      prefix_thenLocalLabelT tac1 (ifLabelPredT isEmptyOrMainLabel tac2 tac3)
*)

   let prefix_thenAT tac1 tac2 =
      prefix_thenLocalLabelT tac1 (ifLabelPredT isAuxLabel tac2 idT)

   let prefix_thenWT tac1 tac2 =
      prefix_thenLocalLabelT tac1 (ifWT tac2)

   let prefix_thenET tac1 tac2 =
      prefix_thenLocalLabelT tac1 (ifET tac2)

   let prefix_thenPT tac1 tac2 =
      prefix_thenLocalLabelT tac1 (ifPT tac2)

   let prefix_thenLabLT tac1 tacs = funT (fun p ->
      let prefer l1 l2 =
         if l2=emptyLabel then l1
         else l2 in
      let restoreHiddenLabelT = argfunT (fun l p ->
         addHiddenLabelT (prefer l (Sequent.label p)))
      in
      let label = Sequent.label p in
      prefix_thenT
        (prefix_thenT
                (prefix_thenT (addHiddenLabelT emptyLabel)
                                                  tac1)
                (ifLabLT tacs))
                        (restoreHiddenLabelT label))

   (*
    * Apply the tactic list only to the specified subgoals.
    *)
   let thenLLT pred tac1 tacs =
      let rec aux ts = function
         p::ps ->
            if pred (Sequent.label p) then
               match ts with
                  tac::tactl ->
                     tac::(aux tactl ps)
                | [] ->
                     raise (RefineError ("thenMLT", StringError "argument mismatch"))
            else
               idT::(aux ts ps)
       | [] ->
            match ts with
               [] -> ts
             | _ ->
                  raise (RefineError ("thenMLT", StringError "argument mismatch"))
      in
         prefix_thenFLT tac1 (aux tacs)

   let prefix_thenMLT =
      thenLLT isEmptyOrMainLabel

   let prefix_thenALT =
      thenLLT isAuxLabel

   (************************************************************************
    * LABEL PROGRESS                                                       *
    ************************************************************************)

   (*
    * Repeat only on main subgoals.
    *)
   let whileProgressMT tac =
      let rec aux t =
         funT (fun p ->
            let t' = Sequent.goal p in
               if alpha_equal t t' then idT else prefix_thenMT tac (aux t')
         )
      in
      let start p =
         let t = Sequent.goal p in
            prefix_thenMT tac (aux t)
      in
         funT start

   let repeatMT tac =  whileProgressMT (tryT tac)

   let untilFailMT tac =
      let rec aux p =
          tryT (prefix_thenMT tac (funT aux))
      in
         funT aux

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
                     [(if alpha_equal t t' && lab = lab' then
                          idT
                       else
                          failWithT "progressT")]
             | _ ->
                  List.map (fun _ -> idT) p'
         in
            prefix_thenFLT tac aux'
      in
         funT aux

   (************************************************************************
    * HYP AND CLAUSE                                                       *
    ************************************************************************)

   (*
    * Renumbering.
    *)
   let onClauseT i tac =
      tac i

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

   let onAllT baseT thenT tac i hyps =
      if i = 0 then baseT else
         let rec aux i =
            if i = 1 then match Term.SeqHyp.get hyps 0 with
                             Hypothesis _ ->
                                tac i
                           | Context _ ->
                                baseT
            else
               let i' = pred i in
                  match Term.SeqHyp.get hyps i' with
                     Hypothesis _ -> thenT (tac i) (aux i')
                   | Context _ -> aux i'
         in aux i

   let rec onAllCumulativeT_aux thenT tac i = funT (fun p ->
      if i < Sequent.hyp_count p then
         let i' = succ i in
         let hyps = (Sequent.explode_sequent_arg p).sequent_hyps in
            match Term.SeqHyp.get hyps i with
               Hypothesis (_, t) ->
                  thenT (tac i') (onAllCumulativeT_aux thenT tac i')
             | Context _ ->
                  onAllCumulativeT_aux thenT tac i'
      else
         idT)


   (*
    * Work on all hyps.
    *)
   let onAllHypsT tac = funT (fun p ->
      let hyps = (Sequent.explode_sequent_arg p).sequent_hyps in
         onAllT idT prefix_thenT tac (Sequent.hyp_count p) hyps)

   let onAllCumulativeHypsT tac = onAllCumulativeT_aux prefix_thenT tac 0

   (*
    * Include conclusion.
    *)
   let onAllClausesT tac =
      prefix_thenT (onAllHypsT tac) (onConclT tac)

   (*
    * Try forms.
    *)
   let tryOnClausesT clauses tac =
      onClausesT clauses (fun i -> tryT (tac i))

   let tryOnHypsT = tryOnClausesT

   let tryOnAllHypsT tac =
      onAllHypsT (fun i -> tryT (tac i))

   let tryOnAllCumulativeHypsT tac =
      onAllCumulativeHypsT (fun i -> tryT (tac i))

   let tryOnAllClausesT tac =
      onAllClausesT (fun i -> tryT (tac i))

   (*
    * Labelled forms.
    *)
   let onAllMHypsT tac =
      funT (fun p ->
            let hyps = (Sequent.explode_sequent_arg p).sequent_hyps in
               onAllT idT prefix_thenMT tac (Sequent.hyp_count p) hyps)

   let onAllMCumulativeHypsT tac = onAllCumulativeT_aux prefix_thenMT tac 0

   let tryOnAllMCumulativeHypsT tac =
      onAllMCumulativeHypsT (fun i -> tryT (tac i))

   let tryOnAllMHypsT tac =
      onAllMHypsT (fun i -> tryT (tac i))

   let tryOnAllMClausesT tac =
      prefix_thenMT (tryOnAllMHypsT tac) (tryT (onConclT tac))

   (*
    * These tactics work with assumptions.
    *)
   let onAllAssumT tac =
      let rec do_all i assums =
         funT (fun p ->
            match assums with
               _ :: assums ->
                  prefix_thenT (tac i) (do_all (succ i) assums)
             | [] ->
                  idT
         )
      in
         funT (fun p -> do_all 1 (snd (dest_msequent (Sequent.msequent p))))

   let onAllMClausesOfAssumT tac assum =
      funT (fun p ->
            let assumption = List.nth (snd (dest_msequent (Sequent.msequent p))) assum in
            let hyps = (TermMan.explode_sequent assumption).sequent_hyps in
               prefix_thenMT
               (onAllT idT prefix_thenMT (tac assum) (Sequent.assum_hyp_count p assum) hyps)
               (onConclT (tac assum))
      )

   (*
    * Labeled form
    *)
   let onAllMAssumT tac =
      let rec do_all i assums =
         funT (fun p ->
            match assums with
               assum :: assums ->
                  prefix_thenMT (tac i) (do_all (succ i) assums)
             | [] ->
                  idT
         )
      in
         funT (fun p -> do_all 1 (snd (dest_msequent (Sequent.msequent p))))

   (*
    * These tactics are useful for trivial search.
    *)
   let onSomeAssumT tac =
      funT (fun p ->
            let num = Sequent.num_assums p in
               if num < 1 then
                  raise (RefineError ("onSomeAssumT", StringError "no assumptions"));
               let rec some i =
                  if i = num then
                     tac i
                  else
                     prefix_orelseT (tac i) (some (succ i))
               in
                  some 1)

   (*
    * Make sure one of the hyps works.
    *)
   let onSomeHypT tac = funT (fun p ->
      let hyps = (Sequent.explode_sequent_arg p).sequent_hyps in
         onAllT failT prefix_orelseT tac (Sequent.hyp_count p) hyps)

   let onAnyHypT tac = funT (fun p ->
      let hyps = (Sequent.explode_sequent_arg p).sequent_hyps in
         onAllT idT prefix_orelseT tac (Sequent.hyp_count p) hyps)

   (************************************************************************
    * ARGUMENTS                                                            *
    ************************************************************************)

   let forceT            = TacticInternal.forceT

   let addTermT          = TacticInternal.addTermT
   let addTermListT      = TacticInternal.addTermListT
   let addTypeT          = TacticInternal.addTypeT
   let addBoolT          = TacticInternal.addBoolT
   let addStringT        = TacticInternal.addStringT
   let addOptionT        = TacticInternal.addOptionT
   let addIntT           = TacticInternal.addIntT

   let withTermT         = TacticInternal.withTermT
   let withTermListT     = TacticInternal.withTermListT
   let withTypeT         = TacticInternal.withTypeT
   let withBoolT         = TacticInternal.withBoolT
   let withStringT       = TacticInternal.withStringT
   let withOptionT       = TacticInternal.withOptionT
   let withIntT          = TacticInternal.withIntT

   let removeTermT       = TacticInternal.removeTermT
   let removeTermListT   = TacticInternal.removeTermListT
   let removeTypeT       = TacticInternal.removeTypeT
   let removeBoolT       = TacticInternal.removeBoolT
   let removeStringT     = TacticInternal.removeStringT
(* unused
   let removeStringValT  = TacticInternal.removeStringValT
*)
   let removeIntT        = TacticInternal.removeIntT
   let removeOptionT     = TacticInternal.removeOptionT

   let withoutTermT      = TacticInternal.withoutTermT
   let withoutTermListT  = TacticInternal.withoutTermListT
   let withoutTypeT      = TacticInternal.withoutTypeT
   let withoutBoolT      = TacticInternal.withoutBoolT
   let withoutStringT    = TacticInternal.withoutStringT
(* unused
   let withoutStringValT = TacticInternal.withoutStringValT
*)
   let withoutIntT       = TacticInternal.withoutIntT
   let withoutOptionT    = TacticInternal.withoutOptionT

   (*
    * Subgoal checking.
    *)
   let subgoalsCheckT    = TacticInternal.subgoalsCheckT

   (*
    * Term arguments.
    *)
   let withTermsT = withTermListT "with"
   let withT t = withTermsT [t]
   let addT t = addTermListT "with" [t]

   (*
    * Other arguments.
    *)
   let atT       = withTypeT   "univ"
   let selT      = withIntT    "sel"
   let altT      = withBoolT   "alt" true
   let thinningT = withBoolT   "thin"

   let get_option_args = Sequent.get_option_args
   let set_option_args = Sequent.set_option_args

   let get_with_args p =
      Sequent.get_term_list_arg p "with"

   let get_with_arg p =
      match get_with_args p with
         Some (t :: _) -> Some t
       | _ -> None

   let get_univ_arg arg =
      Sequent.get_type_arg arg "univ"

   let get_sel_arg arg =
      Sequent.get_int_arg arg "sel"

   let get_thinning_arg arg =
      match Sequent.get_bool_arg arg "thin" with Some v -> v | None -> true

   let get_alt_arg arg =
      match Sequent.get_bool_arg arg "alt" with Some v -> v | None -> false

(* disabled
   let table = Hashtbl.create 19

   let profileWrapT args tac =
      let arg =
         match args with
            NoneArgList s
          | StringArgList (s, _)
          | TermArgList (s, _)
          | BoolArgList (s, _)
          | IntArgList (s, _)
          | IntIntArgList (s, _, _)
          | TermTermArgList (s, _, _)
          | TermBoolArgList (s, _, _)
          | TermIntArgList (s, _, _)
          | BoolTermArgList (s, _, _)
          | BoolBoolArgList (s, _, _)
          | BoolIntArgList (s, _, _)
          | IntTermArgList (s, _, _)
          | IntBoolArgList (s, _, _)
          | IntStringArgList (s, _, _)
          | BoolStringArgList (s, _, _)
          | StringIntArgList (s, _, _)
          | StringBoolArgList (s, _, _)
          | StringStringArgList (s, _, _)
          | StringTermArgList (s, _, _)
          | TermStringArgList (s, _, _) ->
               s
          | GeneralArgList a ->
               if Array.length a > 0 then
                  match a.(0) with
                     StringArg s -> s
                   | _ -> "unknown"
               else
                  "unknown"
      in
         fun p ->
            (* We use a table of bool refs to avoid counting time spent in recursive calls more that once *)
            (* XXX: WARNING: This is not threads-safe! *)
            let flag =
               try Hashtbl.find table arg
               with Not_found ->
                  let flag = ref true in
                     Hashtbl.add table arg flag;
                     flag
            in
               if !flag then begin
                  flag := false;
                  try
                     let res = timing_wrap arg (TacticInternal.wrapT args tac) p in
                        flag := true;
                        res
                  with
                     exn ->
                        flag := true;
                        raise exn
               end else
                  TacticInternal.wrapT args tac p
*)

   let wrapT = (* XXX: this is not working due to debug argument setting is too late *)
      (* if !debug_profile_tactics then profileWrapT else *) TacticInternal.wrapT

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "camlp4o.run"
 * End:
 * -*-
 *)
