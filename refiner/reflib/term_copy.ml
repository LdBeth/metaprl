(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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
 * Modified by: Eli Barzilay, Alexey Nogin
 *)

open Nl_debug
open Printf

open Opname

open Termmod_sig
open Memo

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

module MakeTermCopy (FromTerm : TermModuleSig) (ToTerm : TermModuleSig)
=
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type to_term = TTerm of ToTerm.TermType.term' | TSeq of ToTerm.TermType.esequent

   type t =
      { copy_level_var : (t,
                          FromTerm.TermType.level_exp_var,
                          ToTerm.TermType.level_exp_var',
                          ToTerm.TermType.level_exp_var) Memo.t;
        copy_level     : (t,
                          FromTerm.TermType.level_exp,
                          ToTerm.TermType.level_exp',
                          ToTerm.TermType.level_exp) Memo.t;
        copy_param     : (t,
                          FromTerm.TermType.param,
                          ToTerm.TermType.param',
                          ToTerm.TermType.param) Memo.t;
        copy_operator  : (t,
                          FromTerm.TermType.operator,
                          ToTerm.TermType.operator',
                          ToTerm.TermType.operator) Memo.t;
        copy_term      : (t,
                          FromTerm.TermType.term,
                          to_term,
                          ToTerm.TermType.term) Memo.t;
        copy_bterm     : (t,
                          FromTerm.TermType.bound_term,
                          ToTerm.TermType.bound_term',
                          ToTerm.TermType.bound_term) Memo.t
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = List_util.compare_eq

   (*
    * Comparison functions.
    *)
   let compare_level_var { ToTerm.TermType.le_var = v1; ToTerm.TermType.le_offset = offset1 }
                         { ToTerm.TermType.le_var = v2; ToTerm.TermType.le_offset = offset2 } =
      v1 == v2 & offset1 == offset2

   let compare_level { ToTerm.TermType.le_const = const1; ToTerm.TermType.le_vars = vars1 }
                     { ToTerm.TermType.le_const = const2; ToTerm.TermType.le_vars = vars2 } =
      const1 = const2 & list_mem_eq vars1 vars2

   let compare_param param1 param2 =
      match param1, param2 with
         ToTerm.TermType.Number    n1,         ToTerm.TermType.Number    n2         -> Nl_num.eq_num n1 n2
       | ToTerm.TermType.String    s1,         ToTerm.TermType.String    s2         -> s1 = s2
       | ToTerm.TermType.Token     s1,         ToTerm.TermType.Token     s2         -> s1 = s2
       | ToTerm.TermType.Level     l1,         ToTerm.TermType.Level     l2         -> l1 == l2
       | ToTerm.TermType.Var       v1,         ToTerm.TermType.Var       v2         -> v1 = v2
       | ToTerm.TermType.MNumber   s1,         ToTerm.TermType.MNumber   s2         -> s1 = s2
       | ToTerm.TermType.MString   s1,         ToTerm.TermType.MString   s2         -> s1 = s2
       | ToTerm.TermType.MToken    s1,         ToTerm.TermType.MToken    s2         -> s1 = s2
       | ToTerm.TermType.MLevel    s1,         ToTerm.TermType.MLevel    s2         -> s1 = s2
       | ToTerm.TermType.MVar      s1,         ToTerm.TermType.MVar      s2         -> s1 = s2
       | ToTerm.TermType.ObId      oid1,       ToTerm.TermType.ObId      oid2       -> list_mem_eq oid1 oid2
       | ToTerm.TermType.ParamList params1,    ToTerm.TermType.ParamList params2    -> list_mem_eq params1 params2
       | ToTerm.TermType.MSum      (p11, p12), ToTerm.TermType.MSum      (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MDiff     (p11, p12), ToTerm.TermType.MDiff     (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MProduct  (p11, p12), ToTerm.TermType.MProduct  (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MQuotient (p11, p12), ToTerm.TermType.MQuotient (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MRem      (p11, p12), ToTerm.TermType.MRem      (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MLessThan (p11, p12), ToTerm.TermType.MLessThan (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MEqual    (p11, p12), ToTerm.TermType.MEqual    (p21, p22) -> p11 == p12 & p21 == p22
       | ToTerm.TermType.MNotEqual (p11, p12), ToTerm.TermType.MNotEqual (p21, p22) -> p11 == p12 & p21 == p22
       | _ -> false

   let compare_operator { ToTerm.TermType.op_name = opname1; ToTerm.TermType.op_params = params1 }
                        { ToTerm.TermType.op_name = opname2; ToTerm.TermType.op_params = params2 } =
      opname1 == opname2 & list_mem_eq params1 params2

   let compare_term { ToTerm.TermType.term_op = op1; ToTerm.TermType.term_terms = bterms1 }
                    { ToTerm.TermType.term_op = op2; ToTerm.TermType.term_terms = bterms2 } =
      op1 == op2 & list_mem_eq bterms1 bterms2

   let rec compare_hyps hyp1 hyp2 i =
      (i < 0) ||
      ((match (ToTerm.Term.SeqHyp.get hyp1 i), (ToTerm.Term.SeqHyp.get hyp2 i) with
           ToTerm.TermType.Hypothesis (v1,t1),  ToTerm.TermType.Hypothesis (v2,t2)   -> v1 = v2 && t1 == t2
         | ToTerm.TermType.Context    (v1,ts1), ToTerm.TermType.Context    (v2, ts2) -> v1 = v2 && list_mem_eq ts1 ts2
         | _ -> false) &&
       (compare_hyps hyp1 hyp2 (pred i)))

   let rec compare_goals goal1 goal2 i =
      (i<0) ||
      (((ToTerm.Term.SeqGoal.get goal1 i) == (ToTerm.Term.SeqGoal.get goal2 i)) &&
       (compare_goals goal1 goal2 (pred i)))

   let compare_tterm t1 t2 =
      match (t1,t2) with
         TTerm t1, TTerm t2 ->
            compare_term t1 t2
       | TSeq { ToTerm.TermType.sequent_args = arg1; ToTerm.TermType.sequent_hyps = hyp1; ToTerm.TermType.sequent_goals = goal1},
         TSeq { ToTerm.TermType.sequent_args = arg2; ToTerm.TermType.sequent_hyps = hyp2; ToTerm.TermType.sequent_goals = goal2} ->
            (arg1 == arg2) &&
            (ToTerm.Term.SeqHyp.length hyp1 = ToTerm.Term.SeqHyp.length hyp2) &&
            (compare_hyps hyp1 hyp2 (ToTerm.Term.SeqHyp.length hyp1 - 1)) &&
            (ToTerm.Term.SeqGoal.length goal1 = ToTerm.Term.SeqGoal.length goal2) &&
            (compare_goals goal1 goal2 (ToTerm.Term.SeqGoal.length goal1 - 1))
       | _ -> false

   let compare_bterm { ToTerm.TermType.bvars = bvars1; ToTerm.TermType.bterm = bterm1 }
       { ToTerm.TermType.bvars = bvars2; ToTerm.TermType.bterm = bterm2 } =
      bvars1 = bvars2 & bterm1 == bterm2

   (*
    * Copy functions.
    *)
   let make_hyp info hyps i =
      match FromTerm.Term.SeqHyp.get hyps i with
         FromTerm.TermType.Hypothesis (v, t) -> ToTerm.TermType.Hypothesis (v, Memo.apply info.copy_term info t)
       | FromTerm.TermType.Context (v, trms) -> ToTerm.TermType.Context (v, List.map (Memo.apply info.copy_term info) trms)

   let make_goal info goals i =
      Memo.apply info.copy_term info (FromTerm.Term.SeqGoal.get goals i)

   let make_term info t =
      if FromTerm.TermMan.is_sequent_term t then
         let { FromTerm.TermType.sequent_args = args;
               FromTerm.TermType.sequent_hyps = hyps;
               FromTerm.TermType.sequent_goals = goals } = (FromTerm.TermMan.explode_sequent t)
         in
            TSeq
               { ToTerm.TermType.sequent_args =
                    Memo.apply info.copy_term info args;
                 ToTerm.TermType.sequent_hyps =
                    ToTerm.Term.SeqHyp.init (FromTerm.Term.SeqHyp.length hyps) (make_hyp info hyps);
                 ToTerm.TermType.sequent_goals =
                    ToTerm.Term.SeqGoal.init (FromTerm.Term.SeqGoal.length goals) (make_goal info goals)
               }
      else
         let { FromTerm.TermType.term_op = op; FromTerm.TermType.term_terms = bterms } = FromTerm.Term.dest_term t
         in
            TTerm
               { ToTerm.TermType.term_op = Memo.apply info.copy_operator info op;
                 ToTerm.TermType.term_terms = List.map (Memo.apply info.copy_bterm info) bterms }

   let do_make_term _ = function
      TTerm t -> ToTerm.Term.make_term t
    | TSeq s -> ToTerm.TermMan.mk_sequent_term s

   let make_bterm info bterm =
      let { FromTerm.TermType.bvars = bvars; FromTerm.TermType.bterm = bterm } = FromTerm.Term.dest_bterm bterm
      in
         { ToTerm.TermType.bvars = bvars;
           ToTerm.TermType.bterm = Memo.apply info.copy_term info bterm
         }

   let make_operator info op =
      let { FromTerm.TermType.op_name = opname; FromTerm.TermType.op_params = params } = FromTerm.Term.dest_op op
      in
         { ToTerm.TermType.op_name = normalize_opname opname;
           ToTerm.TermType.op_params = List.map (Memo.apply info.copy_param info) params
         }

   let make_param info param =
      match FromTerm.Term.dest_param param with
         FromTerm.TermType.Number n1 ->    ToTerm.TermType.Number n1
       | FromTerm.TermType.String s1 ->    ToTerm.TermType.String s1
       | FromTerm.TermType.Token s1 ->     ToTerm.TermType.Token s1
       | FromTerm.TermType.Level l1 ->     ToTerm.TermType.Level (Memo.apply info.copy_level info l1)
       | FromTerm.TermType.Var v1 ->       ToTerm.TermType.Var v1
       | FromTerm.TermType.MNumber s1 ->   ToTerm.TermType.MNumber s1
       | FromTerm.TermType.MString s1 ->   ToTerm.TermType.MString s1
       | FromTerm.TermType.MToken s1 ->    ToTerm.TermType.MToken s1
       | FromTerm.TermType.MLevel s1 ->    ToTerm.TermType.MLevel s1
       | FromTerm.TermType.MVar s1 ->      ToTerm.TermType.MVar s1
       | FromTerm.TermType.ObId oid1 ->    ToTerm.TermType.ObId (List.map (Memo.apply info.copy_param info) oid1)
       | FromTerm.TermType.ParamList p1 -> ToTerm.TermType.ParamList (List.map (Memo.apply info.copy_param info) p1)
       | FromTerm.TermType.MSum (p11, p21) ->
            ToTerm.TermType.MSum (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MDiff (p11, p21) ->
            ToTerm.TermType.MDiff (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MProduct (p11, p21) ->
            ToTerm.TermType.MProduct (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MQuotient (p11, p21) ->
            ToTerm.TermType.MQuotient (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MRem (p11, p21) ->
            ToTerm.TermType.MRem (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MLessThan (p11, p21) ->
            ToTerm.TermType.MLessThan (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MEqual (p11, p21) ->
            ToTerm.TermType.MEqual (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FromTerm.TermType.MNotEqual (p11, p21) ->
            ToTerm.TermType.MNotEqual (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)

   let make_level info level =
      let { FromTerm.TermType.le_const = c; FromTerm.TermType.le_vars = vars } = FromTerm.Term.dest_level level
      in
         { ToTerm.TermType.le_const = c;
           ToTerm.TermType.le_vars = List.map (Memo.apply info.copy_level_var info) vars
         }

   let make_level_var info lvar =
      let { FromTerm.TermType.le_var = var; FromTerm.TermType.le_offset = offset } = FromTerm.Term.dest_level_var lvar
      in
         { ToTerm.TermType.le_var = var;
           ToTerm.TermType.le_offset = offset
         }

   (*
    * Create the memo tables.
    *)
   let create () =
      { copy_level_var = Memo.create (**)
           make_level_var
           (fun _ t -> ToTerm.Term.make_level_var t)
           compare_level_var;
        copy_level     = Memo.create (**)
           make_level
           (fun _ t -> ToTerm.Term.make_level t)
           compare_level;
        copy_param     = Memo.create (**)
           make_param
           (fun _ t -> ToTerm.Term.make_param t)
           compare_param;
        copy_operator  = Memo.create (**)
           make_operator
           (fun _ t -> ToTerm.Term.make_op t)
           compare_operator;
        copy_term      = Memo.create (**)
           make_term
           do_make_term
           compare_tterm;
        copy_bterm     = Memo.create (**)
           make_bterm
           (fun _ t -> ToTerm.Term.make_bterm t)
           compare_bterm
      }

   (*
    * Basic term application.
    *)
   let copy_term info t =
      Memo.apply info.copy_term info t

   (*
    * Meta terms.
    * We don't share at the meta-term level.
    *)
   let rec copy_meta_term info = function
      FromTerm.TermType.MetaTheorem t ->
         ToTerm.TermType.MetaTheorem (Memo.apply info.copy_term info t)
    | FromTerm.TermType.MetaImplies (t1, t2) ->
         ToTerm.TermType.MetaImplies (copy_meta_term info t1,
                                         copy_meta_term info t2)
    | FromTerm.TermType.MetaFunction (t1, mt1, mt2) ->
         ToTerm.TermType.MetaFunction (Memo.apply info.copy_term info t1,
                                          copy_meta_term info mt1,
                                          copy_meta_term info mt2)
    | FromTerm.TermType.MetaIff (mt1, mt2) ->
         ToTerm.TermType.MetaIff (copy_meta_term info mt1,
                                     copy_meta_term info mt2)


   (*
    * Single-use versions.
    *)
   let copy_term_single t =
      copy_term (create ()) t

   let copy_meta_term_single t =
      copy_meta_term (create ()) t
end

(*
 * Common cases.
 *)
module NormalizeTerm =
   MakeTermCopy (Refiner_std_verb.Refiner) (Refiner.Refiner)

module DenormalizeTerm =
   MakeTermCopy (Refiner.Refiner) (Refiner_std_verb.Refiner)

type normalize = NormalizeTerm.t
type denormalize = DenormalizeTerm.t

let create_norm = NormalizeTerm.create
let create_denorm = DenormalizeTerm.create

let normalize_term = NormalizeTerm.copy_term
let normalize_meta_term = NormalizeTerm.copy_meta_term
let denormalize_term = DenormalizeTerm.copy_term
let denormalize_meta_term = DenormalizeTerm.copy_meta_term

let normalize_term_single = NormalizeTerm.copy_term_single
let normalize_meta_term_single = NormalizeTerm.copy_meta_term_single
let denormalize_term_single = DenormalizeTerm.copy_term_single
let denormalize_meta_term_single = DenormalizeTerm.copy_meta_term_single

(*
let normalize_term info t =
   if !debug_memo then
      eprintf "Normalizing:%t" eflush;
   let t = NormalizeTerm.copy_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_meta_term info t =
   if !debug_memo then
      eprintf "Normalizing:%t" eflush;
   let t = NormalizeTerm.copy_meta_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_term info t =
   if !debug_memo then
      begin
         eprintf "Denormalizing: ";
         flush stderr;
         Simple_print.prerr_simple_term t;
         eflush stderr
      end;
   let t = DenormalizeTerm.copy_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_meta_term info t =
   if !debug_memo then
      eprintf "Deormalizing:%t" eflush;
   let t = DenormalizeTerm.copy_meta_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_term_single t =
   if !debug_memo then
      eprintf "Normalizing Single:%t" eflush;
   let t = NormalizeTerm.copy_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_meta_term_single t =
   if !debug_memo then
      eprintf "Normalizing Single:%t" eflush;
   let t = NormalizeTerm.copy_meta_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_term_single t =
   if !debug_memo then
      begin
         eprintf "Denormalizing: Single:";
         flush stderr;
         Simple_print.prerr_simple_term t;
         eflush stderr
      end;
   let t = DenormalizeTerm.copy_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_meta_term_single t =
   if !debug_memo then
      eprintf "Deormalizing: Single%t" eflush;
   let t = DenormalizeTerm.copy_meta_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
