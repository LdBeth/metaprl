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
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module FTerm = FromTerm.Term
   module FType = FromTerm.TermType

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type to_term = TTerm of TType.term' | TSeq of TType.esequent

   type t =
      { copy_level_var : (t, FType.level_exp_var, TType.level_exp_var', TType.level_exp_var) Memo.t;
        copy_level     : (t, FType.level_exp,     TType.level_exp',     TType.level_exp)     Memo.t;
        copy_param     : (t, FType.param,         TType.param',         TType.param)         Memo.t;
        copy_operator  : (t, FType.operator,      TType.operator',      TType.operator)      Memo.t;
        copy_term      : (t, FType.term,          to_term,              TType.term)          Memo.t;
        copy_bterm     : (t, FType.bound_term,    TType.bound_term',    TType.bound_term)    Memo.t
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
   let compare_level_var { TType.le_var = v1; TType.le_offset = offset1 }
                         { TType.le_var = v2; TType.le_offset = offset2 } =
      v1 == v2 & offset1 == offset2

   let compare_level { TType.le_const = const1; TType.le_vars = vars1 }
                     { TType.le_const = const2; TType.le_vars = vars2 } =
      const1 = const2 & list_mem_eq vars1 vars2

   let compare_param param1 param2 =
      match param1, param2 with
         TType.Number    n1,         TType.Number    n2         -> Nl_num.eq_num n1 n2
       | TType.String    s1,         TType.String    s2         -> s1 = s2
       | TType.Token     s1,         TType.Token     s2         -> s1 = s2
       | TType.Level     l1,         TType.Level     l2         -> l1 == l2
       | TType.Var       v1,         TType.Var       v2         -> v1 = v2
       | TType.MNumber   s1,         TType.MNumber   s2         -> s1 = s2
       | TType.MString   s1,         TType.MString   s2         -> s1 = s2
       | TType.MToken    s1,         TType.MToken    s2         -> s1 = s2
       | TType.MLevel    s1,         TType.MLevel    s2         -> s1 = s2
       | TType.MVar      s1,         TType.MVar      s2         -> s1 = s2
       | TType.ObId      oid1,       TType.ObId      oid2       -> list_mem_eq oid1 oid2
       | TType.ParamList params1,    TType.ParamList params2    -> list_mem_eq params1 params2
       | TType.MSum      (p11, p12), TType.MSum      (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MDiff     (p11, p12), TType.MDiff     (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MProduct  (p11, p12), TType.MProduct  (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MQuotient (p11, p12), TType.MQuotient (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MRem      (p11, p12), TType.MRem      (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MLessThan (p11, p12), TType.MLessThan (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MEqual    (p11, p12), TType.MEqual    (p21, p22) -> p11 == p12 & p21 == p22
       | TType.MNotEqual (p11, p12), TType.MNotEqual (p21, p22) -> p11 == p12 & p21 == p22
       | _ -> false

   let compare_operator { TType.op_name = opname1; TType.op_params = params1 }
                        { TType.op_name = opname2; TType.op_params = params2 } =
      opname1 == opname2 & list_mem_eq params1 params2

   let compare_term { TType.term_op = op1; TType.term_terms = bterms1 }
                    { TType.term_op = op2; TType.term_terms = bterms2 } =
      op1 == op2 & list_mem_eq bterms1 bterms2

   let rec compare_hyps hyp1 hyp2 i =
      (i < 0) ||
      ((match (TTerm.SeqHyp.get hyp1 i), (TTerm.SeqHyp.get hyp2 i) with
           TType.Hypothesis (v1,t1),  TType.Hypothesis (v2,t2)   -> v1 = v2 && t1 == t2
         | TType.Context    (v1,ts1), TType.Context    (v2, ts2) -> v1 = v2 && list_mem_eq ts1 ts2
         | _ -> false) &&
       (compare_hyps hyp1 hyp2 (pred i)))

   let rec compare_goals goal1 goal2 i =
      (i<0) ||
      (((TTerm.SeqGoal.get goal1 i) == (TTerm.SeqGoal.get goal2 i)) &&
       (compare_goals goal1 goal2 (pred i)))

   let compare_tterm t1 t2 =
      match (t1,t2) with
         TTerm t1, TTerm t2 ->
            compare_term t1 t2
       | TSeq { TType.sequent_args = arg1; TType.sequent_hyps = hyp1; TType.sequent_goals = goal1},
         TSeq { TType.sequent_args = arg2; TType.sequent_hyps = hyp2; TType.sequent_goals = goal2} ->
            (arg1 == arg2) &&
            (TTerm.SeqHyp.length hyp1 = TTerm.SeqHyp.length hyp2) &&
            (compare_hyps hyp1 hyp2 (TTerm.SeqHyp.length hyp1 - 1)) &&
            (TTerm.SeqGoal.length goal1 = TTerm.SeqGoal.length goal2) &&
            (compare_goals goal1 goal2 (TTerm.SeqGoal.length goal1 - 1))
       | _ -> false

   let compare_bterm { TType.bvars = bvars1; TType.bterm = bterm1 }
       { TType.bvars = bvars2; TType.bterm = bterm2 } =
      bvars1 = bvars2 & bterm1 == bterm2

   (*
    * Copy functions.
    *)
   let make_hyp info hyps i =
      match FTerm.SeqHyp.get hyps i with
         FType.Hypothesis (v, t) -> TType.Hypothesis (v, Memo.apply info.copy_term info t)
       | FType.Context (v, trms) -> TType.Context (v, List.map (Memo.apply info.copy_term info) trms)

   let make_goal info goals i =
      Memo.apply info.copy_term info (FTerm.SeqGoal.get goals i)

   let make_term info t =
      if FromTerm.TermMan.is_sequent_term t then
         let { FType.sequent_args = args;
               FType.sequent_hyps = hyps;
               FType.sequent_goals = goals } = (FromTerm.TermMan.explode_sequent t)
         in
            TSeq
               { TType.sequent_args = Memo.apply info.copy_term info args;
                 TType.sequent_hyps = TTerm.SeqHyp.init (FTerm.SeqHyp.length hyps) (make_hyp info hyps);
                 TType.sequent_goals = TTerm.SeqGoal.init (FTerm.SeqGoal.length goals) (make_goal info goals)
               }
      else
         let { FType.term_op = op; FType.term_terms = bterms } = FTerm.dest_term t
         in
            TTerm
               { TType.term_op = Memo.apply info.copy_operator info op;
                 TType.term_terms = List.map (Memo.apply info.copy_bterm info) bterms }

   let do_make_term _ = function
      TTerm t -> TTerm.make_term t
    | TSeq s -> ToTerm.TermMan.mk_sequent_term s

   let make_bterm info bterm =
      let { FType.bvars = bvars; FType.bterm = bterm } = FTerm.dest_bterm bterm
      in
         { TType.bvars = bvars;
           TType.bterm = Memo.apply info.copy_term info bterm
         }

   let make_operator info op =
      let { FType.op_name = opname; FType.op_params = params } = FTerm.dest_op op
      in
         { TType.op_name = normalize_opname opname;
           TType.op_params = List.map (Memo.apply info.copy_param info) params
         }

   let make_param info param =
      match FTerm.dest_param param with
         FType.Number n1 ->            TType.Number n1
       | FType.String s1 ->            TType.String s1
       | FType.Token s1 ->             TType.Token s1
       | FType.Level l1 ->             TType.Level (Memo.apply info.copy_level info l1)
       | FType.Var v1 ->               TType.Var v1
       | FType.MNumber s1 ->           TType.MNumber s1
       | FType.MString s1 ->           TType.MString s1
       | FType.MToken s1 ->            TType.MToken s1
       | FType.MLevel s1 ->            TType.MLevel s1
       | FType.MVar s1 ->              TType.MVar s1
       | FType.ObId oid1 ->            TType.ObId (List.map (Memo.apply info.copy_param info) oid1)
       | FType.ParamList p1 ->         TType.ParamList (List.map (Memo.apply info.copy_param info) p1)
       | FType.MSum (p11, p21) ->      TType.MSum (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MDiff (p11, p21) ->     TType.MDiff (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MProduct (p11, p21) ->  TType.MProduct (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MQuotient (p11, p21) -> TType.MQuotient (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MRem (p11, p21) ->      TType.MRem (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MLessThan (p11, p21) -> TType.MLessThan (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MEqual (p11, p21) ->    TType.MEqual (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)
       | FType.MNotEqual (p11, p21) -> TType.MNotEqual (Memo.apply info.copy_param info p11, Memo.apply info.copy_param info p21)

   let make_level info level =
      let { FType.le_const = c; FType.le_vars = vars } = FTerm.dest_level level
      in
         { TType.le_const = c;
           TType.le_vars = List.map (Memo.apply info.copy_level_var info) vars
         }

   let make_level_var info lvar =
      let { FType.le_var = var; FType.le_offset = offset } = FTerm.dest_level_var lvar
      in
         { TType.le_var = var;
           TType.le_offset = offset
         }

   (*
    * Create the memo tables.
    *)
   let create () =
      { copy_level_var = Memo.create make_level_var (fun _ t -> TTerm.make_level_var t) compare_level_var;
        copy_level     = Memo.create make_level     (fun _ t -> TTerm.make_level t)     compare_level;
        copy_param     = Memo.create make_param     (fun _ t -> TTerm.make_param t)     compare_param;
        copy_operator  = Memo.create make_operator  (fun _ t -> TTerm.make_op t)        compare_operator;
        copy_term      = Memo.create make_term      do_make_term                        compare_tterm;
        copy_bterm     = Memo.create make_bterm     (fun _ t -> TTerm.make_bterm t)     compare_bterm
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
      FType.MetaTheorem t ->
         TType.MetaTheorem (Memo.apply info.copy_term info t)
    | FType.MetaImplies (t1, t2) ->
         TType.MetaImplies (copy_meta_term info t1,
                                         copy_meta_term info t2)
    | FType.MetaFunction (t1, mt1, mt2) ->
         TType.MetaFunction (Memo.apply info.copy_term info t1,
                                          copy_meta_term info mt1,
                                          copy_meta_term info mt2)
    | FType.MetaIff (mt1, mt2) ->
         TType.MetaIff (copy_meta_term info mt1,
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
