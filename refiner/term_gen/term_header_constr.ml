(* This file implements term-headers' constructors
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov, Alexey Nogin
 *)

open Printf
open Mp_debug

open List
open Term_sig
open Termmod_sig
open Weak_memo
open Opname
open Term_hash

module TermHeaderConstr
   (FromTerm : Termmod_sig.TermModuleSig)
   (ToTerm : Termmod_sig.TermModuleSig)

   (TermHash : Term_hash_sig.TermHashSig
      with type param = ToTerm.TermType.param
      with type param' = ToTerm.TermType.param'
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term) =

struct
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module FTerm = FromTerm.Term
   module FType = FromTerm.TermType

   module WM = TheWeakMemo

   let make_level_var lvar =
      let { FType.le_var = var; FType.le_offset = offset } = FTerm.dest_level_var lvar in
         TTerm.make_level_var { TType.le_var = var; TType.le_offset = offset}

   let make_level level =
      let { FType.le_const = c; FType.le_vars = vars } = FTerm.dest_level level in
         TTerm.make_level { TType.le_const=c; TType.le_vars=List.map make_level_var vars }

   let rec make_param info param =
      TermHash.p_constr_param info
      (match FTerm.dest_param param with
         FType.Number n1 ->            TType.Number n1
       | FType.String s1 ->            TType.String s1
       | FType.Token s1 ->             TType.Token s1
       | FType.Var v1 ->               TType.Var v1
       | FType.MNumber s1 ->           TType.MNumber s1
       | FType.MString s1 ->           TType.MString s1
       | FType.MToken s1 ->            TType.MToken s1
       | FType.MLevel l1 ->            TType.MLevel (make_level l1)
       | FType.BackwardsCompatibleLevel l1 -> TType.MLevel (make_level l1)
       | FType.MVar s1 ->              TType.MVar s1
       | FType.ObId oid1 ->            TType.ObId (List.map (make_param info) oid1)
       | FType.ParamList p1 ->         TType.ParamList (List.map (make_param info) p1)
      )

   let rec make_context_header info x =
      TermHash.p_lookup info (make_term_header info x)

   and make_hyp_header info hyp =
      match hyp with
         FType.Hypothesis (v, t) -> TermHash.Hypothesis (v, TermHash.p_lookup info (make_term_header info t))
       | FType.Context (v, trms) -> TermHash.Context (v, List.map (make_context_header info) trms)

   and make_true_term_header info tterm =
      let { FType.term_op = term_op; FType.term_terms = term_terms } = FTerm.dest_term tterm in
      let { FType.op_name = opname; FType.op_params = params } = FTerm.dest_op term_op in
         { TermHash.op_name = normalize_opname opname;
           TermHash.op_params = List.map (make_param info) params;
           TermHash.term_terms = List.map (make_bterm_header info) term_terms
         }

   and make_bterm_header info bterm =
      let { FType.bvars = bvs; FType.bterm = bterm } = FTerm.dest_bterm bterm in
         { TermHash.bvars = bvs; TermHash.bterm = TermHash.p_lookup info (make_term_header info bterm) }

   and make_term_header info t =
      if FromTerm.TermMan.is_sequent_term t then
         let { FType.sequent_args = arg;
               FType.sequent_hyps = hyps;
               FType.sequent_goals = goals } = FromTerm.TermMan.explode_sequent t
         in
         TermHash.Seq
            { TermHash.seq_arg = TermHash.p_lookup info (make_term_header info arg);
              TermHash.seq_hyps = List.map (make_hyp_header info) (FTerm.SeqHyp.to_list hyps);
              TermHash.seq_goals = List.map (fun x -> TermHash.p_lookup info (make_term_header info x)) (FTerm.SeqGoal.to_list goals)
            }
      else
            TermHash.Term (make_true_term_header info t)

   let rec make_meta_term_header info mt =
         match mt with
         FType.MetaTheorem t ->
            TermHash.MetaTheorem (TermHash.p_lookup info (make_term_header info t))
       | FType.MetaImplies (t1, t2) ->
            TermHash.MetaImplies (TermHash.p_lookup_meta info (make_meta_term_header info t1),
                                    TermHash.p_lookup_meta info (make_meta_term_header info t2))
       | FType.MetaFunction (t1, mt1, mt2) ->
            TermHash.MetaFunction (TermHash.p_lookup info (make_term_header info t1),
                                     TermHash.p_lookup_meta info (make_meta_term_header info mt1),
                                     TermHash.p_lookup_meta info (make_meta_term_header info mt2))
       | FType.MetaIff (mt1, mt2) ->
            TermHash.MetaIff (TermHash.p_lookup_meta info (make_meta_term_header info mt1),
                                TermHash.p_lookup_meta info (make_meta_term_header info mt2))
       | FType.MetaLabeled (l, mt) ->
            TermHash.MetaLabeled (l, TermHash.p_lookup_meta info (make_meta_term_header info mt))

end
