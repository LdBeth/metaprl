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

open List
open Term_sig
open Termmod_sig
open Weak_memo
open Opname
open Term_hash
open Infinite_weak_array

module TermHeaderConstr
   (FromTerm : Termmod_sig.TermModuleSig)
   (ToTerm : Termmod_sig.TermModuleSig)

   (TermHeader : Term_header_sig.TermHeaderSig
      with type term = ToTerm.TermType.term
      with type param = ToTerm.TermType.param
      with type meta_term = ToTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor)

   (TermHash : Term_hash_sig.TermHashSig
      with type param_header = TermHeader.param_header
      with type param_weak_header = TermHeader.param_weak_header
      with type term_header = TermHeader.term_header
      with type term_weak_header = TermHeader.term_weak_header
      with type meta_term_header = TermHeader.meta_term_header
      with type meta_term_weak_header = TermHeader.meta_term_weak_header

      with type param = ToTerm.TermType.param
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term) =

struct
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module FTerm = FromTerm.Term
   module FType = FromTerm.TermType

   module IAr = InfiniteWeakArray

   let make_level_var_header lvar =
      let { FType.le_var = var; FType.le_offset = offset } = FTerm.dest_level_var lvar in
         {TermHeader.le_var = var; TermHeader.le_offset = offset}

   let make_level_header level =
      let { FType.le_const = c; FType.le_vars = vars } = FTerm.dest_level level in
         { TermHeader.le_const=c; TermHeader.le_vars=List.map make_level_var_header vars }

   let rec make_param_header info param =
     let {TermHash.param_hash = param_hash}=info in
      match FTerm.dest_param param with
         FType.Number n1 ->            TermHeader.Number n1
       | FType.String s1 ->            TermHeader.String s1
       | FType.Token s1 ->             TermHeader.Token s1
       | FType.Level l1 ->             TermHeader.Level (make_level_header l1)
       | FType.Var v1 ->               TermHeader.Var v1
       | FType.MNumber s1 ->           TermHeader.MNumber s1
       | FType.MString s1 ->           TermHeader.MString s1
       | FType.MToken s1 ->            TermHeader.MToken s1
       | FType.MLevel s1 ->            TermHeader.MLevel s1
       | FType.MVar s1 ->              TermHeader.MVar s1
       | FType.ObId oid1 ->            TermHeader.ObId (List.map (fun x -> TheWeakMemo.lookup param_hash info (make_param_header info x)) oid1)
       | FType.ParamList p1 ->         TermHeader.ParamList (List.map (fun x -> TheWeakMemo.lookup param_hash info (make_param_header info x)) p1)
       | FType.MSum (p11, p21) ->      TermHeader.MSum (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MDiff (p11, p21) ->     TermHeader.MDiff (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MProduct (p11, p21) ->  TermHeader.MProduct (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MQuotient (p11, p21) -> TermHeader.MQuotient (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MRem (p11, p21) ->      TermHeader.MRem (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MLessThan (p11, p21) -> TermHeader.MLessThan (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MEqual (p11, p21) ->    TermHeader.MEqual (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))
       | FType.MNotEqual (p11, p21) -> TermHeader.MNotEqual (TheWeakMemo.lookup param_hash info (make_param_header info p11), TheWeakMemo.lookup param_hash info (make_param_header info p21))

(*   let make_operator_header info op =
      let { FType.op_name = opname; FType.op_params = params } = FTerm.dest_op op in
         ( normalize_opname opname, List.map (TheWeakMemo.lookup param_hash info) params )
*)


   let rec make_term_header info t =
      let {TermHash.term_hash = term_hash} = info in
         if FromTerm.TermMan.is_sequent_term t then
            let { FType.sequent_args = arg;
                  FType.sequent_hyps = hyps;
                  FType.sequent_goals = goals } = (FromTerm.TermMan.explode_sequent t)
            in 
            let make_hyp_header info hyp =
               let {TermHash.term_hash = term_hash} = info in
                  match hyp with
                     FType.Hypothesis (v, t) -> TermHeader.Hypothesis (v, TheWeakMemo.lookup term_hash info (make_term_header info t))
                   | FType.Context (v, trms) -> TermHeader.Context (v, List.map (fun x -> TheWeakMemo.lookup term_hash info (make_term_header info x)) trms)
            in
               TermHeader.Seq
                  { TermHeader.seq_arg = TheWeakMemo.lookup term_hash info (make_term_header info arg);
                    TermHeader.seq_hyps = List.map (make_hyp_header info) (FTerm.SeqHyp.to_list hyps);
                    TermHeader.seq_goals = List.map (fun x -> TheWeakMemo.lookup term_hash info (make_term_header info x)) (FTerm.SeqGoal.to_list goals)
                  }
         else
            let make_true_term_header info tterm =
               let {TermHash.opname_hash = opname_hash; TermHash.param_hash = param_hash; TermHash.term_hash = term_hash} = info in
               let { FType.term_op = term_op; FType.term_terms = term_terms } = FTerm.dest_term tterm in
               let { FType.op_name = opname; FType.op_params = params } = FTerm.dest_op term_op in
               let make_bterm_header info bterm =
                  (let { FType.bvars = bvs; FType.bterm = bterm } = FTerm.dest_bterm bterm in
                     { TermHeader.bvars = bvs; TermHeader.bterm = TheWeakMemo.lookup term_hash info (make_term_header info bterm) }
                  )
               in
                  { TermHeader.op_name = TheWeakMemo.lookup opname_hash info opname;
                    TermHeader.op_params = List.map (fun x -> TheWeakMemo.lookup param_hash info (make_param_header info x)) params;
                    TermHeader.term_terms = (List.map (make_bterm_header info) term_terms)
                  }
            in 
               TermHeader.Term (make_true_term_header info t)

   let rec make_meta_term_header info mt =
      let {TermHash.term_hash = term_hash; TermHash.meta_term_hash = meta_term_hash} = info in
         match mt with
         FType.MetaTheorem t ->
            TermHeader.MetaTheorem (TheWeakMemo.lookup term_hash info (make_term_header info t))
       | FType.MetaImplies (t1, t2) ->
            TermHeader.MetaImplies (TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info t1),
                                    TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info t2))
       | FType.MetaFunction (t1, mt1, mt2) ->
            TermHeader.MetaFunction (TheWeakMemo.lookup term_hash info (make_term_header info t1),
                                     TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info mt1),
                                     TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info mt2))
       | FType.MetaIff (mt1, mt2) ->
            TermHeader.MetaIff (TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info mt1),
                                TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info mt2))
       | FType.MetaLabeled (l, mt) ->
            TermHeader.MetaLabeled (l, TheWeakMemo.lookup meta_term_hash info (make_meta_term_header info mt))

end   
