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

open Opname

module TermHeaderConstr (**)
   (FromTerm : Termmod_sig.TermModuleSig)
   (ToTerm : Termmod_sig.TermModuleSig)
   (TermHash : Term_hash_sig.TermHashSig
    with type param = ToTerm.TermType.param
    with type param' = ToTerm.TermType.param'
    with type term = ToTerm.TermType.term
    with type meta_term = ToTerm.TermType.meta_term
    with type msequent = ToTerm.Refine .msequent) =
struct
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module FTerm = FromTerm.Term
   module FType = FromTerm.TermType;;

   let make_level_var lvar =
      let { FType.le_var = var; FType.le_offset = offset } = FTerm.dest_level_var lvar in
         TTerm.make_level_var { TType.le_var = var; TType.le_offset = offset}

   let make_level level =
      let { FType.le_const = c; FType.le_vars = vars } = FTerm.dest_level level in
         TTerm.make_level { TType.le_const=c; TType.le_vars=List.map make_level_var vars }

   let rec make_param' = function
      FType.Number n1 ->            TType.Number n1
    | FType.String s1 ->            TType.String s1
    | FType.Token s1 ->             TType.Token s1
    | FType.Var v1 ->               TType.Var v1
    | FType.MNumber s1 ->           TType.MNumber s1
    | FType.MString s1 ->           TType.MString s1
    | FType.MToken s1 ->            TType.MToken s1
    | FType.MLevel l1 ->            TType.MLevel (make_level l1)
    | FType.ObId oid1 ->            TType.ObId (List.map make_param_aux oid1)
    | FType.ParamList p1 ->         TType.ParamList (List.map make_param_aux p1)

   and make_param_aux param = TTerm.make_param (make_param' (FTerm.dest_param param))

   let make_param info param =
      TermHash.p_constr_param info (make_param' (FTerm.dest_param param))

   let rec make_context_header info x =
      TermHash.p_lookup info (make_term_header info x)

   and make_hyp_header info hyp =
      match hyp with
         Term_sig.Hypothesis (v, t) -> TermHash.Hypothesis (v, TermHash.p_lookup info (make_term_header info t))
       | Term_sig.Context (v, conts, trms) -> TermHash.Context (v, conts, List.map (make_context_header info) trms)

   and make_goal_header info goal =
      TermHash.p_lookup info (make_term_header info goal)

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
      if FTerm.is_var_term t then
         TermHash.FOVar(FTerm.dest_var t)
      else if FromTerm.TermMan.is_so_var_term t then
         let v, conts, terms = FromTerm.TermMan.dest_so_var t in
            TermHash.SOVar(v, conts, List.map (make_context_header info) terms)
      else if FromTerm.TermMan.is_sequent_term t then
         let { FType.sequent_args = arg;
               FType.sequent_hyps = hyps;
               FType.sequent_goals = goals } = FromTerm.TermMan.explode_sequent t
         in
         let goals = FTerm.SeqGoal.to_list goals in
         (* let hyps = FromTerm.TermMan.remove_redundant_hypbindings (FTerm.SeqHyp.to_list hyps) goals in *)
         let hyps = FTerm.SeqHyp.to_list hyps in
            TermHash.Seq
            { TermHash.seq_arg = TermHash.p_lookup info (make_term_header info arg);
              TermHash.seq_hyps = List.map (make_hyp_header info) hyps;
              TermHash.seq_goals = List.map (make_goal_header info) goals;
            }
      else
         TermHash.Term (make_true_term_header info t)

   let rec make_meta_term_header info mt =
      match mt with
         Term_sig.MetaTheorem t ->
            TermHash.MetaTheorem (TermHash.p_lookup info (make_term_header info t))
       | Term_sig.MetaImplies (t1, t2) ->
            TermHash.MetaImplies (TermHash.p_lookup_meta info (make_meta_term_header info t1),
                                  TermHash.p_lookup_meta info (make_meta_term_header info t2))
       | Term_sig.MetaFunction (t1, mt1, mt2) ->
            TermHash.MetaFunction (TermHash.p_lookup info (make_term_header info t1),
                                   TermHash.p_lookup_meta info (make_meta_term_header info mt1),
                                   TermHash.p_lookup_meta info (make_meta_term_header info mt2))
       | Term_sig.MetaIff (mt1, mt2) ->
            TermHash.MetaIff (TermHash.p_lookup_meta info (make_meta_term_header info mt1),
                              TermHash.p_lookup_meta info (make_meta_term_header info mt2))
       | Term_sig.MetaLabeled (l, mt) ->
            TermHash.MetaLabeled (l, TermHash.p_lookup_meta info (make_meta_term_header info mt))

   let make_msequent_header info mseq =
      let goal, hyps = FromTerm.Refine .dest_msequent mseq in
      let mk_term t =
         TermHash.p_lookup info (make_term_header info t)
      in
         List.map mk_term hyps, mk_term goal
end

(*
 *)
