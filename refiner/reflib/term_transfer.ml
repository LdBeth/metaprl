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
 * Modified by: Eli Barzilay, Alexey Nogin, Yegor Bryukhov
 *)

open Mp_debug
open Printf

open Opname

open Termmod_sig
open Simplehash_sig
open Bi_memo
open Term_compare_sig

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

module MakeTermCopy =
  functor(Hash : SimpleHashSig) ->
  functor(TermCompare: TermCompareSig) ->
  functor(FromTerm : TermModuleSig) ->
  functor(ToTerm : TermModuleSig) ->
struct
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module FTerm = FromTerm.Term
   module FType = FromTerm.TermType

   module ExtBim = ExtBiMemo(Hash)

   module ForwardCompare = TermCompare(ToTerm)
   module BackwardCompare = TermCompare(FromTerm)

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)
(*
   type to_term = TTerm of TType.term' | TSeq of TType.esequent

   type from_term = FTerm of FType.term' | FSeq of FType.esequent
*)

   type to_term = ForwardCompare.c_term

   type from_term = BackwardCompare.c_term

   type t =
      { copy_level_var : (t, FType.level_exp_var, TType.level_exp_var', FType.level_exp_var', TType.level_exp_var) ExtBim.t;
        copy_level     : (t, FType.level_exp,     TType.level_exp',     FType.level_exp',     TType.level_exp)     ExtBim.t;
        copy_param     : (t, FType.param,         TType.param',         FType.param',         TType.param)         ExtBim.t;
        copy_operator  : (t, FType.operator,      TType.operator',      FType.operator',      TType.operator)      ExtBim.t;
        copy_term      : (t, FType.term,          to_term,              from_term,            TType.term)          ExtBim.t;
        copy_bterm     : (t, FType.bound_term,    TType.bound_term',    FType.bound_term',    TType.bound_term)    ExtBim.t
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = List_util.compare_eq

   (*
    * Copy functions.
    *)
   let make_hyp info hyps i =
      match FTerm.SeqHyp.get hyps i with
         FType.Hypothesis (v, t) -> TType.Hypothesis (v, ExtBim.apply info.copy_term info t)
       | FType.Context (v, trms) -> TType.Context (v, List.map (ExtBim.apply info.copy_term info) trms)

   let invert_hyp info hyps i =
      match TTerm.SeqHyp.get hyps i with
         TType.Hypothesis (v,t) -> FType.Hypothesis (v, ExtBim.invert info.copy_term info t)
       | TType.Context (v, trms) -> FType.Context (v, List.map (ExtBim.invert info.copy_term info) trms)   

   let make_goal info goals i =
      ExtBim.apply info.copy_term info (FTerm.SeqGoal.get goals i)

   let invert_goal info goals i =
      ExtBim.invert info.copy_term info (TTerm.SeqGoal.get goals i)

   let make_term info t =
      if FromTerm.TermMan.is_sequent_term t then
         let { FType.sequent_args = args;
               FType.sequent_hyps = hyps;
               FType.sequent_goals = goals } = (FromTerm.TermMan.explode_sequent t)
         in 
            ForwardCompare.CSeq
               { TType.sequent_args = ExtBim.apply info.copy_term info args;
                 TType.sequent_hyps = TTerm.SeqHyp.init (FTerm.SeqHyp.length hyps) (make_hyp info hyps);
                 TType.sequent_goals = TTerm.SeqGoal.init (FTerm.SeqGoal.length goals) (make_goal info goals)
               }
      else
         let { FType.term_op = op; FType.term_terms = bterms } = FTerm.dest_term t
         in 
            ForwardCompare.CTerm
               { TType.term_op = ExtBim.apply info.copy_operator info op;
                 TType.term_terms = List.map (ExtBim.apply info.copy_bterm info) bterms }

   let invert_term info t =
      if ToTerm.TermMan.is_sequent_term t then
         let { TType.sequent_args = args;
               TType.sequent_hyps = hyps;
               TType.sequent_goals = goals } = (ToTerm.TermMan.explode_sequent t)
         in
            BackwardCompare.CSeq
               { FType.sequent_args = ExtBim.invert info.copy_term info args;
                 FType.sequent_hyps = FTerm.SeqHyp.init (TTerm.SeqHyp.length hyps) (invert_hyp info hyps);
                 FType.sequent_goals = FTerm.SeqGoal.init (TTerm.SeqGoal.length goals) (invert_goal info goals)
               }
      else
         let { TType.term_op = op; TType.term_terms = bterms } = TTerm.dest_term t
         in
            BackwardCompare.CTerm
               { FType.term_op = ExtBim.invert info.copy_operator info op;
                 FType.term_terms = List.map (ExtBim.invert info.copy_bterm info) bterms }

   let do_make_term _ = function
      ForwardCompare.CTerm t -> TTerm.make_term t
    | ForwardCompare.CSeq s -> ToTerm.TermMan.mk_sequent_term s

   let do_invert_term _ = function
      BackwardCompare.CTerm t -> FTerm.make_term t
    | BackwardCompare.CSeq s -> FromTerm.TermMan.mk_sequent_term s

   let make_bterm info bterm =
      let { FType.bvars = bvars; FType.bterm = bterm } = FTerm.dest_bterm bterm
      in
         { TType.bvars = bvars;
           TType.bterm = ExtBim.apply info.copy_term info bterm
         }

   let invert_bterm info bterm =
      let { TType.bvars = bvars; TType.bterm = bterm } = TTerm.dest_bterm bterm
      in
         { FType.bvars = bvars;
           FType.bterm = ExtBim.invert info.copy_term info bterm
         }

   let make_operator info op =
      let { FType.op_name = opname; FType.op_params = params } = FTerm.dest_op op
      in
         { TType.op_name = normalize_opname opname;
           TType.op_params = List.map (ExtBim.apply info.copy_param info) params
         }

   let invert_operator info op =
      let { TType.op_name = opname; TType.op_params = params } = TTerm.dest_op op
      in
         { FType.op_name = normalize_opname opname;
           FType.op_params = List.map (ExtBim.invert info.copy_param info) params
         }

   let make_param info param =
      match FTerm.dest_param param with
         FType.Number n1 ->            TType.Number n1
       | FType.String s1 ->            TType.String s1
       | FType.Token s1 ->             TType.Token s1
       | FType.Level l1 ->             TType.Level (ExtBim.apply info.copy_level info l1)
       | FType.Var v1 ->               TType.Var v1
       | FType.MNumber s1 ->           TType.MNumber s1
       | FType.MString s1 ->           TType.MString s1
       | FType.MToken s1 ->            TType.MToken s1
       | FType.MLevel s1 ->            TType.MLevel s1
       | FType.MVar s1 ->              TType.MVar s1
       | FType.ObId oid1 ->            TType.ObId (List.map (ExtBim.apply info.copy_param info) oid1)
       | FType.ParamList p1 ->         TType.ParamList (List.map (ExtBim.apply info.copy_param info) p1)
       | FType.MSum (p11, p21) ->      TType.MSum (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MDiff (p11, p21) ->     TType.MDiff (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MProduct (p11, p21) ->  TType.MProduct (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MQuotient (p11, p21) -> TType.MQuotient (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MRem (p11, p21) ->      TType.MRem (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MLessThan (p11, p21) -> TType.MLessThan (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MEqual (p11, p21) ->    TType.MEqual (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)
       | FType.MNotEqual (p11, p21) -> TType.MNotEqual (ExtBim.apply info.copy_param info p11, ExtBim.apply info.copy_param info p21)

   let invert_param info param =
      match TTerm.dest_param param with
         TType.Number n1 ->            FType.Number n1
       | TType.String s1 ->            FType.String s1
       | TType.Token s1 ->             FType.Token s1
       | TType.Level l1 ->             FType.Level (ExtBim.invert info.copy_level info l1)
       | TType.Var v1 ->               FType.Var v1
       | TType.MNumber s1 ->           FType.MNumber s1
       | TType.MString s1 ->           FType.MString s1
       | TType.MToken s1 ->            FType.MToken s1
       | TType.MLevel s1 ->            FType.MLevel s1
       | TType.MVar s1 ->              FType.MVar s1
       | TType.ObId oid1 ->            FType.ObId (List.map (ExtBim.invert info.copy_param info) oid1)
       | TType.ParamList p1 ->         FType.ParamList (List.map (ExtBim.invert info.copy_param info) p1)
       | TType.MSum (p11, p21) ->      FType.MSum (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MDiff (p11, p21) ->     FType.MDiff (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MProduct (p11, p21) ->  FType.MProduct (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MQuotient (p11, p21) -> FType.MQuotient (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MRem (p11, p21) ->      FType.MRem (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MLessThan (p11, p21) -> FType.MLessThan (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MEqual (p11, p21) ->    FType.MEqual (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)
       | TType.MNotEqual (p11, p21) -> FType.MNotEqual (ExtBim.invert info.copy_param info p11, ExtBim.invert info.copy_param info p21)

   let make_level info level =
      let { FType.le_const = c; FType.le_vars = vars } = FTerm.dest_level level
      in
         { TType.le_const = c;
           TType.le_vars = List.map (ExtBim.apply info.copy_level_var info) vars
         }

   let invert_level info level =
      let { TType.le_const = c; TType.le_vars = vars } = TTerm.dest_level level
      in
         { FType.le_const = c;
           FType.le_vars = List.map (ExtBim.invert info.copy_level_var info) vars
         }

   let make_level_var info lvar =
      let { FType.le_var = var; FType.le_offset = offset } = FTerm.dest_level_var lvar
      in
         { TType.le_var = var;
           TType.le_offset = offset
         }

   let invert_level_var info lvar =
      let { TType.le_var = var; TType.le_offset = offset } = TTerm.dest_level_var lvar
      in
         { FType.le_var = var;
           FType.le_offset = offset
         }
  
   (*
    * Create the memo tables.
    *)
   let create () =
      { copy_level_var = ExtBim.create make_level_var    (fun _ t -> TTerm.make_level_var t)  ForwardCompare.compare_level_var
                                        invert_level_var  (fun _ t -> FTerm.make_level_var t)  BackwardCompare.compare_level_var;
        copy_level     = ExtBim.create make_level        (fun _ t -> TTerm.make_level t)      ForwardCompare.compare_level
                                        invert_level      (fun _ t -> FTerm.make_level t)      BackwardCompare.compare_level;
        copy_param     = ExtBim.create make_param        (fun _ t -> TTerm.make_param t)      ForwardCompare.compare_param
                                        invert_param      (fun _ t -> FTerm.make_param t)      BackwardCompare.compare_param;
        copy_operator  = ExtBim.create make_operator     (fun _ t -> TTerm.make_op t)         ForwardCompare.compare_operator
                                        invert_operator   (fun _ t -> FTerm.make_op t)         BackwardCompare.compare_operator;
        copy_term      = ExtBim.create make_term         do_make_term                         ForwardCompare.compare_cterm
                                        invert_term       do_invert_term                         BackwardCompare.compare_cterm;
        copy_bterm     = ExtBim.create make_bterm        (fun _ t -> TTerm.make_bterm t)      ForwardCompare.compare_bterm
                                        invert_bterm      (fun _ t -> FTerm.make_bterm t)      BackwardCompare.compare_bterm
      }

   (*
    * Basic term application.
    *)
   let copy_term info t =
      ExtBim.apply info.copy_term info t

   let back_term info t =
      ExtBim.invert info.copy_term info t

   (*
    * Meta terms.
    * We don't share at the meta-term level.
    *)
   let rec copy_meta_term info = function
      FType.MetaTheorem t ->
         TType.MetaTheorem (ExtBim.apply info.copy_term info t)
    | FType.MetaImplies (t1, t2) ->
         TType.MetaImplies (copy_meta_term info t1,
                                         copy_meta_term info t2)
    | FType.MetaFunction (t1, mt1, mt2) ->
         TType.MetaFunction (ExtBim.apply info.copy_term info t1,
                                          copy_meta_term info mt1,
                                          copy_meta_term info mt2)
    | FType.MetaIff (mt1, mt2) ->
         TType.MetaIff (copy_meta_term info mt1,
                                     copy_meta_term info mt2)

   let rec back_meta_term info = function
      TType.MetaTheorem t ->
         FType.MetaTheorem (ExtBim.invert info.copy_term info t)
    | TType.MetaImplies (t1, t2) ->
         FType.MetaImplies (back_meta_term info t1, back_meta_term info t2)
    | TType.MetaFunction (t1, mt1, mt2) ->
         FType.MetaFunction (ExtBim.invert info.copy_term info t1,
                                             back_meta_term info mt1,
                                             back_meta_term info mt2)
    | TType.MetaIff (mt1, mt2) ->
         FType.MetaIff (back_meta_term info mt1,
                                     back_meta_term info mt2)


   (*
    * Single-use versions.
    *)
   let copy_term_single t =
      copy_term (create ()) t

   let back_term_single t =
      back_term (create ()) t

   let copy_meta_term_single t =
      copy_meta_term (create ()) t

   let back_meta_term_single t =
      back_meta_term (create ()) t

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)






