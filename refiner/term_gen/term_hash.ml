(* This file implements terms' recursive hashing module
 * based on weak arrays
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
open Opname
open Term_sig
open Termmod_sig

module TermHash (ToTerm : Termmod_sig.TermModuleSig) =
struct

   module WM = Weak_memo.TheWeakMemo
   module TTerm = ToTerm.Term
   module TType = ToTerm.TermType
   module TSubst = ToTerm.TermSubst

   type param = TType.param
   type param' = TType.param'
   type term = TType.term
   type meta_term = TType.meta_term


   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)
   
   type term_index = TType.term WM.descriptor
   type meta_term_index = TType.meta_term WM.descriptor

   type hypothesis_header =
      Hypothesis of string * term_index
    | Context of string * term_index list

   type hypothesis_weak_header =
      Hypothesis_weak of string * TType.term WM.weak_descriptor
    | Context_weak of string * TType.term WM.weak_descriptor list

   type bound_term_header = {
                         bvars: string list;
                         bterm: term_index
                       }

   type bound_term_weak_header = {
                         bvars_weak: string list;
                         bterm_weak: TType.term WM.weak_descriptor
                       }

   type true_term_header = {
                             op_name: opname;
                             op_params: TType.param list;
                             term_terms: bound_term_header list
                           }

   type true_term_weak_header = {
                             op_name_weak: opname;
                             op_params_weak: TType.param list;
                             term_terms_weak: bound_term_weak_header list
                           }

   type seq_header = { 
                       seq_arg: term_index;
                       seq_hyps: hypothesis_header list;
                       seq_goals: term_index list
                     }

   type seq_weak_header = { 
                       seq_arg_weak: TType.term WM.weak_descriptor;
                       seq_hyps_weak: hypothesis_weak_header list;
                       seq_goals_weak: TType.term WM.weak_descriptor list
                     }

   type term_header = Term of true_term_header
                    | Seq of seq_header

   type term_weak_header = Term_weak of true_term_weak_header
                         | Seq_weak of seq_weak_header

   type meta_term_header =
      MetaTheorem of term_index
    | MetaImplies of meta_term_index * TType.meta_term WM.descriptor
    | MetaFunction of term_index * meta_term_index * TType.meta_term WM.descriptor
    | MetaIff of meta_term_index * TType.meta_term WM.descriptor
    | MetaLabeled of string * meta_term_index

   type meta_term_weak_header =
      MetaTheorem_weak of TType.term WM.weak_descriptor
    | MetaImplies_weak of TType.meta_term WM.weak_descriptor * TType.meta_term WM.weak_descriptor
    | MetaFunction_weak of TType.term WM.weak_descriptor * TType.meta_term WM.weak_descriptor * TType.meta_term WM.weak_descriptor
    | MetaIff_weak of TType.meta_term WM.weak_descriptor * TType.meta_term WM.weak_descriptor
    | MetaLabeled_weak of string * TType.meta_term WM.weak_descriptor

   type t =
      { param_hash     : (TType.param', TType.param) Hashtbl.t;
        term_hash      : (t, term_header, term_weak_header, TType.term) WM.t;
        meta_term_hash : (t, meta_term_header, meta_term_weak_header, TType.meta_term) WM.t
      }


   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   let weak_bterm_header { bvars=bvs; bterm=term_index } =
      { bvars_weak=bvs; bterm_weak= WM.weaking term_index }

   let weak_hyp_header hyp =
      match hyp with
         Hypothesis (v, t) -> Hypothesis_weak (v, WM.weaking t)
       | Context (v, trms) -> Context_weak (v, List.map WM.weaking trms)

   let weak_tterm_header { op_name = op; op_params = params; term_terms = bterms } =
      { op_name_weak = op; 
        op_params_weak = params; 
        term_terms_weak = List.map weak_bterm_header bterms }

   let weak_term_header th =
      match th with
         Seq { seq_arg = arg; seq_hyps = hyps; seq_goals = goals } ->
            Seq_weak { seq_arg_weak = WM.weaking arg;
                       seq_hyps_weak = List.map weak_hyp_header hyps; 
                       seq_goals_weak = List.map WM.weaking goals 
                     }
       | Term th -> Term_weak (weak_tterm_header th)

   let weak_meta_term_header mt =
      match mt with
      MetaTheorem t ->
         MetaTheorem_weak (WM.weaking t)
    | MetaImplies (t1, t2) ->
         MetaImplies_weak (WM.weaking t1, WM.weaking t2)
    | MetaFunction (t1, mt1, mt2) ->
         MetaFunction_weak (WM.weaking t1, WM.weaking mt1, WM.weaking mt2)
    | MetaIff (mt1, mt2) ->
         MetaIff_weak (WM.weaking mt1, WM.weaking mt2)
    | MetaLabeled (l, mt) ->
         MetaLabeled_weak (l, WM.weaking mt)

   (*
    * Compare that the elements on the lists are equal.
    *)
   let list_mem_eq = List_util.compare_eq

   (*
    * Compare lists with cmp
   *)
   let list_compare cmp lst1 lst2 =
      try fold_right (&&) (map2 cmp lst1 lst2) true
      with
         Invalid_argument _ -> false

   let compare_bterm_header { bvars_weak=bvars1; bterm_weak=bterm1 } { bvars_weak=bvars2; bterm_weak=bterm2 } =
      bvars1 = bvars2 & bterm1 == bterm2

   let compare_hyp_header hyp1 hyp2 =
      match hyp1, hyp2 with
           Hypothesis_weak (v1,t1),  Hypothesis_weak (v2,t2)   -> v1 = v2 && t1 == t2
         | Context_weak    (v1,ts1), Context_weak    (v2, ts2) -> v1 = v2 && list_mem_eq ts1 ts2
         | _ -> false

   let compare_tterm_header { op_name_weak = opn1; op_params_weak = ops1; term_terms_weak = bterms1 }
   							{ op_name_weak = opn2; op_params_weak = ops2; term_terms_weak = bterms2 } =
      opn1 == opn2 & list_mem_eq ops1 ops2 & list_compare (compare_bterm_header) bterms1 bterms2

   let compare_term_header t1 t2 =
      match (t1,t2) with
         Term_weak t1, Term_weak t2 ->
            compare_tterm_header t1 t2
       | Seq_weak { seq_arg_weak = arg1; seq_hyps_weak = hyp1; seq_goals_weak = goal1 },
         Seq_weak { seq_arg_weak = arg2; seq_hyps_weak = hyp2; seq_goals_weak = goal2 } ->
            (arg1 == arg2) && list_compare (compare_hyp_header) hyp1 hyp2 &&
			list_mem_eq goal1 goal2
       | _ -> false

   let compare_meta_term_header mt_a mt_b =
      match mt_a, mt_b with
      MetaTheorem_weak t1, MetaTheorem_weak t2 -> t1==t2
    | MetaImplies_weak (t11, t12), MetaImplies_weak (t21, t22) -> t11==t21 && t12==t22
    | MetaFunction_weak (t1, mt11, mt12), MetaFunction_weak (t2, mt21, mt22) -> t1==t2 && mt11==mt21 && mt12==mt22
    | MetaIff_weak (mt11, mt12), MetaIff_weak (mt21, mt22) -> mt11==mt21 && mt12==mt22
    | _ -> false

   let p_constr_param info param =
      try
         Hashtbl.find info.param_hash param
      with Not_found ->
         let result = TTerm.make_param param in
         Hashtbl.add info.param_hash param result;
         result

   let p_constr_operator info (opname_index, params) =
      TTerm.make_op
      { TType.op_name = opname_index;
        TType.op_params = params
      }


   let p_constr_bterm info { bvars=bvs; bterm=term_index } =
      TTerm.make_bterm
      { TType.bvars = bvs;
        TType.bterm = WM.retrieve info.term_hash info term_index
      }

   let p_constr_hyp info hyp =
      match hyp with
         Hypothesis (v, t) -> TType.Hypothesis (v, WM.retrieve info.term_hash info t)
       | Context (v, trms) -> TType.Context (v, List.map (WM.retrieve info.term_hash info) trms)

   let p_constr_tterm info { op_name = op; op_params = params; term_terms = bterms } =
      TTerm.make_term
      { TType.term_op = p_constr_operator info (op, params);
        TType.term_terms = List.map (p_constr_bterm info) bterms }

   let p_constr_term info th =
      match th with
         Seq { seq_arg = arg; seq_hyps = hyps; seq_goals = goals } ->
               ToTerm.TermMan.mk_sequent_term
               { TType.sequent_args = WM.retrieve info.term_hash info arg;
                 TType.sequent_hyps  = TTerm.SeqHyp.of_list  (List.map (p_constr_hyp info) hyps);
                 TType.sequent_goals = TTerm.SeqGoal.of_list (List.map (WM.retrieve info.term_hash info) goals)
               }
       | Term th -> p_constr_tterm info th

   let p_constr_meta_term info mt =
      match mt with
      MetaTheorem t ->
         TType.MetaTheorem (WM.retrieve info.term_hash info t)
    | MetaImplies (t1, t2) ->
         TType.MetaImplies (WM.retrieve info.meta_term_hash info t1,
                            WM.retrieve info.meta_term_hash info t2)
    | MetaFunction (t1, mt1, mt2) ->
         TType.MetaFunction (WM.retrieve info.term_hash info t1,
                             WM.retrieve info.meta_term_hash info mt1,
                             WM.retrieve info.meta_term_hash info mt2)
    | MetaIff (mt1, mt2) ->
         TType.MetaIff (WM.retrieve info.meta_term_hash info mt1,
                        WM.retrieve info.meta_term_hash info mt2)
    | MetaLabeled (l, mt) ->
         TType.MetaLabeled (l, WM.retrieve info.meta_term_hash info mt)

   let p_create hash_size =
      {
        param_hash = Hashtbl.create hash_size;
        term_hash = WM.create hash_size 20 weak_term_header compare_term_header p_constr_term;
        meta_term_hash = WM.create hash_size 20 weak_meta_term_header compare_meta_term_header p_constr_meta_term
      }

   let p_lookup info th = WM.lookup info.term_hash info th

   let p_unsafe_lookup info th = WM.unsafe_lookup info.term_hash info th

   let p_retrieve info ti = WM.retrieve info.term_hash info ti

   let p_lookup_meta info mth = WM.lookup info.meta_term_hash info mth

   let p_unsafe_lookup_meta info mth = WM.unsafe_lookup info.meta_term_hash info mth

   let p_retrieve_meta info mti = WM.retrieve info.meta_term_hash info mti

   let global_hash = p_create 17

   let lookup th = p_lookup global_hash th
   let unsafe_lookup th = p_unsafe_lookup global_hash th
   let retrieve ti = p_retrieve global_hash ti
   let lookup_meta mth = p_lookup_meta global_hash mth
   let unsafe_lookup_meta mth = p_unsafe_lookup_meta global_hash mth
   let retrieve_meta mti = p_retrieve_meta global_hash mti
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_norm, term_copy_weak"
 * End:
 * -*-
 *)
