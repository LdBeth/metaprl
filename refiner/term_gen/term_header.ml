(* This file is an interface for recursiveless denotation
 * of term types
 *
 * -----------------------------------------------------------------
 * This file is part of Nuprl-Light, a modular, higher order
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
open Infinite_weak_array
open Opname

module TermHeader =
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
struct
   module TType = ToTerm.TermType
   module IAr = InfiniteWeakArray

   type term = TType.term
   type param = TType.param
   type meta_term = TType.meta_term

   type 'a descriptor = 'a InfiniteWeakArray.descriptor
   type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor
 

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type level_exp_var_header = { le_var : string; le_offset : int }

(*   type level_exp_var_weak_header = { le_var_weak : string; le_offset_weak : int }
*)

   type level_exp_header = {
                             le_const: int;
                             le_vars: level_exp_var_header list
                           }

(*   type level_exp_weak_header = {
                                 le_const_weak: int;
                                 le_vars_weak: level_exp_var_header list
                                }
*)

   type param_header =
      Number of Mp_num.num
    | String of string
    | Token of string
    | Level of level_exp_header
    | Var of string
    | MNumber of string
    | MString of string
    | MToken of string
    | MLevel of string
    | MVar of string

      (* Special Nuprl5 values *)
    | ObId of TType.param IAr.descriptor list
    | ParamList of TType.param IAr.descriptor list

      (* Num operations *)
    | MSum of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MDiff of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MProduct of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MQuotient of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MRem of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MLessThan of TType.param IAr.descriptor * TType.param IAr.descriptor

      (* Comparisons *)
    | MEqual of TType.param IAr.descriptor * TType.param IAr.descriptor
    | MNotEqual of TType.param IAr.descriptor * TType.param IAr.descriptor

   type param_weak_header =
      Number_weak of Mp_num.num
    | String_weak of string
    | Token_weak of string
    | Level_weak of level_exp_header
    | Var_weak of string
    | MNumber_weak of string
    | MString_weak of string
    | MToken_weak of string
    | MLevel_weak of string
    | MVar_weak of string

      (* Special Nuprl5 values *)
    | ObId_weak of TType.param IAr.weak_descriptor list
    | ParamList_weak of TType.param IAr.weak_descriptor list

      (* Num operations *)
    | MSum_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor
    | MDiff_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor
    | MProduct_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor
    | MQuotient_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor
    | MRem_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor
    | MLessThan_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor

      (* Comparisons *)
    | MEqual_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor
    | MNotEqual_weak of TType.param IAr.weak_descriptor * TType.param IAr.weak_descriptor

   type hypothesis_header =
      Hypothesis of string * TType.term IAr.descriptor
    | Context of string * TType.term IAr.descriptor list

   type hypothesis_weak_header =
      Hypothesis_weak of string * TType.term IAr.weak_descriptor
    | Context_weak of string * TType.term IAr.weak_descriptor list

   type bound_term_header = {
                         bvars: string list;
                         bterm: TType.term IAr.descriptor
                       }

   type bound_term_weak_header = {
                         bvars_weak: string list;
                         bterm_weak: TType.term IAr.weak_descriptor
                       }

   type true_term_header = {
                             op_name: opname IAr.descriptor;
                             op_params: TType.param IAr.descriptor list;
                             term_terms: bound_term_header list
                           }

   type true_term_weak_header = {
                             op_name_weak: opname IAr.weak_descriptor;
                             op_params_weak: TType.param IAr.weak_descriptor list;
                             term_terms_weak: bound_term_weak_header list
                           }

   type seq_header = { 
                       seq_arg: TType.term IAr.descriptor;
                       seq_hyps: hypothesis_header list;
                       seq_goals: TType.term IAr.descriptor list
                     }

   type seq_weak_header = { 
                       seq_arg_weak: TType.term IAr.weak_descriptor;
                       seq_hyps_weak: hypothesis_weak_header list;
                       seq_goals_weak: TType.term IAr.weak_descriptor list
                     }

   type term_header = Term of true_term_header
                    | Seq of seq_header

   type term_weak_header = Term_weak of true_term_weak_header
                         | Seq_weak of seq_weak_header

   type meta_term_header =
      MetaTheorem of TType.term IAr.descriptor
    | MetaImplies of TType.meta_term IAr.descriptor * TType.meta_term IAr.descriptor
    | MetaFunction of TType.term IAr.descriptor * TType.meta_term IAr.descriptor * TType.meta_term IAr.descriptor
    | MetaIff of TType.meta_term IAr.descriptor * TType.meta_term IAr.descriptor
    | MetaLabeled of string * TType.meta_term IAr.descriptor

   type meta_term_weak_header =
      MetaTheorem_weak of TType.term IAr.weak_descriptor
    | MetaImplies_weak of TType.meta_term IAr.weak_descriptor * TType.meta_term IAr.weak_descriptor
    | MetaFunction_weak of TType.term IAr.weak_descriptor * TType.meta_term IAr.weak_descriptor * TType.meta_term IAr.weak_descriptor
    | MetaIff_weak of TType.meta_term IAr.weak_descriptor * TType.meta_term IAr.weak_descriptor
    | MetaLabeled_weak of string * TType.meta_term IAr.weak_descriptor

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

(*   let weak_level_var_header {le_var=var; le_offset=offset} =
      {le_var_weak=var; le_offset_weak=offset}

   let weak_level_exp_header { le_const=c; le_vars=vars } =
      { le_const_weak=c; le_vars_weak=List.map weak_level_var_header vars }
*)

   let weak_param_header param_header =
      match param_header with
         Number n1 ->            Number_weak n1
       | String s1 ->            String_weak s1
       | Token s1 ->             Token_weak s1
       | Level l1 ->             Level_weak l1
       | Var v1 ->               Var_weak v1
       | MNumber s1 ->           MNumber_weak s1
       | MString s1 ->           MString_weak s1
       | MToken s1 ->            MToken_weak s1
       | MLevel s1 ->            MLevel_weak s1
       | MVar s1 ->              MVar_weak s1
       | ObId oid1 ->            ObId_weak (List.map IAr.weaking oid1)
       | ParamList p1 ->         ParamList_weak (List.map IAr.weaking p1)
       | MSum (p11, p21) ->      MSum_weak (IAr.weaking p11, IAr.weaking p21)
       | MDiff (p11, p21) ->     MDiff_weak (IAr.weaking p11, IAr.weaking p21)
       | MProduct (p11, p21) ->  MProduct_weak (IAr.weaking p11, IAr.weaking p21)
       | MQuotient (p11, p21) -> MQuotient_weak (IAr.weaking p11, IAr.weaking p21)
       | MRem (p11, p21) ->      MRem_weak (IAr.weaking p11, IAr.weaking p21)
       | MLessThan (p11, p21) -> MLessThan_weak (IAr.weaking p11, IAr.weaking p21)
       | MEqual (p11, p21) ->    MEqual_weak (IAr.weaking p11, IAr.weaking p21)
       | MNotEqual (p11, p21) -> MNotEqual_weak (IAr.weaking p11, IAr.weaking p21)


   let weak_bterm_header { bvars=bvs; bterm=term_index } =
      { bvars_weak=bvs; bterm_weak= IAr.weaking term_index }

   let weak_hyp_header hyp =
      match hyp with
         Hypothesis (v, t) -> Hypothesis_weak (v, IAr.weaking t)
       | Context (v, trms) -> Context_weak (v, List.map IAr.weaking trms)

   let weak_tterm_header { op_name = op; op_params = params; term_terms = bterms } =
      { op_name_weak = IAr.weaking op; 
        op_params_weak = List.map IAr.weaking params; 
        term_terms_weak = List.map weak_bterm_header bterms }

   let weak_term_header th =
      match th with
         Seq { seq_arg = arg; seq_hyps = hyps; seq_goals = goals } ->
            Seq_weak { seq_arg_weak = IAr.weaking arg;
                       seq_hyps_weak = List.map weak_hyp_header hyps; 
                       seq_goals_weak = List.map IAr.weaking goals 
                     }
       | Term th -> Term_weak (weak_tterm_header th)

   let weak_meta_term_header mt =
      match mt with
      MetaTheorem t ->
         MetaTheorem_weak (IAr.weaking t)
    | MetaImplies (t1, t2) ->
         MetaImplies_weak (IAr.weaking t1, IAr.weaking t2)
    | MetaFunction (t1, mt1, mt2) ->
         MetaFunction_weak (IAr.weaking t1, IAr.weaking mt1, IAr.weaking mt2)
    | MetaIff (mt1, mt2) ->
         MetaIff_weak (IAr.weaking mt1, IAr.weaking mt2)
    | MetaLabeled (l, mt) ->
         MetaLabeled_weak (l, IAr.weaking mt)

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

   let compare_level_var_header { le_var=v1; le_offset=offset1 } { le_var=v2; le_offset=offset2 } =
      v1 == v2 & offset1 == offset2

   let compare_level_header { le_const=const1; le_vars=vars1 } { le_const=const2; le_vars=vars2 } =
      const1 = const2 & list_compare compare_level_var_header vars1 vars2

   let compare_param_header param1 param2 =
      match param1, param2 with
         Number_weak    n1,         Number_weak    n2         -> Mp_num.eq_num n1 n2
       | String_weak    s1,         String_weak    s2         -> s1 = s2
       | Token_weak     s1,         Token_weak     s2         -> s1 = s2
       | Level_weak     l1,         Level_weak     l2         -> compare_level_header l1 l2
       | Var_weak       v1,         Var_weak       v2         -> v1 = v2
       | MNumber_weak   s1,         MNumber_weak   s2         -> s1 = s2
       | MString_weak   s1,         MString_weak   s2         -> s1 = s2
       | MToken_weak    s1,         MToken_weak    s2         -> s1 = s2
       | MLevel_weak    s1,         MLevel_weak    s2         -> s1 = s2
       | MVar_weak      s1,         MVar_weak      s2         -> s1 = s2
       | ObId_weak      oid1,       ObId_weak      oid2       -> list_mem_eq oid1 oid2
       | ParamList_weak params1,    ParamList_weak params2    -> list_mem_eq params1 params2
       | MSum_weak      (p11, p12), MSum_weak      (p21, p22) -> p11 == p21 & p12 == p22
       | MDiff_weak     (p11, p12), MDiff_weak     (p21, p22) -> p11 == p21 & p12 == p22
       | MProduct_weak  (p11, p12), MProduct_weak  (p21, p22) -> p11 == p21 & p12 == p22
       | MQuotient_weak (p11, p12), MQuotient_weak (p21, p22) -> p11 == p21 & p12 == p22
       | MRem_weak      (p11, p12), MRem_weak      (p21, p22) -> p11 == p21 & p12 == p22
       | MLessThan_weak (p11, p12), MLessThan_weak (p21, p22) -> p11 == p21 & p12 == p22
       | MEqual_weak    (p11, p12), MEqual_weak    (p21, p22) -> p11 == p21 & p12 == p22
       | MNotEqual_weak (p11, p12), MNotEqual_weak (p21, p22) -> p11 == p21 & p12 == p22
       | _ -> false

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

end   

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
