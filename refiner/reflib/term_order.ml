(*
 * Order over terms.
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
 * Copyright (C) 2003 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 *)

open Term_sig

module type TermOrderSig =
   functor(R: Refiner_sig.RefinerSig) ->
   sig
      open R.TermType

      val compare_level_vars : level_exp_var -> level_exp_var -> int
      val compare_levels : level_exp -> level_exp -> int
      val compare_params : param -> param -> int
      val compare_operators : operator -> operator -> int
      val compare_terms : term -> term -> int
      val compare_bterms : bound_term -> bound_term -> int
   end

module TermOrder (R: Refiner_sig.RefinerSig) =
struct
   open R.Term
   open R.TermType
   open R.TermSubst
   open R.TermMan
   open R.TermShape

   let rec compare_lists compare l1 l2 =
      if l1==l2 then
         0
      else
         match l1, l2 with
            [], [] -> 0
          | [], _  -> -1
          | _ , []  -> 1
          | hd1::tail1, hd2::tail2 ->
               begin match compare hd1 hd2 with
                        0 -> compare_lists compare tail1 tail2
                      | cmp -> cmp
               end

   let rec compare_terms t1 t2 =
      if t1==t2 then
         0
      else
         let s1 = shape_of_term t1 in
         let s2 = shape_of_term t2 in
            match s1 == var_shape, s1 == sequent_shape, s2 == var_shape, s2 == sequent_shape with
               true, _, false, _ -> 1
             | false, _, true, _ -> -1
             | true, _, true, _ -> Lm_symbol.compare (dest_var t1) (dest_var t2)
             | _, true, _, false -> -1
             | _, false, _, true -> 1
             | _, true, _, true ->
                  let s1 = explode_sequent t1 in
                  let s2 = explode_sequent t2 in
                     begin match compare_terms s1.sequent_args s2.sequent_args with
                        0 ->
                           let l1 = SeqHyp.length s1.sequent_hyps in
                           let l2 = SeqHyp.length s2.sequent_hyps in
                              if l1 = l2 then
                                 compare_hyps s1 s2 l1 [] 0
                              else if l1 < l2 then
                                 -1
                              else
                                 1
                      | c -> c
                     end
             | false, false, false, false ->
                  begin match shape_compare s1 s2 with
                     0 ->
                        let {term_op=op1; term_terms=subt1} = dest_term t1 in
                        let {term_op=op2; term_terms=subt2} = dest_term t2 in
                           begin match compare_operators op1 op2 with
                              0 -> compare_btlists subt1 subt2
                           | c -> c
                           end
                   | c -> c
                  end

   and compare_operators op1 op2 =
      if op1==op2 then
         0
      else
         let {op_name = opn1; op_params = par1} = dest_op op1 in
         let {op_name = opn2; op_params = par2} = dest_op op2 in
            match Opname.compare opn1 opn2 with
               0 -> compare_plists par1 par2
             | c -> c

   and compare_btlists btl1 btl2 = compare_lists compare_bterms btl1 btl2

   and compare_bterms b1 b2 =
      if b1==b2 then
         0
      else
         let { bterm = bt1; bvars = bv1 } = dest_bterm b1 in
         let { bterm = bt2; bvars = bv2 } = dest_bterm b2 in
         let pairs = List.combine bv2 bv1 in
         let subst = List.map (fun (v2,v1) -> (v2, mk_var_term v1)) pairs in
         let bt2' = apply_subst subst bt2 in
            compare_terms bt1 bt2'

   and compare_plists pl1 pl2 = compare_lists compare_params pl1 pl2

   and compare_hyps s1 s2 len sub i =
      if i = len then
         compare_terms s1.sequent_concl (apply_subst sub s2.sequent_concl)
      else
         match (SeqHyp.get s1.sequent_hyps i), (SeqHyp.get s2.sequent_hyps i) with
            Context(v1, conts1, ts1), Context(v2, conts2, ts2) ->
               begin match compare_lists Lm_symbol.compare (v1::conts1) (v2::conts2) with
                  0 ->
                     begin match compare_lists compare_terms ts1 (List.map (apply_subst sub) ts2) with
                        0 -> compare_hyps s1 s2 len sub (i+1)
                      | c -> c
                     end
                | c -> c
               end
          | Hypothesis(v1, t1), Hypothesis(v2, t2) ->
               begin match compare_terms t1 (apply_subst sub t2) with
                  0 -> compare_hyps s1 s2 len ((v2, mk_var_term v1)::sub) (i+1)
                | c -> c
               end
          | Hypothesis _, Context _ -> -1
          | Context _, Hypothesis _ -> 1

   and compare_params par1 par2 =
      if par1==par2 then
         0
      else
         match dest_param par1, dest_param par2 with
            Number n1    , Number n2     -> Lm_num.compare_num n1 n2
          | Number _     , _             -> -1
          | _            , Number _      -> 1
          | String s1    , String s2     -> Stdlib.compare s1 s2
          | String _     , _             -> -1
          | _            , String _      -> 1
          | Token t1     , Token t2      -> Opname.compare t1 t2
          | Token _      , _             -> -1
          | _            , Token _       -> 1
          | Var v1       , Var v2        -> Lm_symbol.compare v1 v2
          | Var _        , _             -> -1
          | _            , Var _         -> 1
          | Shape s1     , Shape s2      -> shape_compare s1 s2
          | Shape _      , _             -> -1
          | _            , Shape _       -> 1
          | Operator op1 , Operator op2  -> compare_opparams op1 op2
          | Operator _   , _             -> -1
          | _            , Operator _    -> 1
          | Quote        , _             -> -1
          | _            , Quote         -> 1
          | MNumber v1   , MNumber v2    -> Lm_symbol.compare v1 v2
          | MNumber _    , _             -> -1
          | _            , MNumber _     -> 1
          | MString v1   , MString v2    -> Lm_symbol.compare v1 v2
          | MString _    , _             -> -1
          | _            , MString _     -> 1
          | MToken v1    , MToken v2     -> Lm_symbol.compare v1 v2
          | MToken _     , _             -> -1
          | _            , MToken _      -> 1
          | MShape v1    , MShape v2     -> Lm_symbol.compare v1 v2
          | MShape _     , _             -> -1
          | _            , MShape _      -> 1
          | MOperator v1 , MOperator v2  -> Lm_symbol.compare v1 v2
          | MOperator _  , _             -> -1
          | _            , MOperator _   -> 1
          | MLevel l1    , MLevel l2     -> compare_levels l1 l2
          | MLevel _     , _             -> -1
          | _            , MLevel _      -> 1
          | ObId id1     , ObId id2      -> compare_plists id1 id2
          | ObId _       , _             -> -1
          | _            , ObId _        -> 1
          | ParamList pl1, ParamList pl2 -> compare_plists pl1 pl2

   and compare_opparams op1 op2 =
      if op1 == op2 then
         0
      else
         match Opname.compare op1.opparam_name op2.opparam_name with
            0 ->
               begin match compare_plists op1.opparam_params op2.opparam_params with
                  0 -> Stdlib.compare op1.opparam_arities op2.opparam_arities
                | i -> i
               end
          | i -> i

   and compare_levels l1 l2 =
      if l1==l2 then
         0
      else
         let {le_const = c1; le_vars = v1} = dest_level l1 in
         let {le_const = c2; le_vars = v2} = dest_level l2 in
            if c1<c2 then -1
            else if c1>c2 then 1
            else compare_lvlists v1 v2

   and compare_lvlists lvl1 lvl2 = compare_lists compare_level_vars lvl1 lvl2

   and compare_level_vars v1 v2 =
      if v1==v2 then
         0
      else
         let {le_var=s1; le_offset=o1}=dest_level_var v1 in
         let {le_var=s2; le_offset=o2}=dest_level_var v2 in
            match Lm_symbol.compare s1 s2 with
               0 -> if o1 = o2 then 0 else if o1 < o2 then -1 else 1
             | c -> c
end
