(*
 * Order over terms.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

open Opname
open Term_sig

module type TermOrderSig =
   functor(R: Refiner_sig.RefinerSig) ->
   sig
      open R.TermType

      type comparison = Less | Equal | Greater

      val compare_level_vars : level_exp_var -> level_exp_var -> comparison
      val compare_levels : level_exp -> level_exp -> comparison
      val compare_params : param -> param -> comparison
      val compare_operators : operator -> operator -> comparison
      val compare_terms : term -> term -> comparison
      val compare_bterms : bound_term -> bound_term -> comparison
	(*val compare_hyps : Term.Term.SeqHyp.t -> Term.Term.SeqHyp.t -> int -> comparison*)
   end

module TermOrder (R: Refiner_sig.RefinerSig) =
struct
   open R.Term
   open R.TermType
   open R.TermSubst

   type comparison = Less | Equal | Greater

   let simple_compare x y =
      match Pervasives.compare x y with
         0 -> Equal
       | i when i > 0 -> Greater
       | _ -> Less

   let rec compare_lists compare l1 l2 =
      if l1==l2 then
         Equal
      else
         match l1, l2 with
            [], [] -> Equal
          | [], _  -> Less
          | _ , []  -> Greater
          | hd1::tail1, hd2::tail2 ->
               begin match compare hd1 hd2 with
                        Equal -> compare_lists compare tail1 tail2
                      | cmp -> cmp
               end

   let rec compare_terms t1 t2 =
      if t1==t2 then
         Equal
      else
         let {term_op=op1; term_terms=subt1} = dest_term t1 in
         let {term_op=op2; term_terms=subt2} = dest_term t2 in
            match compare_operators op1 op2 with
               Less -> Less
             | Greater -> Greater
             | Equal -> compare_btlists subt1 subt2

   and compare_operators op1 op2 =
      if op1==op2 then
         Equal
      else
         let {op_name = opn1; op_params = par1} = dest_op op1 in
         let {op_name = opn2; op_params = par2} = dest_op op2 in
         let str1 = string_of_opname opn1 in
         let str2 = string_of_opname opn2 in
            if str1 < str2 then
               Less
            else if str1 > str2 then
               Greater
            else
               compare_plists par1 par2

   and compare_btlists btl1 btl2 = compare_lists compare_bterms btl1 btl2

   and compare_bterms b1 b2 =
      if b1==b2 then
         Equal
      else
         let { bterm = bt1; bvars = bv1 } = dest_bterm b1 in
         let { bterm = bt2; bvars = bv2 } = dest_bterm b2 in
         let pairs = List.combine bv2 bv1 in
         let subst = List.map (fun (v2,v1) -> (v2, mk_var_term v1)) pairs in
         let bt2' = apply_subst subst bt2 in
            compare_terms bt1 bt2'

   and compare_plists pl1 pl2 = compare_lists compare_params pl1 pl2

   and compare_params par1 par2 =
      if par1==par2 then
         Equal
      else
         match dest_param par1, dest_param par2 with
            Number n1    , Number n2     -> simple_compare n1 n2
          | Number _     , _             -> Less
          | _            , Number _      -> Greater
          | String s1    , String s2     -> simple_compare s1 s2
          | String _     , _             -> Less
          | _            , String _      -> Greater
          | Token t1     , Token t2      -> simple_compare t1 t2
          | Token _      , _             -> Less
          | _            , Token _       -> Greater
          | Var v1       , Var v2        -> simple_compare v1 v2
          | Var _        , _             -> Less
          | _            , Var _         -> Greater
          | Quote        , _             -> Less
          | _            , Quote         -> Greater
          | MNumber s1   , MNumber s2    -> simple_compare s1 s2
          | MNumber _    , _             -> Less
          | _            , MNumber _     -> Greater
          | MString s1   , MString s2    -> simple_compare s1 s2
          | MString _    , _             -> Less
          | _            , MString _     -> Greater
          | MToken s1    , MToken s2     -> simple_compare s1 s2
          | MToken _     , _             -> Less
          | _            , MToken _      -> Greater
          | MLevel l1    , MLevel l2     -> compare_levels l1 l2
          | MLevel _     , _             -> Less
          | _            , MLevel _      -> Greater
          | ObId id1     , ObId id2      -> compare_plists id1 id2
          | ObId _       , _             -> Less
          | _            , ObId _        -> Greater
          | ParamList pl1, ParamList pl2 -> compare_plists pl1 pl2

   and compare_levels l1 l2 =
      if l1==l2 then
         Equal
      else
         let {le_const = c1; le_vars = v1} = dest_level l1 in
         let {le_const = c2; le_vars = v2} = dest_level l2 in
            if c1<c2 then Less
            else if c1>c2 then Greater
            else compare_lvlists v1 v2

   and compare_lvlists lvl1 lvl2 = compare_lists compare_level_vars lvl1 lvl2

   and compare_level_vars v1 v2 =
      if v1==v2 then
         Equal
      else
         let {le_var=s1; le_offset=o1}=dest_level_var v1 in
         let {le_var=s2; le_offset=o2}=dest_level_var v2 in
            if s1<s2 then Less
            else if s1>s2 then Greater
            else simple_compare o1 o2
end
