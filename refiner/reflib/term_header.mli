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

open Infinite_weak_array

module TermHeader :
  functor(ToTerm : Termmod_sig.TermModuleSig) ->
sig

(*
 * Headers - recursiveless denotation of term types
 * All recursive references replaced with their target's descriptors in
 * Infinite_weak_array's array
 *
 * Weak_headers - recursiveless denotation of term types
 * All recursive references replaced with their target's weak_descriptors in
 * Infinite_weak_array's array
 *)

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
    | ObId of ToTerm.TermType.param InfiniteWeakArray.descriptor list
    | ParamList of ToTerm.TermType.param InfiniteWeakArray.descriptor list

      (* Num operations *)
    | MSum of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor
    | MDiff of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor
    | MProduct of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor
    | MQuotient of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor
    | MRem of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor
    | MLessThan of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor

      (* Comparisons *)
    | MEqual of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor
    | MNotEqual of ToTerm.TermType.param InfiniteWeakArray.descriptor * ToTerm.TermType.param InfiniteWeakArray.descriptor

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
    | ObId_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor list
    | ParamList_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor list

      (* Num operations *)
    | MSum_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor
    | MDiff_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor
    | MProduct_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor
    | MQuotient_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor
    | MRem_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor
    | MLessThan_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor

      (* Comparisons *)
    | MEqual_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor
    | MNotEqual_weak of ToTerm.TermType.param InfiniteWeakArray.weak_descriptor * ToTerm.TermType.param InfiniteWeakArray.weak_descriptor

   type hypothesis_header =
      Hypothesis of string * ToTerm.TermType.term InfiniteWeakArray.descriptor
    | Context of string * ToTerm.TermType.term InfiniteWeakArray.descriptor list

   type hypothesis_weak_header =
      Hypothesis_weak of string * ToTerm.TermType.term InfiniteWeakArray.weak_descriptor
    | Context_weak of string * ToTerm.TermType.term InfiniteWeakArray.weak_descriptor list

   type bound_term_header = {
                         bvars: string list;
                         bterm: ToTerm.TermType.term InfiniteWeakArray.descriptor
                       }

   type bound_term_weak_header = {
                         bvars_weak: string list;
                         bterm_weak: ToTerm.TermType.term InfiniteWeakArray.weak_descriptor
                       }

   type true_term_header = {
                             op_name: Opname.opname InfiniteWeakArray.descriptor;
                             op_params: ToTerm.TermType.param InfiniteWeakArray.descriptor list;
                             term_terms: bound_term_header list
                           }

   type true_term_weak_header = {
                             op_name_weak: Opname.opname InfiniteWeakArray.weak_descriptor;
                             op_params_weak: ToTerm.TermType.param InfiniteWeakArray.weak_descriptor list;
                             term_terms_weak: bound_term_weak_header list
                           }

   type seq_header = { 
                       seq_arg: ToTerm.TermType.term InfiniteWeakArray.descriptor;
                       seq_hyps: hypothesis_header list;
                       seq_goals: ToTerm.TermType.term InfiniteWeakArray.descriptor list
                     }

   type seq_weak_header = { 
                       seq_arg_weak: ToTerm.TermType.term InfiniteWeakArray.weak_descriptor;
                       seq_hyps_weak: hypothesis_weak_header list;
                       seq_goals_weak: ToTerm.TermType.term InfiniteWeakArray.weak_descriptor list
                     }

   type term_header = Term of true_term_header
                    | Seq of seq_header

   type term_weak_header = Term_weak of true_term_weak_header
                         | Seq_weak of seq_weak_header

   type meta_term_header =
      MetaTheorem of ToTerm.TermType.term InfiniteWeakArray.descriptor
    | MetaImplies of ToTerm.TermType.meta_term InfiniteWeakArray.descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.descriptor
    | MetaFunction of ToTerm.TermType.term InfiniteWeakArray.descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.descriptor
    | MetaIff of ToTerm.TermType.meta_term InfiniteWeakArray.descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.descriptor

   type meta_term_weak_header =
      MetaTheorem_weak of ToTerm.TermType.term InfiniteWeakArray.weak_descriptor
    | MetaImplies_weak of ToTerm.TermType.meta_term InfiniteWeakArray.weak_descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.weak_descriptor
    | MetaFunction_weak of ToTerm.TermType.term InfiniteWeakArray.weak_descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.weak_descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.weak_descriptor
    | MetaIff_weak of ToTerm.TermType.meta_term InfiniteWeakArray.weak_descriptor * ToTerm.TermType.meta_term InfiniteWeakArray.weak_descriptor


(*
 * These functions convert headers to weak_headers
 *)
   val weak_param_header : param_header -> param_weak_header
   val weak_term_header : term_header -> term_weak_header
   val weak_meta_term_header : meta_term_header -> meta_term_weak_header

(*
 * These functions compare term types' weak_headers
 *)
(*   val compare_level_var_header :
      level_exp_var_weak_header -> level_exp_var_weak_header -> bool
   val compare_level_header :
      level_exp_weak_header -> level_exp_weak_header -> bool
*)
   val compare_param_header : param_weak_header -> param_weak_header -> bool
(*
   val compare_bterm_header :
      bound_term_weak_header -> bound_term_weak_header -> bool
   val compare_hyp_header :
      hypothesis_weak_header -> hypothesis_weak_header -> bool
   val compare_tterm_header :
      true_term_weak_header -> true_term_weak_header -> bool
*)
   val compare_term_header : term_weak_header -> term_weak_header -> bool
   val compare_meta_term_header :
      meta_term_weak_header -> meta_term_weak_header -> bool

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
