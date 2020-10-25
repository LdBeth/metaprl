(*
 * Run two refiners in parallel for debugging purposes.
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
 * Copyright (C) 2005-2006 MetaPRL Group, California Institute of Technology
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
 * Author: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Term_sig
open Term_addr_sig
open Term_man_sig
open Refiner_sig
open Termmod_sig
open Term_ty_sig

open Lm_symbol
open Lm_printf
open Lm_array_util
open Opname

module MakeRefinerDebug (Refiner1 : RefinerSig) (Refiner2 : RefinerSig) = struct

   (******************************************************************************************
    * MODULE ALIASES
    *)

   module Type1 = Refiner1.TermType
   module Type2 = Refiner2.TermType
   module Term1 = Refiner1.Term
   module Term2 = Refiner2.Term
   module TermAddr1 = Refiner1.TermAddr
   module TermAddr2 = Refiner2.TermAddr
   module TermOp1 = Refiner1.TermOp
   module TermOp2 = Refiner2.TermOp
   module TermMan1 = Refiner1.TermMan
   module TermMan2 = Refiner2.TermMan
(* unused
   module TermMeta1 = Refiner1.TermMeta
   module TermMeta2 = Refiner2.TermMeta
*)
   module TermSubst1 = Refiner1.TermSubst
   module TermSubst2 = Refiner2.TermSubst
   module TermShape1 = Refiner1.TermShape
   module TermShape2 = Refiner2.TermShape
   module TermTy1 = Refiner1.TermTy
   module TermTy2 = Refiner2.TermTy
   module SeqHyp1 = Refiner1.Term.SeqHyp
   module SeqHyp2 = Refiner2.Term.SeqHyp
   module Err1 = Refiner1.RefineError
   module Err2 = Refiner2.RefineError
(* unused
   module Rewrite1 = Refiner1.Rewrite
   module Rewrite2 = Refiner2.Rewrite
   module Refine1 = Refiner1.Refine
   module Refine2 = Refiner2.Refine
*)

   (******************************************************************************************
    * TYPES
    *)

   (*
    * In this module we define _all_ the refiner types (except for the RefineError ones),
    * not only those that are required by the signature to be in the TermType submodule.
    *)
   module TermType = struct
      type term = Type1.term * Type2.term
      type bound_term = Type1.bound_term * Type2.bound_term
      type operator = Type1.operator * Type2.operator
      type param = Type1.param * Type2.param
      type address = TermAddr1.address * TermAddr2.address
      type level_exp_var = Type1.level_exp_var * Type2.level_exp_var
      type level_exp = Type1.level_exp * Type2.level_exp
      type seq_hyps = Type1.seq_hyps * Type2.seq_hyps
(* unused
      type rewrite_rule = Rewrite1.rewrite_rule * Rewrite2.rewrite_rule
      type rewrite_redex = Rewrite1.rewrite_redex * Rewrite2.rewrite_redex
      type sentinal = Refine1.sentinal * Refine2.sentinal
      type tactic = Refine1.tactic * Refine2.tactic
      type rw = Refine1.rw * Refine2.rw
      type cond_rewrite = Refine1.cond_rewrite * Refine2.cond_rewrite
      type msequent = Refine1.msequent * Refine2.msequent
      type extract = Refine1.extract * Refine2.extract
      type refiner = Refine1.refiner * Refine2.refiner
      type build = Refine1.build * Refine2.build
*)

      type level_exp_var' = poly_level_exp_var
      type level_exp' = level_exp_var poly_level_exp
      type operator' = param poly_operator
      type term' = (operator, bound_term) poly_term
      type bound_term' = term poly_bound_term
      type object_id = param list
      type param' = (level_exp, param) poly_param
      type meta_term = term poly_meta_term
      type hypothesis = term poly_hypothesis
      type esequent = { sequent_args : term; sequent_hyps : seq_hyps; sequent_concl : term }
      type ty_param = term poly_ty_param
      type ty_bterm = term poly_ty_bterm
      type ty_term  = (term, term) poly_ty_term
(* unused
      type rewrite_item = (param, term, level_exp) poly_rewrite_item
*)
      type match_param = (param, level_exp) poly_match_param

      type match_term =
         MatchTerm of string list * match_param list * bound_term' list
       | MatchSequent of string list * match_term * hypothesis list * term

(* unused
      type rw_args = address rw_args_poly
      type rewrite_args = rw_args * SymbolSet.t
      type term_extract = rw_args -> term list -> term -> term list -> term
      type ml_rewrite = term -> term
      type ml_cond_rewrite = SymbolSet.t -> term list -> term -> term * term list * term_extract
      type ml_rule = rw_args -> msequent -> term list -> msequent list *  term_extract

      type ed_args = opname * int list * address list * term list

      type extract_description =
         EDRule of ed_args
       | EDRewrite of (opname * address) option
       | EDCondRewrite of (ed_args * address) option
       | EDComposition
       | EDNthHyp of int
       | EDCut of term
       | EDIdentity

      type prim_tactic = rw_args -> term list -> tactic
      type prim_rewrite =
         PrimRW of rw
       | CondRW of (rw_args -> term list -> cond_rewrite)
*)
   end

   open TermType

   module RefineError = struct
      module Types = TermType
      module Params = TermType

      type match_type =
         ParamMatch of param
       | VarMatch of var
       | TermMatch of term
       | TermMatch2 of term * term
       | BTermMatch of bound_term
       | HypMatch of seq_hyps

      type refine_error =
         GenericError

       | ToploopIgnoreError

       | StringError of string
       | IntError of int
       | TermError of term
       | StringIntError of string * int
       | StringStringError of string * string
       | StringVarError of string * var
       | StringTermError of string * term
       | StringWrapError of string * refine_error
       | SubgoalError of int * string * refine_error
       | PairError of string * refine_error * string * refine_error

       | NodeError of string * term * (string * refine_error) list
       | AddressError of address * term

       | TermMatchError of term * string
       | TermPairError of term * term
       | MetaTermMatchError of meta_term

       | RewriteFreeSOVar of var
       | RewriteSOVarArity of var
       | RewriteBoundParamVar of var
       | RewriteFreeParamVar of var
       | RewriteBadRedexParam of param
       | RewriteNoRuleOperator
       | RewriteBadMatch of match_type
       | RewriteAllSOInstances of var
       | RewriteMissingContextArg of var
       | RewriteStringError of string
       | RewriteStringOpnameOpnameError of string * opname * opname
       | RewriteAddressError of address * string * refine_error
       | RewriteFreeContextVar of var * var

       | VarError of var
       | OpnameError of opname
       | Opname2Error of opname * opname
       | ParamError of param
       | Param2Error of param * param
       | ParamTyParamError of param * ty_param
       | ShapeError of shape
       | Shape2Error of shape * shape
       | Term2Error of term * term
       | VarTermError of var * term
       | IntTermError of int * term

       | StringErrorError of string * refine_error
       | VarErrorError of var * refine_error
       | IntErrorError of int * refine_error
       | TermErrorError of term * refine_error
       | OpnameErrorError of opname * refine_error
       | ShapeErrorError of shape * refine_error
       | ParamErrorError of param * refine_error
       | MetaTermErrorError of meta_term * refine_error

      exception RefineError of string * refine_error
      exception RefineForceError of string * string * refine_error

      let generic_refiner_exn = RefineError("generic", GenericError)

   end

   open RefineError

   (******************************************************************************************
    * CONVERSIONS 1 <--> 2
    *)

   (*
    * These helper functions define functions of type  Refiner1.Mod.t -> Refiner2.Mod.t
    * for certain Mod.t types. This is needed for higher-order functions and exceptions
    *)

   let lev2_of_lev1 lev =
      let { le_var = v; le_offset = i } = Term1.dest_level_var lev in
         Term2.mk_level_var v i

   let levex2_of_levex1 levex =
      let { le_const = c; le_vars = vs } = Term1.dest_level levex in
      Term2.mk_level c (List.map lev2_of_lev1 vs)

   let rec param2_of_param1 p =
      let p =
         match Term1.dest_param p with
            (Number _ | String _ | Token _ | Shape _ | Var _ | MNumber _ | MString _ | MToken _ | MShape _ | MOperator _ | Quote) as p -> p
          | Operator op -> Operator { op with opparam_params = List.map param2_of_param1 op.opparam_params }
          | MLevel l -> MLevel (levex2_of_levex1 l)
          | ObId pl -> ObId (List.map param2_of_param1 pl)
          | ParamList pl -> ParamList (List.map param2_of_param1 pl)
      in
         Term2.make_param p

   let op2_of_op1 op =
      let { op_name = name; op_params = pl } = Term1.dest_op op in
         Term2.mk_op name (List.map param2_of_param1 pl)

   let rec term2_of_term1 t =
      if Term1.is_var_term t then
         Term2.mk_var_term (Term1.dest_var t)
      else if TermMan1.is_so_var_term t then
         let v, vs, ts = TermMan1.dest_so_var t in
            TermMan2.mk_so_var_term v vs (List.map term2_of_term1 ts)
      else if TermMan1.is_sequent_term t then
         let { Type1.sequent_args = a; Type1.sequent_hyps = h; Type1.sequent_concl = c } = TermMan1.explode_sequent t in
            TermMan2.mk_sequent_term {
               Type2.sequent_args = term2_of_term1 a;
               Type2.sequent_hyps = hyps2_of_hyps1 h;
               Type2.sequent_concl = term2_of_term1 c
            }
      else
         let { term_op = op; term_terms = btl } = Term1.dest_term t in
            Term2.mk_term (op2_of_op1 op) (List.map bterm2_of_bterm1 btl)

   and bterm2_of_bterm1 bt =
      let { bvars = vs; bterm = bt } = Term1.dest_bterm bt in
         Term2.mk_bterm vs (term2_of_term1 bt)

   and hyp2_of_hyp1 = function
      Hypothesis (v, t) -> Hypothesis(v, term2_of_term1 t)
    | Context (v, vs, ts) -> Context (v, vs, List.map term2_of_term1 ts)

   and hyps2_of_hyps1 h =
      SeqHyp2.of_list (List.map hyp2_of_hyp1 (SeqHyp1.to_list h))

   let lev1_of_lev2 lev =
      let { le_var = v; le_offset = i } = Term2.dest_level_var lev in
         Term1.mk_level_var v i

   let levex1_of_levex2 levex =
      let { le_const = c; le_vars = vs } = Term2.dest_level levex in
      Term1.mk_level c (List.map lev1_of_lev2 vs)

   let rec param1_of_param2 p =
      let p =
         match Term2.dest_param p with
            (Number _ | String _ | Token _ | Shape _ | Var _ | MNumber _ | MString _ | MToken _ | MShape _ | MOperator _ | Quote) as p -> p
          | Operator op -> Operator { op with opparam_params = List.map param1_of_param2 op.opparam_params }
          | MLevel l -> MLevel (levex1_of_levex2 l)
          | ObId pl -> ObId (List.map param1_of_param2 pl)
          | ParamList pl -> ParamList (List.map param1_of_param2 pl)
      in
         Term1.make_param p

   let op1_of_op2 op =
      let { op_name = name; op_params = pl } = Term2.dest_op op in
         Term1.mk_op name (List.map param1_of_param2 pl)

   let rec term1_of_term2 t =
      if Term2.is_var_term t then
         Term1.mk_var_term (Term2.dest_var t)
      else if TermMan2.is_so_var_term t then
         let v, vs, ts = TermMan2.dest_so_var t in
            TermMan1.mk_so_var_term v vs (List.map term1_of_term2 ts)
      else if TermMan2.is_sequent_term t then
         let { Type2.sequent_args = a; Type2.sequent_hyps = h; Type2.sequent_concl = c } = TermMan2.explode_sequent t in
            TermMan1.mk_sequent_term {
               Type1.sequent_args = term1_of_term2 a;
               Type1.sequent_hyps = hyps1_of_hyps2 h;
               Type1.sequent_concl = term1_of_term2 c
            }
      else
         let { term_op = op; term_terms = btl } = Term2.dest_term t in
            Term1.mk_term (op1_of_op2 op) (List.map bterm1_of_bterm2 btl)

   and bterm1_of_bterm2 bt =
      let { bvars = vs; bterm = bt } = Term2.dest_bterm bt in
         Term1.mk_bterm vs (term1_of_term2 bt)

   and hyp1_of_hyp2 = function
      Hypothesis (v, t) -> Hypothesis(v, term1_of_term2 t)
    | Context (v, vs, ts) -> Context (v, vs, List.map term1_of_term2 ts)

   and hyps1_of_hyps2 h =
      SeqHyp1.of_list (List.map hyp1_of_hyp2 (SeqHyp2.to_list h))

   let hyps_of_hyps1 h = h, (hyps2_of_hyps1 h)
   let param_of_param1 p = p, (param2_of_param1 p)
   let bterm_of_bterm1 bt = bt, (bterm2_of_bterm1 bt)

   let term_of_term1 t = t, (term2_of_term1 t)
   let term_of_term2 t = (term1_of_term2 t), t

   let rec mterm_of_mterm1 = function
      MetaTheorem t -> MetaTheorem (term_of_term1 t)
    | MetaImplies (mt1, mt2) -> MetaImplies (mterm_of_mterm1 mt1, mterm_of_mterm1 mt2)
    | MetaFunction (t, mt1, mt2) -> MetaFunction (term_of_term1 t, mterm_of_mterm1 mt1, mterm_of_mterm1 mt2)
    | MetaIff (mt1, mt2) -> MetaIff (mterm_of_mterm1 mt1, mterm_of_mterm1 mt2)
    | MetaLabeled (s, mt) -> MetaLabeled (s, mterm_of_mterm1 mt)

   let mtype_of_mtype1 = function
      Err1.ParamMatch p -> ParamMatch (param_of_param1 p)
    | Err1.VarMatch v -> VarMatch v
    | Err1.TermMatch t -> TermMatch (term_of_term1 t)
    | Err1.TermMatch2 (t1, t2) -> TermMatch2 (term_of_term1 t1, term_of_term1 t2)
    | Err1.BTermMatch bt -> BTermMatch (bterm_of_bterm1 bt)
    | Err1.HypMatch h -> HypMatch (hyps_of_hyps1 h)

   let addr_of_addr1 a =
      a, (TermAddr2.make_address (TermAddr1.dest_address a))

(* unused
   let addr_of_addr2 a =
      (TermAddr1.make_address (TermAddr2.dest_address a)), a

   let rwargs_of_rwargs1 a =
      { a with arg_addrs = Array.map addr_of_addr1 a.arg_addrs }

   let rwargs_of_rwargs2 a =
      { a with arg_addrs = Array.map addr_of_addr2 a.arg_addrs }
*)

   let tp_of_tp1 = function
      TyToken t -> TyToken (term_of_term1 t)
    | (TyNumber | TyString | TyShape | TyOperator | TyLevel | TyVar | TyQuote) as tp -> tp

   let rec re_of_re1 = function
    | Err1.GenericError  -> GenericError
    | Err1.ToploopIgnoreError  -> ToploopIgnoreError
    | Err1.StringError s0 -> StringError (s0)
    | Err1.IntError i0 -> IntError (i0)
    | Err1.TermError t0 -> TermError (term_of_term1 t0)
    | Err1.StringIntError (s0, i1) -> StringIntError (s0, i1)
    | Err1.StringStringError (s0, s1) -> StringStringError (s0, s1)
    | Err1.StringVarError (s0, v1) -> StringVarError (s0, v1)
    | Err1.StringTermError (s0, t1) -> StringTermError (s0, term_of_term1 t1)
    | Err1.StringWrapError (s0, re1) -> StringWrapError (s0, re_of_re1 re1)
    | Err1.SubgoalError (i0, s1, re2) -> SubgoalError (i0, s1, re_of_re1 re2)
    | Err1.PairError (s0, re1, s2, re3) -> PairError (s0, re_of_re1 re1, s2, re_of_re1 re3)
    | Err1.NodeError (s0, t1, sre2) -> NodeError (s0, term_of_term1 t1, List.map sre_of_sre1 sre2)
    | Err1.AddressError (a0, t1) -> AddressError (addr_of_addr1 a0, term_of_term1 t1)
    | Err1.TermMatchError (t0, s1) -> TermMatchError (term_of_term1 t0, s1)
    | Err1.TermPairError (t0, t1) -> TermPairError (term_of_term1 t0, term_of_term1 t1)
    | Err1.MetaTermMatchError mt0 -> MetaTermMatchError (mterm_of_mterm1 mt0)
    | Err1.RewriteFreeSOVar v0 -> RewriteFreeSOVar (v0)
    | Err1.RewriteSOVarArity v0 -> RewriteSOVarArity (v0)
    | Err1.RewriteBoundParamVar v0 -> RewriteBoundParamVar (v0)
    | Err1.RewriteFreeParamVar v0 -> RewriteFreeParamVar (v0)
    | Err1.RewriteBadRedexParam p0 -> RewriteBadRedexParam (param_of_param1 p0)
    | Err1.RewriteNoRuleOperator  -> RewriteNoRuleOperator
    | Err1.RewriteBadMatch mt0 -> RewriteBadMatch (mtype_of_mtype1 mt0)
    | Err1.RewriteAllSOInstances v0 -> RewriteAllSOInstances (v0)
    | Err1.RewriteMissingContextArg v0 -> RewriteMissingContextArg (v0)
    | Err1.RewriteStringError s0 -> RewriteStringError (s0)
    | Err1.RewriteStringOpnameOpnameError (s0, o1, o2) -> RewriteStringOpnameOpnameError (s0, o1, o2)
    | Err1.RewriteAddressError (a0, s1, re2) -> RewriteAddressError (addr_of_addr1 a0, s1, re_of_re1 re2)
    | Err1.RewriteFreeContextVar (v0, v1) -> RewriteFreeContextVar (v0, v1)
    | Err1.VarError v0 -> VarError (v0)
    | Err1.OpnameError o0 -> OpnameError (o0)
    | Err1.Opname2Error (o0, o1) -> Opname2Error (o0, o1)
    | Err1.ParamError p0 -> ParamError (param_of_param1 p0)
    | Err1.Param2Error (p0, p1) -> Param2Error (param_of_param1 p0, param_of_param1 p1)
    | Err1.ParamTyParamError (p0, tp1) -> ParamTyParamError (param_of_param1 p0, tp_of_tp1 tp1)
    | Err1.ShapeError s0 -> ShapeError s0
    | Err1.Shape2Error (s0, s1) -> Shape2Error (s0, s1)
    | Err1.Term2Error (t0, t1) -> Term2Error (term_of_term1 t0, term_of_term1 t1)
    | Err1.VarTermError (v0, t1) -> VarTermError (v0, term_of_term1 t1)
    | Err1.IntTermError (i0, t1) -> IntTermError (i0, term_of_term1 t1)
    | Err1.StringErrorError (s0, re1) -> StringErrorError (s0, re_of_re1 re1)
    | Err1.VarErrorError (v0, re1) -> VarErrorError (v0, re_of_re1 re1)
    | Err1.IntErrorError (i0, re1) -> IntErrorError (i0, re_of_re1 re1)
    | Err1.TermErrorError (t0, re1) -> TermErrorError (term_of_term1 t0, re_of_re1 re1)
    | Err1.OpnameErrorError (o0, re1) -> OpnameErrorError (o0, re_of_re1 re1)
    | Err1.ShapeErrorError (s0, re1) -> ShapeErrorError (s0, re_of_re1 re1)
    | Err1.ParamErrorError (p0, re1) -> ParamErrorError (param_of_param1 p0, re_of_re1 re1)
    | Err1.MetaTermErrorError (mt0, re1) -> MetaTermErrorError (mterm_of_mterm1 mt0, re_of_re1 re1)

   and sre_of_sre1 (s, re) =
      s, re_of_re1 re

   (******************************************************************************************
    * DEBUG TERM PRINTING
    *)

   let print_term_ref = ref (fun _ _ -> raise (Failure "Refiner_debug.Term.print_term: printer not installed"))

   let print_term out (t: term) =
      !print_term_ref out t

   let rec print_term_list out = function
      [t] ->
         print_term out t
    | h::t ->
         print_term out h;
         output_string out ", ";
         print_term_list out t
    | [] ->
         ()

   (******************************************************************************************
    * EXCEPTIONS WRAPPING
    *)

   (*
    * In order to catch the cases where one implementation raises an exception and another
    * succeeds, we need to catch exceptions. We also need to take into account that an
    * exception may be raised in a partial application
    *)

   type 'a wrap =
      Val of 'a
    | Err of exn

   let wrap1 f a =
      try Val (f a) with exn -> Err exn

   let wrap_plus f a =
      match f with
         Val f -> wrap1 f a
       | (Err _) as err -> err

   let wrap2 f a1 = wrap_plus (wrap1 f a1)
   let wrap3 f a1 a2 = wrap_plus (wrap2 f a1 a2)
   let wrap4 f a1 a2 a3 = wrap_plus (wrap3 f a1 a2 a3)
   let wrap5 f a1 a2 a3 a4 = wrap_plus (wrap4 f a1 a2 a3 a4)
   let wrap6 f a1 a2 a3 a4 a5 = wrap_plus (wrap5 f a1 a2 a3 a4 a5)
   let wrap7 f a1 a2 a3 a4 a5 a6 = wrap_plus (wrap6 f a1 a2 a3 a4 a5 a6)
   let wrap8 f a1 a2 a3 a4 a5 a6 a7 = wrap_plus (wrap7 f a1 a2 a3 a4 a5 a6 a7)
   let wrap9 f a1 a2 a3 a4 a5 a6 a7 a9 = wrap_plus (wrap8 f a1 a2 a3 a4 a5 a6 a7 a9)

   (******************************************************************************************
    * SPLITS
    *)

   (*
    * This section defines functions of the type   t -> Refiner1.Mod.t * Refiner1,Mod.t
    * and in general
    * TypeFun(Refiner_debug types) -> TypeFun(Refiner1 types) * TypeFun(Refiner2 types)
    *)
   let split = List.split

(* unused
   let split_opt = function
      None -> None, None
    | Some (a, b) -> Some a, Some b

   let split_array a =
      (Array.map fst a), (Array.map snd a)

   let split_pop (po, (p1, p2)) =
      let po1, po2 = split_opt po in ((po1, p1), (po2, p2))

   let split_popl l =
      split (List.map split_pop l)

   let split_sp (s, (p1, p2)) =
      (s, p1), (s, p2)

   let split_spl l =
      split (List.map split_sp l)
*)

   let split_term' { term_op = (op1, op2); term_terms = btl } =
      let btl1, btl2 = split btl in
         { term_op = op1; term_terms = btl1 },
         { term_op = op2; term_terms = btl2 }

   let split_bterm' { bvars = vs; bterm = bt1, bt2 } =
      { bvars = vs; bterm = bt1 },
      { bvars = vs; bterm = bt2 }

   let split_op' { op_name = name; op_params = pars } =
      let pl1, pl2 = split pars in
         { op_name = name; op_params = pl1 },
         { op_name = name; op_params = pl2 }

   let split_level_exp' { le_const = c; le_vars = vs } =
      let vs1, vs2 = split vs in
         { le_const = c; le_vars = vs1 },
         { le_const = c; le_vars = vs2 }

   let split_opparam op =
      let pl1, pl2 = split op.opparam_params in
         { op with opparam_params = pl1 }, { op with opparam_params = pl2 }

   let split_param' = function
      (Number _ | String _ | Token _ | Var _ | Shape _ | MNumber _ | MString _ | MToken _ | MShape _ | MOperator _ | Quote) as p -> p, p
    | Operator op -> let op1, op2 = split_opparam op in Operator op1, Operator op2
    | MLevel (l1, l2) -> MLevel l1, MLevel l2
    | ObId pl -> let pl1, pl2 = split pl in ObId pl1, ObId pl2
    | ParamList pl -> let pl1, pl2 = split pl in ParamList pl1, ParamList pl2

   let split_hyp = function
      Hypothesis (v, (t1, t2)) ->
         Hypothesis (v, t1), Hypothesis (v, t2)
    | Context(v, vs, ts) ->
         let ts1, ts2 = split ts in
            Context (v, vs, ts1), Context(v, vs, ts2)

   let split_hyps hs =
      split (List.map split_hyp hs)

   let split_eseq { sequent_args = a1, a2; sequent_hyps = h1, h2; sequent_concl = c1, c2 } =
      { Type1.sequent_args = a1; Type1.sequent_hyps = h1; Type1.sequent_concl = c1 },
      { Type2.sequent_args = a2; Type2.sequent_hyps = h2; Type2.sequent_concl = c2 }

   let split_term_subst sub =
      let vars, terms = split sub in
      let ts1, ts2 = split terms in
         (List.combine vars ts1), (List.combine vars ts2)

(* unused
   let rec split_meta_term = function
      MetaTheorem (t1, t2) ->
         MetaTheorem t1, MetaTheorem t2
    | MetaImplies (mta, mtb) ->
         let mta1, mta2 = split_meta_term mta in
         let mtb1, mtb2 = split_meta_term mtb in
            MetaImplies (mta1, mtb1), MetaImplies (mta2, mtb2)
    | MetaFunction ((t1, t2), mta, mtb) ->
         let mta1, mta2 = split_meta_term mta in
         let mtb1, mtb2 = split_meta_term mtb in
            MetaFunction (t1, mta1, mtb1), MetaFunction (t2, mta2, mtb2)
    | MetaIff (mta, mtb) ->
         let mta1, mta2 = split_meta_term mta in
         let mtb1, mtb2 = split_meta_term mtb in
            MetaIff (mta1, mtb1), MetaIff (mta2, mtb2)
    | MetaLabeled (s, mt) ->
         let mt1, mt2 = split_meta_term mt in MetaLabeled (s, mt1), MetaLabeled (s, mt2)

   let split_args a =
      let aa1, aa2 = split_array a.arg_addrs in
         { a with arg_addrs = aa1 }, { a with arg_addrs = aa2 }

   let split_rewrite_args (a, ss) =
      let a1, a2 = split_args a in
         (a1, ss), (a2, ss)
*)

   let split_ty_param = function
      TyToken (t1, t2) -> TyToken t1, TyToken t2
    | (TyNumber | TyString | TyShape | TyOperator | TyLevel | TyVar | TyQuote) as tp -> tp, tp

   let split_ty_params pl =
      split (List.map split_ty_param pl)

   let split_ty_bterm { ty_bvars = tl; ty_bterm = t1, t2 } =
      let tl1, tl2 = split tl in
         { ty_bvars = tl1; ty_bterm = t1 }, { ty_bvars = tl2; ty_bterm = t2 }

   let split_ty_bterms bl =
      split (List.map split_ty_bterm bl)

   let split_ty_term { ty_term = t1, t2; ty_opname = o; ty_params = pl; ty_bterms = bl; ty_type = tt1, tt2 } =
      let pl1, pl2 = split_ty_params pl in
      let bl1, bl2 = split_ty_bterms bl in
         { ty_term = t1; ty_opname = o; ty_params = pl1; ty_bterms = bl1; ty_type = tt1 },
         { ty_term = t2; ty_opname = o; ty_params = pl2; ty_bterms = bl2; ty_type = tt2 }

   (*
    * Splits for function types need to use "Refiner1 -> Refiner_debug" and
    * "Refiner2 -> Refiner_debug" conversions
    *)

   let split_taf f =
      (fun t1 -> f (term_of_term1 t1)), (fun t2 -> f (term_of_term2 t2))

   let split_ttf f =
      (fun t1 -> fst (f (term_of_term1 t1))),
      (fun t2 -> snd (f (term_of_term2 t2)))

   let split_attf f =
      (fun a t1 -> fst (f a (term_of_term1 t1))),
      (fun a t2 -> snd (f a (term_of_term2 t2)))

(* unused
   let split_atf f =
      (fun a -> fst (f a)), (fun a -> snd (f a))
*)

   let split_ttaf f =
      (fun t1 -> let t, res = f (term_of_term1 t1) in fst t, res),
      (fun t2 -> let t, res = f (term_of_term2 t2) in snd t, res)

   let split_attaf f =
      (fun a t1 -> let t, res = f a (term_of_term1 t1) in fst t, res),
      (fun a t2 -> let t, res = f a (term_of_term2 t2) in snd t, res)

(* unused
   let split_ml_rule r =
      (fun ia ms tl -> raise (Invalid_argument "Refine_debug.split_ml_rule: not implemented")),
      (fun ia ms tl -> raise (Invalid_argument "Refine_debug.split_ml_rule: not implemented"))

   let split_term_extract e =
      (fun a tla t tlb ->
         fst (e (rwargs_of_rwargs1 a) (List.map term_of_term1 tla) (term_of_term1 t) (List.map term_of_term1 tlb))),
      (fun a tla t tlb ->
         snd (e (rwargs_of_rwargs2 a) (List.map term_of_term2 tla) (term_of_term2 t) (List.map term_of_term2 tlb)))

   let split_ml_cond_rewrite crw =
      (fun s tl t ->
         let tl = List.map term_of_term1 tl in
         let (t1, _), tl, e = crw s tl (term_of_term1 t) in
            t1, fst (split tl), fst (split_term_extract e)),
      (fun s tl t ->
         let tl = List.map term_of_term2 tl in
         let (_, t2), tl, e = crw s tl (term_of_term2 t) in
            t2, snd (split tl), snd (split_term_extract e))

   let split_utriv f =
      (fun () -> fst (f ())), (fun () -> snd (f ()))
*)

   (******************************************************************************************
    * MERGES
    *)

   (*
    * The merge functions in general have the type
    * string -> TypeFun(Refiner1) -> TypeFun(Refiner2) -> TypeFun(Refiner)
    * where TypeFun just specifies some particular combinarion of the refiner types.
    * The string argument (which we always call "x" for brievity) is the name of the API
    * function that is responsible for the merge (and which should be blamed if the values
    * are inconsisntent. The merge functions _must_ test the two values for consistency if
    * at all possible.
    *)

   (*
    * We use a separate error reporting function to have a single breakpoint
    * location that can be used to catch _all_ error in the debugger
    *)
   let report_error x msg =
      raise (Invalid_argument ("Found a mismatch in function " ^ x ^ ": " ^ msg))

   let merge merge_fun x v1 v2 =
      match v1, v2 with
         Val v1, Val v2 -> merge_fun x v1 v2
       | Err (Err1.RefineError (s1, err1)), Err (Err2.RefineError _) -> raise (RefineError (s1, re_of_re1 err1))
       | Err (RefineError _ as exn), Err (RefineError _) -> raise exn
       | Err (Err1.RefineError _), _
       | _, Err (Err2.RefineError _)
       | Err (RefineError _), _
       | _, Err (RefineError _) -> report_error x "One implementation raise RE, another did not"
       | Err exn1, Err exn2 when exn1 = exn2 -> raise exn1
       | Err (Failure s1), Err (Failure s2) -> raise (Failure ("Impl1: " ^ s1 ^ "; Impl2: " ^ s2))
       | Err (Invalid_argument s1), Err (Invalid_argument s2) -> raise (Invalid_argument ("Impl1: " ^ s1 ^ "; Impl2: " ^ s2))
       | Err exn, _
       | _, Err exn -> raise exn

   let merge_triv x v1 v2 =
      v1, v2

   let merge_poly x v1 v2 =
      if v1 <> v2 then
         report_error x "Polymorphic merge";
      v1

(* unused
   let merge_addr_item = merge_poly
*)
   let merge_addr_items = merge_poly

   let merge_bool x (b1:bool) b2 =
      if b1 <> b2 then
         report_error x "Booleans mismatch";
      b1

   let merge_int x (i1: int) i2 =
      if i1 <> i2 then
         report_error x (sprintf "Integers mismatch: %i vs %i" i1 i2);
      i1

   let merge_var x v1 v2 =
      if not (Lm_symbol.eq v1 v2) then
         report_error x ("Variable mismatch: \"" ^ (string_of_symbol v1) ^ "\" vs \"" ^ (string_of_symbol v2) ^ "\"");
      v1

   let merge_string x s1 s2 =
      if s1 <> s2 then
         report_error x ("Strings mismatch: \"" ^s1^"\" vs \""^s2^"\"");
      s1

   let merge_num x n1 n2 =
      if not (Lm_num.eq_num n1 n2) then
          report_error x "nums mismatch";
      n1

   let merge_shape_param x (sp1 : shape_param) sp2 =
      if sp1 <> sp2 then
         report_error x "Shape_param mismatch";
      sp1

   let merge_unit x () () = ()

   let merge_list merge name x l1 l2 =
      if not (List.length l1 = List.length l2) then
         report_error x (name ^ " lists length mismatch");
      List.map2 (merge x) l1 l2

(* unused
   let merge_array merge name x a1 a2 =
      if not (Array.length a1 = Array.length a2) then
         report_error x (name ^ " array length mismatch");
      Array.of_list (List.map2 (merge x) (Array.to_list a1) (Array.to_list a2))

   let merge_opt merge name x o1 o2 =
      match o1, o2 with
         None, None -> None
       | Some v1, Some v2 -> Some (merge x v1 v2)
       | _ -> report_error x (name ^ " option None/Some mismatch")
*)

   let merge_ints = merge_list merge_int "integer"
   let merge_vars = merge_list merge_var "var"
(* unused
   let merge_int_arr = merge_array merge_int "integer"
   let merge_var_arr = merge_array merge_var "var"
*)
   let merge_strings = merge_list merge_string "string"
(* unused
   let merge_var_lo = merge_opt merge_vars "var list"
*)

   let merge_ss x s1 s2 =
      if not (SymbolSet.equal s1 s2) then
         report_error x ("Symbol sets mismatch: Implementation 1: {" ^
            (String.concat ", " (List.map string_of_symbol (SymbolSet.elements s1))) ^
            "}; Implementation 2: {" ^
            (String.concat ", " (List.map string_of_symbol (SymbolSet.elements s2))) ^
            "}");
      s1

   let merge_param x p1 p2 =
      (* XXX: TODO: need some consistency checks *)
      p1, p2

   let merge_level_exp_var x v1 v2 =
      (* XXX: TODO: need some consistency checks *)
      v1, v2

   let merge_level_exp x le1 le2 =
      (* XXX: TODO: need some consistency checks *)
      le1, le2

   let merge_address x a1 a2 =
      (* XXX: TODO: need some consistency checks *)
      a1, a2

(* unused
   let merge_msequent x ms1 ms2 =
      let t1, ts1 = Refine1.dest_msequent ms1 in
      let t2, ts2 = Refine2.dest_msequent ms2 in
         if not (List.length ts1 = List.length ts2) then
            report_error x "assumption number mismatch in msequent";
         (* XXX: TODO: need some more consistency checks *)
         ms1, ms2

   let merge_msequents =  merge_list merge_msequent "msequent"
*)
   let merge_addresss = merge_list merge_address "address"
   let merge_params = merge_list merge_param "param"
(* unused
   let merge_address_arr = merge_array merge_address "address"
*)

   let merge_opname x op1 op2 =
      if not (Opname.eq op1 op2) then
         report_error x "opnames mismatch";
      op1

   let merge_opparam x op1 op2 =
      { opparam_name = merge_opname x op1.opparam_name op2.opparam_name;
        opparam_params = merge_params x op1.opparam_params op2.opparam_params;
        opparam_arities = merge_ints x op1.opparam_arities op2.opparam_arities
      }

   let merge_param' x p1 p2 =
      match p1, p2 with
         (Number _ | String _ | Token _ | Shape _ | Var _ | MNumber _ | MString _ | MToken _ | MShape _ | MOperator _ | Quote as p1),
         (Number _ | String _ | Token _ | Shape _ | Var _ | MNumber _ | MString _ | MToken _ | MShape _ | MOperator _ | Quote as p2)
         when p1 = p2 ->
            p1
       | Operator op1, Operator op2 -> Operator (merge_opparam x op1 op2)
       | MLevel l1, MLevel l2 -> MLevel (l1, l2)
       | ObId pl1, ObId pl2 -> ObId (merge_params x pl1 pl2)
       | ParamList pl1, ParamList pl2 -> ParamList (merge_params x pl1 pl2)
       | _ -> report_error x "incompatible param'"

   let merge_params' = merge_list merge_param' "param'"
   let merge_level_exp_vars = merge_list merge_level_exp_var "level_exp_var"

   let merge_level_exp_var' x { le_var = v1; le_offset = i1 } { le_var = v2; le_offset = i2 } =
      if not (Lm_symbol.eq v1 v2) then
         report_error x "le_var field mismatch";
      if not (i1 = i2) then
         report_error x "le_offset field mismatch";
      { le_var = v1; le_offset = i1 }

(* unused
   let merge_level_exp_vars' = merge_list merge_level_exp_var' "level_exp_var'"
*)

   let merge_level_exp' x { le_const = c1; le_vars = vs1 } { le_const = c2; le_vars = vs2 } =
      if not (c1 = c2) then
         report_error x "le_const field mismatch";
      { le_const = c1; le_vars = merge_level_exp_vars x vs1 vs2 }

   let merge_op' x { op_name = name1; op_params = pl1 } { op_name = name2; op_params = pl2 } =
      if not (Opname.eq name1 name2) then
         report_error x "operator' opname mismatch";
      { op_name = name1; op_params = merge_params x pl1 pl2 }

   let merge_op x op1 op2 =
      (* XXX: TODO: need some consistency checks *)
      op1, op2

   let merge_term =
      let compare_terms t1 t2 =
         if TermShape1.shape_of_term t1 = TermShape2.shape_of_term t2 then
            (*
             * Much more precise, but very expensive:
            Lm_list_util.for_all2 compare_terms (Term1.subterms_of_term t1) (Term2.subterms_of_term t2)
             *)
            true
         else begin
            eprintf "[sub]term shape mismatch:\n\t%a%t" print_term (t1, t2) eflush;
            false
         end
      in
         fun x t1 t2 ->
            if compare_terms t1 t2 then
               (t1, t2)
            else begin
               eprintf "Term mismatch:\n\t%a%t" print_term (t1, t2) eflush;
               report_error x "terms mismatch"
            end

(* unused
   let merge_ttf x f1 f2 =
      fun (t1, t2) -> merge_term x (f1 t1) (f2 t2)

   let rec merge_meta_term x mt1 mt2 =
      match mt1, mt2 with
         MetaTheorem t1, MetaTheorem t2 ->
            MetaTheorem (merge_term x t1 t2)
       | MetaImplies (mt1a, mt1b), MetaImplies (mt2a, mt2b) ->
            MetaImplies (merge_meta_term x mt1a mt2a, merge_meta_term x mt1b mt2b)
       | MetaFunction (t1, mt1a, mt1b), MetaFunction (t2, mt2a, mt2b) ->
            MetaFunction (merge_term x t1 t2, merge_meta_term x mt1a mt2a, merge_meta_term x mt1b mt2b)
       | MetaIff (mt1a, mt1b), MetaIff (mt2a, mt2b) ->
            MetaIff (merge_meta_term x mt1a mt2a, merge_meta_term x mt1b mt2b)
       | MetaLabeled (s1, mt1), MetaLabeled (s2, mt2) ->
            MetaLabeled (merge_string x s1 s2, merge_meta_term x mt1 mt2)
       | _ ->
            report_error x "meta term kind mismatch"
*)

   let merge_shape x s1 s2 =
      if s1 <> s2 then
         report_error x "shape mismatch"
      else
         s1

   let merge_tsub x (v1, t1) (v2, t2) =
      (merge_var x v1 v2), (merge_term x t1 t2)

   let merge_terms = merge_list merge_term "term"
(* unused
   let merge_term_opt = merge_opt merge_term "term"
*)
   let merge_term_subst = merge_list merge_tsub "term_subst"

(* unused
   let merge_sltot x (sl1, to1, t1) (sl2, to2, t2) =
      (merge_strings x sl1 sl2), (merge_term_opt x to1 to2), (merge_term x t1 t2)

   let merge_sltotlt x (l1, t1) (l2, t2) =
      (merge_list merge_sltot "string list * term option * term" x l1 l2), (merge_term x t1 t2)
*)

   let merge_bterm' x { bvars = bv1; bterm = t1 } { bvars = bv2; bterm = t2 } =
      if not (List.length bv1 = List.length bv2) then
         report_error x "bvar length mismatch";
      { bvars = bv1; bterm = merge_term x t1 (TermSubst2.subst t2 (List.rev bv2) (List.rev_map Term2.mk_var_term bv1)) }

   let merge_bterm x bt1 bt2 =
      (* XXX: TODO: need some consistency checks *)
      bt1, bt2

   let merge_bterms' = merge_list merge_bterm' "bound_term'"
   let merge_bterms = merge_list merge_bterm "bterm"

   let merge_term' x { term_op = op1; term_terms = btl1 } { term_op = op2; term_terms = btl2 } =
      { term_op = merge_op x op1 op2; term_terms = merge_bterms x btl1 btl2 }

   let merge_hyp x h1 h2 =
      match h1, h2 with
         Hypothesis (v1, t1), Hypothesis (v2, t2) ->
            if v1 <> v2 then
               report_error x "Hyp variable mismatch";
            Hypothesis (v1, merge_term x t1 t2)
       | Context (v1, vs1, ts1), Context (v2, vs2, ts2) ->
            if v1 <> v2 then
               report_error x "Context variable mismatch";
            if vs1 <> vs2 then
               report_error x "Contexts of a context mismatch";
            Context (v1, vs1, merge_terms x ts1 ts2)
       | _ ->
            report_error x "hypothesis kind mismatch"

   let merge_hyps = merge_list merge_hyp "hyp"

   let merge_SeqHyp x hyps1 hyps2 =
      if not (SeqHyp1.length hyps1 = SeqHyp2.length hyps2) then
         report_error x "SeqHyp.length mismatch on merge";
      hyps1, hyps2

   let merge_and_rename x h1 h2 =
      if not (SeqHyp1.length h1 = SeqHyp2.length h2) then
         report_error x "SeqHyp.length mismatch on merge_and_rename";
      let rec aux l1 l2 sub =
         match l1, l2 with
            [], [] -> [], sub
          | Hypothesis (v1, t1) :: tl1, Hypothesis (v2, t2) :: tl2 ->
               let t2 = TermSubst2.apply_subst sub t2 in
               let hyps, sub = aux tl1 tl2 ((v2, Term2.mk_var_term v1) :: sub) in
                  (Hypothesis (v1, t2) :: hyps), sub
          | Context (v1, vs1, ts1) :: tl1, Context (v2, vs2, ts2) :: tl2 ->
               if v1 <> v2 then
                  report_error x "Context variable mismatch on merge_and_rename";
               if vs1 <> vs2 then
                  report_error x "Contexts of a context mismatch on merge_and_rename";
               let ts2 = List.map (TermSubst2.apply_subst sub) ts2 in
               let hyps, sub = aux tl1 tl2 sub in
                 (Context (v1, vs1, ts2) :: hyps), sub
          | _ -> report_error x "hypothesis kind mismatch on merge_and_rename"
      in
         let hyps, sub = aux (SeqHyp1.to_list h1) (SeqHyp2.to_list h2) [] in
            (h1, (SeqHyp2.of_list hyps)), sub

   let merge_esequent x { Type1.sequent_args = a1; Type1.sequent_hyps = h1; Type1.sequent_concl = c1 } { Type2.sequent_args = a2; Type2.sequent_hyps = h2; Type2.sequent_concl = c2 } =
      let hyps, sub = merge_and_rename x h1 h2 in {
         sequent_args = merge_term x a1 a2;
         sequent_hyps = hyps;
         sequent_concl = merge_term x c1 (TermSubst2.apply_subst sub c2) }

   let merge_match_param x p1 p2 =
      match p1, p2 with
         MatchNumber (n1, None), MatchNumber (n2, None) -> MatchNumber (merge_num x n1 n2, None)
       | MatchNumber (n1, Some i1), MatchNumber (n2, Some i2) -> MatchNumber (merge_num x n1 n2, Some (merge_int x i1 i2))
       | MatchString s1, MatchString s2 -> MatchString (merge_string x s1 s2)
       | MatchToken (o1, s1), MatchToken (o2, s2) -> MatchToken (merge_opname x o1 o2, merge_strings x s1 s2)
       | MatchVar v1, MatchVar v2 -> MatchVar (merge_var x v1 v2)
       | MatchLevel l1, MatchLevel l2 -> MatchLevel (merge_level_exp x l1 l2)
       | MatchUnsupported, MatchUnsupported -> MatchUnsupported
       | _ -> report_error x "match_param kind mismatch"

   let merge_match_params = merge_list merge_match_param "match param"

   let rec merge_match_term x mt1 mt2 =
      match mt1, mt2 with
         Type1.MatchTerm (sl1, pl1, bl1), Type2.MatchTerm (sl2, pl2, bl2) ->
            MatchTerm (merge_strings x sl1 sl2, merge_match_params x pl1 pl2, merge_bterms' x bl1 bl2)
       | Type1.MatchSequent (sl1, mt1, hl1, t1), Type2.MatchSequent (sl2, mt2, hl2, t2) ->
            MatchSequent (merge_strings x sl1 sl2, merge_match_term x mt1 mt2, merge_hyps x hl1 hl2, merge_term x t1 t2)
       | _ -> report_error x "match_term kind mismatch"

(* unused
   and merge_match_terms x mtl1 mtl2 = merge_list merge_match_term "match term" x mtl1 mtl2

   let merge_rwspecs x sp1 sp2 = {
      spec_addrs = merge_var_arr x sp1.spec_addrs sp2.spec_addrs;
      spec_ints = merge_var_arr x sp1.spec_ints sp2.spec_ints
   }

   let merge_rwargs x a1 a2 = {
      arg_ints = merge_int_arr x a1.arg_ints a2.arg_ints;
      arg_addrs = merge_address_arr x a1.arg_addrs a2.arg_addrs
   }

   let merge_rewrite_args x (a1, ss1) (a2, ss2) =
      (merge_rwargs x a1 a2), (merge_ss x ss1 ss2)

   let merge_rwtyp x (rwt1: rewrite_type) rwt2 =
      if rwt1 <> rwt2 then
         report_error x "rewrite_type mismatch";
      rwt1

   let merge_rwtv x (rt1, v1) (rt2, v2) =
      (merge_rwtyp x rt1 rt2), (merge_var x v1 v2)

   let merge_rwtvl = merge_list merge_rwtv "rewrite_type * var"

   let merge_rwp merge x p1 p2 =
      match p1, p2 with
         RewriteParam p1, RewriteParam p2 -> RewriteParam (merge x p1 p2)
       | RewriteMetaParam v1, RewriteMetaParam v2 -> RewriteMetaParam (merge_var x v1 v2)
       | _ -> report_error x "rewrite_param kind mismatch"

   let merge_rewrite_item x i1 i2 =
      match i1, i2 with
         RewriteTerm t1, RewriteTerm t2 ->
             RewriteTerm (merge_term x t1 t2)
       | RewriteString s1, RewriteString s2 ->
            RewriteString (merge_rwp merge_string x s1 s2)
       | RewriteToken t1, RewriteToken t2 ->
            RewriteToken (merge_rwp merge_opname x t1 t2)
       | RewriteNum n1, RewriteNum n2 ->
            RewriteNum (merge_rwp merge_num x n1 n2)
       | RewriteLevel le1, RewriteLevel le2 ->
            RewriteLevel (merge_level_exp x le1 le2)
       | RewriteUnsupported, RewriteUnsupported ->
            RewriteUnsupported
       | _ ->
            report_error x "rewrite_item kind mismatch"

   let merge_ib x (i1, b1) (i2, b2) =
      (merge_int x i1 i2), (merge_bool x b1 b2)

   let merge_rewrite_items = merge_list merge_rewrite_item "rewrite_item"
   let merge_ibl = merge_list merge_ib "int * bool"

   let merge_extract_description x ed1 ed2 =
      match ed1, ed2 with
         Refine1.EDRule (o1, il1, al1, tl1), Refine2.EDRule (o2, il2, al2, tl2) ->
            EDRule (merge_opname x o1 o2, merge_ints x il1 il2, merge_addresss x al1 al2, merge_terms x tl1 tl2)
       | Refine1.EDRewrite (Some (o1, a1)), Refine2.EDRewrite (Some (o2, a2)) ->
            EDRewrite (Some ((merge_opname x o1 o2, merge_address x a1 a2)))
       | Refine1.EDCondRewrite (Some ((o1, il1, al1, tl1), a1)),
         Refine2.EDCondRewrite (Some ((o2, il2, al2, tl2), a2))->
            EDCondRewrite (Some ((merge_opname x o1 o2, merge_ints x il1 il2, merge_addresss x al1 al2, merge_terms x tl1 tl2), merge_address x a1 a2))
       | Refine1.EDRewrite None, Refine2.EDRewrite None -> EDRewrite None
       | Refine1.EDCondRewrite None, Refine2.EDCondRewrite None -> EDCondRewrite None
       | Refine1.EDComposition, Refine2.EDComposition -> EDComposition
       | Refine1.EDNthHyp i1, Refine2.EDNthHyp i2 -> EDNthHyp (merge_int x i1 i2)
       | Refine1.EDCut t1, Refine2.EDCut t2 -> EDCut (merge_term x t1 t2)
       | Refine1.EDIdentity, Refine2.EDIdentity -> EDIdentity
       | _ -> report_error x "extract_description kind mismatch"

   let merge_do x (d1, o1) (d2, o2) =
      (merge_poly x d1 d2), (merge_opname x o1 o2)

   let merge_dos = merge_list merge_do "dependency * opname"

   let merge_prim_tactic x pt1 pt2 a tl =
      let a1, a2 = split_args a in
      let tl1, tl2 = split tl in merge merge_triv x (wrap2 pt1 a1 tl1) (wrap2 pt2 a2 tl2)

   let merge_prim_rewrite x pr1 pr2 =
      match pr1, pr2 with
         Refine1.PrimRW rw1, Refine2.PrimRW rw2 -> PrimRW (rw1, rw2)
       | Refine1.CondRW rw1, Refine2.CondRW rw2 ->
            let rw a tl =
               let tl1, tl2 = split tl in
               let a1, a2 = split_args a in
                  merge merge_triv x (wrap2 rw1 a1 tl1) (wrap2 rw2 a2 tl2)
            in
               CondRW rw
       | _ -> report_error x "prim_rewrite kind mismatch"
*)

   let merge_stables x t1 t2 =
      let tst t s v =
         if not (SymbolTable.mem t s) then
            report_error x ("table membership mismatch for " ^ (string_of_symbol s));
         if (SymbolTable.find t s) <> v then
            report_error x ("table value mismatch for " ^ (string_of_symbol s));
      in
         SymbolTable.iter (tst t2) t1;
         SymbolTable.iter (tst t1) t2;
         t1

   (******************************************************************************************
    * The SeqHyp Module
    *)

   module SeqHyp = struct
      type elt = hypothesis
      type t = seq_hyps
      type index = int

      let empty =
         merge_SeqHyp "SeqHyp.empty" SeqHyp1.empty SeqHyp2.empty

      let singleton h =
         let h1, h2 = split_hyp h in
            merge_SeqHyp "SeqHyp.singleton" (SeqHyp1.singleton h1) (SeqHyp2.singleton h2)

      let length (t1, t2) =
         merge_int "SeqHyp.length" (SeqHyp1.length t1) (SeqHyp2.length t2)

      let make i h =
         let h1, h2 = split_hyp h in
            merge_SeqHyp "SeqHyp.make" (SeqHyp1.make i h1) (SeqHyp2.make i h2)

      let create = make

      let get (t1, t2) i =
         merge_hyp "SeqHyp.get" (SeqHyp1.get t1 i) (SeqHyp2.get t2 i)

      let to_list (t1, t2) =
         merge_hyps "SeqHyp.to_list" (SeqHyp1.to_list t1) (SeqHyp2.to_list t2)

      let of_list hl =
         let hl1, hl2 = split_hyps hl in
            merge_SeqHyp "SeqHyp.of_list" (SeqHyp1.of_list hl1) (SeqHyp2.of_list hl2)

      let concat (t1, t2) (tt1, tt2) =
         merge_SeqHyp "SeqHyp.concat" (SeqHyp1.concat t1 tt1) (SeqHyp2.concat t2 tt2)

      let append (t1, t2) h (tt1, tt2) =
         let h1, h2 = split_hyp h in
            merge_SeqHyp "SeqHyp.append" (SeqHyp1.append t1 h1 tt1) (SeqHyp2.append t2 h2 tt2)

      let append_list (t1, t2) (hs : elt list) (tt1, tt2) =
         let hs1, hs2 = split_hyps hs in
            merge_SeqHyp "SeqHyp.append_list" (SeqHyp1.append_list t1 hs1 tt1) (SeqHyp2.append_list t2 hs2 tt2)

      let split (t1, t2) i =
         let l1, h1, r1 = SeqHyp1.split t1 i in
         let l2, h2, r2 = SeqHyp2.split t2 i in
            (merge_SeqHyp "SeqHyp.split - 1" l1 l2),
            (merge_hyp "SeqHyp.split - 2" h1 h2),
            (merge_SeqHyp "SeqHyp.split - 3" r1 r2)

      let init i f =
         merge_SeqHyp "SeqHyp.init" (SeqHyp1.init i (fun i -> fst (split_hyp (f i)))) (SeqHyp2.init i (fun i -> snd (split_hyp (f i))))

      (* XXX: TODO: we do not use the underlying implementation here, so it is not fully tested *)
      let iter f t = List.iter f (to_list t)
      let map f t = of_list (List.map f (to_list t))
      let fold f x (t1, t2) =
         SeqHyp1.fold (fun r i h -> f r i (merge_hyp "SeqHyp.fold" h (SeqHyp2.get t2 i))) x t1
      let lazy_apply = map
      let mapi f t = init (length t) (fun i -> f i (get t i))
      let lazy_sub_map f t i len = of_list (Array.to_list (Lm_array_util.sub_map f (Array.of_list (to_list t)) i len))
      let map_part = function
         ArrayElement a -> ArrayElement a
       | ArrayArray (t, i, j) -> ArrayArray (Array.of_list (to_list t), i, j)
      let collect ps = of_list (Array.to_list (Lm_array_util.collect (List.map map_part ps)))
      let for_all f t = List.for_all f (to_list t)
      let exists f t = List.exists f (to_list t)
   end

   (******************************************************************************************
    * NORMAL MODULES
    *)

   (*
    * Most of the code in the modules below is generated by the util/gen_refiner_debug.pl script.
    * The shell command line needed to regenerate the code is given in front of each
    * generated section.
    *)

   module Term = struct
      module TermTypes = TermType
      module SeqHyp = SeqHyp

      let debug_print = print_term
      let print_term = print_term
      let print_term_list = print_term_list

      let install_debug_printer f =
         let print_term1 out t = f out (term_of_term1 t) in
         let print_term2 out t = f out (term_of_term2 t) in
         let print_both_terms out (t1, t2) =
            fprintf out "Implementation 1: %a\nImplementation 2: %a%t" print_term1 t1 print_term2 t2 eflush
         in
            Term1.install_debug_printer print_term1;
            Term2.install_debug_printer print_term2;
            print_term_ref := print_both_terms

      (*
       * No descriptors here.
       *)
      let dest_descriptor _ =
         None

      let mk_descriptor_term d =
         Weak_memo.TheWeakMemo.retrieve_hack d

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/term_base_sig.ml | grep -v print | util/gen_refiner_debug.pl Term > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let mk_term (p0 : operator) (p1 : bound_term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split p1 in
         merge merge_term "Term.mk_term" (wrap2 Term1.mk_term p0_1 p1_1) (wrap2 Term2.mk_term p0_2 p1_2)

      let make_term (p0 : term') =
         let p0_1, p0_2 = split_term' p0 in
         merge merge_term "Term.make_term" (wrap1 Term1.make_term p0_1) (wrap1 Term2.make_term p0_2)

      let dest_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term' "Term.dest_term" (wrap1 Term1.dest_term p0_1) (wrap1 Term2.dest_term p0_2)

      let mk_op (p0 : opname) (p1 : param list) =
         let p1_1, p1_2 = split p1 in
         merge merge_op "Term.mk_op" (wrap2 Term1.mk_op p0 p1_1) (wrap2 Term2.mk_op p0 p1_2)

      let make_op (p0 : operator') =
         let p0_1, p0_2 = split_op' p0 in
         merge merge_op "Term.make_op" (wrap1 Term1.make_op p0_1) (wrap1 Term2.make_op p0_2)

      let dest_op (p0 : operator) =
         let p0_1, p0_2 = p0 in
         merge merge_op' "Term.dest_op" (wrap1 Term1.dest_op p0_1) (wrap1 Term2.dest_op p0_2)

      let ops_eq (p0 : operator) (p1 : operator) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "Term.ops_eq" (wrap2 Term1.ops_eq p0_1 p1_1) (wrap2 Term2.ops_eq p0_2 p1_2)

      let mk_bterm (p0 : var list) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bterm "Term.mk_bterm" (wrap2 Term1.mk_bterm p0 p1_1) (wrap2 Term2.mk_bterm p0 p1_2)

      let make_bterm (p0 : bound_term') =
         let p0_1, p0_2 = split_bterm' p0 in
         merge merge_bterm "Term.make_bterm" (wrap1 Term1.make_bterm p0_1) (wrap1 Term2.make_bterm p0_2)

      let dest_bterm (p0 : bound_term) =
         let p0_1, p0_2 = p0 in
         merge merge_bterm' "Term.dest_bterm" (wrap1 Term1.dest_bterm p0_1) (wrap1 Term2.dest_bterm p0_2)

      let make_param (p0 : param') =
         let p0_1, p0_2 = split_param' p0 in
         merge merge_param "Term.make_param" (wrap1 Term1.make_param p0_1) (wrap1 Term2.make_param p0_2)

      let dest_param (p0 : param) =
         let p0_1, p0_2 = p0 in
         merge merge_param' "Term.dest_param" (wrap1 Term1.dest_param p0_1) (wrap1 Term2.dest_param p0_2)

      let dest_params (p0 : param list) =
         let p0_1, p0_2 = split p0 in
         merge merge_params' "Term.dest_params" (wrap1 Term1.dest_params p0_1) (wrap1 Term2.dest_params p0_2)

      let mk_level (p0 : int) (p1 : level_exp_var list) =
         let p1_1, p1_2 = split p1 in
         merge merge_level_exp "Term.mk_level" (wrap2 Term1.mk_level p0 p1_1) (wrap2 Term2.mk_level p0 p1_2)

      let make_level (p0 : level_exp') =
         let p0_1, p0_2 = split_level_exp' p0 in
         merge merge_level_exp "Term.make_level" (wrap1 Term1.make_level p0_1) (wrap1 Term2.make_level p0_2)

      let dest_level (p0 : level_exp) =
         let p0_1, p0_2 = p0 in
         merge merge_level_exp' "Term.dest_level" (wrap1 Term1.dest_level p0_1) (wrap1 Term2.dest_level p0_2)

      let mk_level_var (p0 : var) (p1 : int) =
         merge merge_level_exp_var "Term.mk_level_var" (wrap2 Term1.mk_level_var p0 p1) (wrap2 Term2.mk_level_var p0 p1)

      let make_level_var (p0 : level_exp_var') =
         merge merge_level_exp_var "Term.make_level_var" (wrap1 Term1.make_level_var p0) (wrap1 Term2.make_level_var p0)

      let dest_level_var (p0 : level_exp_var) =
         let p0_1, p0_2 = p0 in
         merge merge_level_exp_var' "Term.dest_level_var" (wrap1 Term1.dest_level_var p0_1) (wrap1 Term2.dest_level_var p0_2)

      let make_object_id (p0 : param list) =
         let p0_1, p0_2 = split p0 in
         merge merge_params "Term.make_object_id" (wrap1 Term1.make_object_id p0_1) (wrap1 Term2.make_object_id p0_2)

      let dest_object_id (p0 : object_id) =
         let p0_1, p0_2 = split p0 in
         merge merge_params "Term.dest_object_id" (wrap1 Term1.dest_object_id p0_1) (wrap1 Term2.dest_object_id p0_2)

      let opname_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_opname "Term.opname_of_term" (wrap1 Term1.opname_of_term p0_1) (wrap1 Term2.opname_of_term p0_2)

      let subterms_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_terms "Term.subterms_of_term" (wrap1 Term1.subterms_of_term p0_1) (wrap1 Term2.subterms_of_term p0_2)

      let subterm_arities (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_ints "Term.subterm_arities" (wrap1 Term1.subterm_arities p0_1) (wrap1 Term2.subterm_arities p0_2)

      let is_var_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "Term.is_var_term" (wrap1 Term1.is_var_term p0_1) (wrap1 Term2.is_var_term p0_2)

      let dest_var (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_var "Term.dest_var" (wrap1 Term1.dest_var p0_1) (wrap1 Term2.dest_var p0_2)

      let mk_var_term (p0 : var) =
         merge merge_term "Term.mk_var_term" (wrap1 Term1.mk_var_term p0) (wrap1 Term2.mk_var_term p0)

      let mk_any_term (p0 : operator) (p1 : term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split p1 in
         merge merge_term "Term.mk_any_term" (wrap2 Term1.mk_any_term p0_1 p1_1) (wrap2 Term2.mk_any_term p0_2 p1_2)

      let mk_simple_term (p0 : opname) (p1 : term list) =
         let p1_1, p1_2 = split p1 in
         merge merge_term "Term.mk_simple_term" (wrap2 Term1.mk_simple_term p0 p1_1) (wrap2 Term2.mk_simple_term p0 p1_2)

      let dest_simple_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 Term1.dest_simple_term p0_1 in
         let res2 = wrap1 Term2.dest_simple_term p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "Term.dest_simple_term" res1 res2 in
         (merge_opname "Term.dest_simple_term - 0" res0_1 res0_2),
         (merge_terms "Term.dest_simple_term - 1" res1_1 res1_2)

      let is_simple_term_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "Term.is_simple_term_opname" (wrap2 Term1.is_simple_term_opname p0 p1_1) (wrap2 Term2.is_simple_term_opname p0 p1_2)

      let dest_simple_term_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_terms "Term.dest_simple_term_opname" (wrap2 Term1.dest_simple_term_opname p0 p1_1) (wrap2 Term2.dest_simple_term_opname p0 p1_2)

      let is_simple_bterm (p0 : bound_term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "Term.is_simple_bterm" (wrap1 Term1.is_simple_bterm p0_1) (wrap1 Term2.is_simple_bterm p0_2)

      let mk_simple_bterm (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bterm "Term.mk_simple_bterm" (wrap1 Term1.mk_simple_bterm p0_1) (wrap1 Term2.mk_simple_bterm p0_2)

      let dest_simple_bterm (p0 : bound_term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "Term.dest_simple_bterm" (wrap1 Term1.dest_simple_bterm p0_1) (wrap1 Term2.dest_simple_bterm p0_2)

   end

   module TermOp = struct
      module OpTypes = TermType

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/term_op_sig.ml | util/gen_refiner_debug.pl TermOp > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let iter_down (p0 : (term -> unit)) (p1 : term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = p1 in
         merge merge_unit "TermOp.iter_down" (wrap2 TermOp1.iter_down p0_1 p1_1) (wrap2 TermOp2.iter_down p0_2 p1_2)

      let iter_up (p0 : (term -> unit)) (p1 : term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = p1 in
         merge merge_unit "TermOp.iter_up" (wrap2 TermOp1.iter_up p0_1 p1_1) (wrap2 TermOp2.iter_up p0_2 p1_2)

      let map_down (p0 : (term -> term)) (p1 : term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermOp.map_down" (wrap2 TermOp1.map_down p0_1 p1_1) (wrap2 TermOp2.map_down p0_2 p1_2)

      let map_up (p0 : (term -> term)) (p1 : term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermOp.map_up" (wrap2 TermOp1.map_up p0_1 p1_1) (wrap2 TermOp2.map_up p0_2 p1_2)

      let is_quoted_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermOp.is_quoted_term" (wrap1 TermOp1.is_quoted_term p0_1) (wrap1 TermOp2.is_quoted_term p0_2)

      let quote_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermOp.quote_term" (wrap1 TermOp1.quote_term p0_1) (wrap1 TermOp2.quote_term p0_2)

      let unquote_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermOp.unquote_term" (wrap1 TermOp1.unquote_term p0_1) (wrap1 TermOp2.unquote_term p0_2)

      let is_no_subterms_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_no_subterms_term" (wrap2 TermOp1.is_no_subterms_term p0 p1_1) (wrap2 TermOp2.is_no_subterms_term p0 p1_2)

      let is_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_term" (wrap2 TermOp1.is_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep0_term p0 p1_2)

      let mk_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_term "TermOp.mk_dep0_term" (wrap2 TermOp1.mk_dep0_term p0 p1_1) (wrap2 TermOp2.mk_dep0_term p0 p1_2)

      let dest_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_term "TermOp.dest_dep0_term" (wrap2 TermOp1.dest_dep0_term p0 p1_1) (wrap2 TermOp2.dest_dep0_term p0 p1_2)

      let one_subterm (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermOp.one_subterm" (wrap1 TermOp1.one_subterm p0_1) (wrap1 TermOp2.one_subterm p0_2)

      let one_subterm_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_term "TermOp.one_subterm_opname" (wrap2 TermOp1.one_subterm_opname p0 p1_1) (wrap2 TermOp2.one_subterm_opname p0 p1_2)

      let is_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep0_term" (wrap2 TermOp1.is_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep0_term p0 p1_2)

      let mk_dep0_dep0_term (p0 : opname) (p1 : term) (p2 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge merge_term "TermOp.mk_dep0_dep0_term" (wrap3 TermOp1.mk_dep0_dep0_term p0 p1_1 p2_1) (wrap3 TermOp2.mk_dep0_dep0_term p0 p1_2 p2_2)

      let dest_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_dep0_dep0_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_term - 1" res1_1 res1_2)

      let two_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.two_subterms p0_1 in
         let res2 = wrap1 TermOp2.two_subterms p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.two_subterms" res1 res2 in
         (merge_term "TermOp.two_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.two_subterms - 1" res1_1 res1_2)

      let two_subterms_opname (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.two_subterms_opname p0 p1_1 in
         let res2 = wrap2 TermOp2.two_subterms_opname p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.two_subterms_opname" res1 res2 in
         (merge_term "TermOp.two_subterms_opname - 0" res0_1 res0_2),
         (merge_term "TermOp.two_subterms_opname - 1" res1_1 res1_2)

      let is_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep0_dep0_term" (wrap2 TermOp1.is_dep0_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep0_dep0_term p0 p1_2)

      let mk_dep0_dep0_dep0_term (p0 : opname) (p1 : term) (p2 : term) (p3 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_dep0_dep0_dep0_term" (wrap4 TermOp1.mk_dep0_dep0_dep0_term p0 p1_1 p2_1 p3_1) (wrap4 TermOp2.mk_dep0_dep0_dep0_term p0 p1_2 p2_2 p3_2)

      let dest_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_dep0_dep0_dep0_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_term - 2" res2_1 res2_2)

      let is_dep0_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep0_dep0_dep0_term" (wrap2 TermOp1.is_dep0_dep0_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep0_dep0_dep0_term p0 p1_2)

      let mk_dep0_dep0_dep0_dep0_term (p0 : opname) (p1 : term) (p2 : term) (p3 : term) (p4 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_dep0_dep0_dep0_dep0_term" (wrap5 TermOp1.mk_dep0_dep0_dep0_dep0_term p0 p1_1 p2_1 p3_1 p4_1) (wrap5 TermOp2.mk_dep0_dep0_dep0_dep0_term p0 p1_2 p2_2 p3_2 p4_2)

      let dest_dep0_dep0_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep0_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep0_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_dep0_dep0_dep0_dep0_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep0_dep0_dep0_term - 3" res3_1 res3_2)

      let is_two_subterm (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_two_subterm" (wrap2 TermOp1.is_two_subterm p0 p1_1) (wrap2 TermOp2.is_two_subterm p0 p1_2)

      let is_three_subterm (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_three_subterm" (wrap2 TermOp1.is_three_subterm p0 p1_1) (wrap2 TermOp2.is_three_subterm p0 p1_2)

      let is_five_subterm (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_five_subterm" (wrap2 TermOp1.is_five_subterm p0 p1_1) (wrap2 TermOp2.is_five_subterm p0 p1_2)

      let three_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.three_subterms p0_1 in
         let res2 = wrap1 TermOp2.three_subterms p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.three_subterms" res1 res2 in
         (merge_term "TermOp.three_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.three_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.three_subterms - 2" res2_1 res2_2)

      let four_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.four_subterms p0_1 in
         let res2 = wrap1 TermOp2.four_subterms p0_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.four_subterms" res1 res2 in
         (merge_term "TermOp.four_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.four_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.four_subterms - 2" res2_1 res2_2),
         (merge_term "TermOp.four_subterms - 3" res3_1 res3_2)

      let five_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.five_subterms p0_1 in
         let res2 = wrap1 TermOp2.five_subterms p0_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1), (res0_2, res1_2, res2_2, res3_2, res4_2) = merge merge_triv "TermOp.five_subterms" res1 res2 in
         (merge_term "TermOp.five_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.five_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.five_subterms - 2" res2_1 res2_2),
         (merge_term "TermOp.five_subterms - 3" res3_1 res3_2),
         (merge_term "TermOp.five_subterms - 4" res4_1 res4_2)

      let six_subterms (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.six_subterms p0_1 in
         let res2 = wrap1 TermOp2.six_subterms p0_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1, res5_1), (res0_2, res1_2, res2_2, res3_2, res4_2, res5_2) = merge merge_triv "TermOp.six_subterms" res1 res2 in
         (merge_term "TermOp.six_subterms - 0" res0_1 res0_2),
         (merge_term "TermOp.six_subterms - 1" res1_1 res1_2),
         (merge_term "TermOp.six_subterms - 2" res2_1 res2_2),
         (merge_term "TermOp.six_subterms - 3" res3_1 res3_2),
         (merge_term "TermOp.six_subterms - 4" res4_1 res4_2),
         (merge_term "TermOp.six_subterms - 5" res5_1 res5_2)

      let is_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep1_term" (wrap2 TermOp1.is_dep1_term p0 p1_1) (wrap2 TermOp2.is_dep1_term p0 p1_2)

      let mk_dep1_term (p0 : opname) (p1 : var) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge merge_term "TermOp.mk_dep1_term" (wrap3 TermOp1.mk_dep1_term p0 p1 p2_1) (wrap3 TermOp2.mk_dep1_term p0 p1 p2_2)

      let dest_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep1_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep1_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_dep1_term" res1 res2 in
         (merge_var "TermOp.dest_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_term - 1" res1_1 res1_2)

      let dest_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_dep1_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_dep1_any_term p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_dep1_any_term" res1 res2 in
         (merge_var "TermOp.dest_dep1_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_any_term - 1" res1_1 res1_2)

      let is_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep2_term" (wrap2 TermOp1.is_dep2_term p0 p1_1) (wrap2 TermOp2.is_dep2_term p0 p1_2)

      let mk_dep2_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_dep2_term" (wrap4 TermOp1.mk_dep2_term p0 p1 p2 p3_1) (wrap4 TermOp2.mk_dep2_term p0 p1 p2 p3_2)

      let dest_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep2_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep2_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_dep2_term" res1 res2 in
         (merge_var "TermOp.dest_dep2_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep2_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep2_term - 2" res2_1 res2_2)

      let is_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep1_dep1_term" (wrap2 TermOp1.is_dep1_dep1_term p0 p1_1) (wrap2 TermOp2.is_dep1_dep1_term p0 p1_2)

      let mk_dep1_dep1_term (p0 : opname) (p1 : var) (p2 : term) (p3 : var) (p4 : term) =
         let p2_1, p2_2 = p2 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_dep1_dep1_term" (wrap5 TermOp1.mk_dep1_dep1_term p0 p1 p2_1 p3 p4_1) (wrap5 TermOp2.mk_dep1_dep1_term p0 p1 p2_2 p3 p4_2)

      let dest_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep1_dep1_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep1_dep1_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_dep1_dep1_term" res1 res2 in
         (merge_var "TermOp.dest_dep1_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_dep1_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep1_dep1_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep1_dep1_term - 3" res3_1 res3_2)

      let is_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep1_term" (wrap2 TermOp1.is_dep0_dep1_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep1_term p0 p1_2)

      let is_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermOp.is_dep0_dep1_any_term" (wrap1 TermOp1.is_dep0_dep1_any_term p0_1) (wrap1 TermOp2.is_dep0_dep1_any_term p0_2)

      let mk_dep0_dep1_term (p0 : opname) (p1 : var) (p2 : term) (p3 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_dep0_dep1_term" (wrap4 TermOp1.mk_dep0_dep1_term p0 p1 p2_1 p3_1) (wrap4 TermOp2.mk_dep0_dep1_term p0 p1 p2_2 p3_2)

      let mk_dep0_dep1_any_term (p0 : operator) (p1 : var) (p2 : term) (p3 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_dep0_dep1_any_term" (wrap4 TermOp1.mk_dep0_dep1_any_term p0_1 p1 p2_1 p3_1) (wrap4 TermOp2.mk_dep0_dep1_any_term p0_2 p1 p2_2 p3_2)

      let dest_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep1_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep1_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_dep0_dep1_term" res1 res2 in
         (merge_var "TermOp.dest_dep0_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep1_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep1_term - 2" res2_1 res2_2)

      let dest_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_dep0_dep1_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_dep0_dep1_any_term p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_dep0_dep1_any_term" res1 res2 in
         (merge_var "TermOp.dest_dep0_dep1_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep1_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep1_any_term - 2" res2_1 res2_2)

      let is_dep1_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep1_dep0_term" (wrap2 TermOp1.is_dep1_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep1_dep0_term p0 p1_2)

      let mk_dep1_dep0_term (p0 : opname) (p1 : var) (p2 : term) (p3 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_dep1_dep0_term" (wrap4 TermOp1.mk_dep1_dep0_term p0 p1 p2_1 p3_1) (wrap4 TermOp2.mk_dep1_dep0_term p0 p1 p2_2 p3_2)

      let dest_dep1_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep1_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep1_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_dep1_dep0_term" res1 res2 in
         (merge_var "TermOp.dest_dep1_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep1_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep1_dep0_term - 2" res2_1 res2_2)

      let is_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep2_term" (wrap2 TermOp1.is_dep0_dep2_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep2_term p0 p1_2)

      let mk_dep0_dep2_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) (p4 : term) =
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_dep0_dep2_term" (wrap5 TermOp1.mk_dep0_dep2_term p0 p1 p2 p3_1 p4_1) (wrap5 TermOp2.mk_dep0_dep2_term p0 p1 p2 p3_2 p4_2)

      let dest_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep2_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep2_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_dep0_dep2_term" res1 res2 in
         (merge_var "TermOp.dest_dep0_dep2_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep2_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep2_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep2_term - 3" res3_1 res3_2)

      let is_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep3_term" (wrap2 TermOp1.is_dep0_dep3_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep3_term p0 p1_2)

      let mk_dep0_dep3_term (p0 : opname) (p1 : var) (p2 : var) (p3 : var) (p4 : term) (p5 : term) =
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         merge merge_term "TermOp.mk_dep0_dep3_term" (wrap6 TermOp1.mk_dep0_dep3_term p0 p1 p2 p3 p4_1 p5_1) (wrap6 TermOp2.mk_dep0_dep3_term p0 p1 p2 p3 p4_2 p5_2)

      let dest_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep3_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep3_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1), (res0_2, res1_2, res2_2, res3_2, res4_2) = merge merge_triv "TermOp.dest_dep0_dep3_term" res1 res2 in
         (merge_var "TermOp.dest_dep0_dep3_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep3_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep3_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep3_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep3_term - 4" res4_1 res4_2)

      let is_dep2_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep2_dep0_term" (wrap2 TermOp1.is_dep2_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep2_dep0_term p0 p1_2)

      let mk_dep2_dep0_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) (p4 : term) =
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_dep2_dep0_term" (wrap5 TermOp1.mk_dep2_dep0_term p0 p1 p2 p3_1 p4_1) (wrap5 TermOp2.mk_dep2_dep0_term p0 p1 p2 p3_2 p4_2)

      let dest_dep2_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep2_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep2_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_dep2_dep0_term" res1 res2 in
         (merge_var "TermOp.dest_dep2_dep0_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep2_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep2_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep2_dep0_term - 3" res3_1 res3_2)

      let is_dep0_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep0_dep1_term" (wrap2 TermOp1.is_dep0_dep0_dep1_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep0_dep1_term p0 p1_2)

      let mk_dep0_dep0_dep1_term (p0 : opname) (p1 : term) (p2 : term) (p3 : var) (p4 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_dep0_dep0_dep1_term" (wrap5 TermOp1.mk_dep0_dep0_dep1_term p0 p1_1 p2_1 p3 p4_1) (wrap5 TermOp2.mk_dep0_dep0_dep1_term p0 p1_2 p2_2 p3 p4_2)

      let dest_dep0_dep0_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep0_dep1_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep0_dep1_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_dep0_dep0_dep1_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_dep1_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep1_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_term - 3" res3_1 res3_2)

      let is_dep0_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep0_dep2_term" (wrap2 TermOp1.is_dep0_dep0_dep2_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep0_dep2_term p0 p1_2)

      let mk_dep0_dep0_dep2_term (p0 : opname) (p1 : term) (p2 : term) (p3 : var) (p4 : var) (p5 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p5_1, p5_2 = p5 in
         merge merge_term "TermOp.mk_dep0_dep0_dep2_term" (wrap6 TermOp1.mk_dep0_dep0_dep2_term p0 p1_1 p2_1 p3 p4 p5_1) (wrap6 TermOp2.mk_dep0_dep0_dep2_term p0 p1_2 p2_2 p3 p4 p5_2)

      let dest_dep0_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep0_dep2_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep0_dep2_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1), (res0_2, res1_2, res2_2, res3_2, res4_2) = merge merge_triv "TermOp.dest_dep0_dep0_dep2_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_dep2_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep2_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep2_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep0_dep0_dep2_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep0_dep2_term - 4" res4_1 res4_2)

      let is_dep0_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermOp.is_dep0_dep0_dep1_any_term" (wrap1 TermOp1.is_dep0_dep0_dep1_any_term p0_1) (wrap1 TermOp2.is_dep0_dep0_dep1_any_term p0_2)

      let mk_dep0_dep0_dep1_any_term (p0 : operator) (p1 : term) (p2 : term) (p3 : var) (p4 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_dep0_dep0_dep1_any_term" (wrap5 TermOp1.mk_dep0_dep0_dep1_any_term p0_1 p1_1 p2_1 p3 p4_1) (wrap5 TermOp2.mk_dep0_dep0_dep1_any_term p0_2 p1_2 p2_2 p3 p4_2)

      let dest_dep0_dep0_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_dep0_dep0_dep1_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_dep0_dep0_dep1_any_term p0_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_dep0_dep0_dep1_any_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_dep1_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_any_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep1_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep0_dep1_any_term - 3" res3_1 res3_2)

      let is_dep0_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep1_dep1_term" (wrap2 TermOp1.is_dep0_dep1_dep1_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep1_dep1_term p0 p1_2)

      let mk_dep0_dep1_dep1_term (p0 : opname) (p1 : term) (p2 : var) (p3 : term) (p4 : var) (p5 : term) =
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         let p5_1, p5_2 = p5 in
         merge merge_term "TermOp.mk_dep0_dep1_dep1_term" (wrap6 TermOp1.mk_dep0_dep1_dep1_term p0 p1_1 p2 p3_1 p4 p5_1) (wrap6 TermOp2.mk_dep0_dep1_dep1_term p0 p1_2 p2 p3_2 p4 p5_2)

      let dest_dep0_dep1_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep1_dep1_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep1_dep1_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1), (res0_2, res1_2, res2_2, res3_2, res4_2) = merge merge_triv "TermOp.dest_dep0_dep1_dep1_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep1_dep1_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep1_dep1_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep0_dep1_dep1_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep0_dep1_dep1_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep1_dep1_term - 4" res4_1 res4_2)

      let is_dep0_dep2_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep2_dep0_dep2_term" (wrap2 TermOp1.is_dep0_dep2_dep0_dep2_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep2_dep0_dep2_term p0 p1_2)

      let mk_dep0_dep2_dep0_dep2_term (p0 : opname) (p1 : term) (p2 : var) (p3 : var) (p4 : term) (p5 : term) (p6 : var) (p7 : var) (p8 : term) =
         let p1_1, p1_2 = p1 in
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         let p8_1, p8_2 = p8 in
         merge merge_term "TermOp.mk_dep0_dep2_dep0_dep2_term" (wrap9 TermOp1.mk_dep0_dep2_dep0_dep2_term p0 p1_1 p2 p3 p4_1 p5_1 p6 p7 p8_1) (wrap9 TermOp2.mk_dep0_dep2_dep0_dep2_term p0 p1_2 p2 p3 p4_2 p5_2 p6 p7 p8_2)

      let dest_dep0_dep2_dep0_dep2_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep2_dep0_dep2_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep2_dep0_dep2_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1, res5_1, res6_1, res7_1), (res0_2, res1_2, res2_2, res3_2, res4_2, res5_2, res6_2, res7_2) = merge merge_triv "TermOp.dest_dep0_dep2_dep0_dep2_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 4" res4_1 res4_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 5" res5_1 res5_2),
         (merge_var "TermOp.dest_dep0_dep2_dep0_dep2_term - 6" res6_1 res6_2),
         (merge_term "TermOp.dest_dep0_dep2_dep0_dep2_term - 7" res7_1 res7_2)

      let is_dep0_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep0_dep0_dep3_term" (wrap2 TermOp1.is_dep0_dep0_dep3_term p0 p1_1) (wrap2 TermOp2.is_dep0_dep0_dep3_term p0 p1_2)

      let mk_dep0_dep0_dep3_term (p0 : opname) (p1 : term) (p2 : term) (p3 : var) (p4 : var) (p5 : var) (p6 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let p6_1, p6_2 = p6 in
         merge merge_term "TermOp.mk_dep0_dep0_dep3_term" (wrap7 TermOp1.mk_dep0_dep0_dep3_term p0 p1_1 p2_1 p3 p4 p5 p6_1) (wrap7 TermOp2.mk_dep0_dep0_dep3_term p0 p1_2 p2_2 p3 p4 p5 p6_2)

      let dest_dep0_dep0_dep3_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep0_dep0_dep3_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep0_dep0_dep3_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1, res5_1), (res0_2, res1_2, res2_2, res3_2, res4_2, res5_2) = merge merge_triv "TermOp.dest_dep0_dep0_dep3_term" res1 res2 in
         (merge_term "TermOp.dest_dep0_dep0_dep3_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_dep0_dep0_dep3_term - 1" res1_1 res1_2),
         (merge_var "TermOp.dest_dep0_dep0_dep3_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep0_dep0_dep3_term - 3" res3_1 res3_2),
         (merge_var "TermOp.dest_dep0_dep0_dep3_term - 4" res4_1 res4_2),
         (merge_term "TermOp.dest_dep0_dep0_dep3_term - 5" res5_1 res5_2)

      let is_dep2_dep2_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_dep2_dep2_dep0_dep0_term" (wrap2 TermOp1.is_dep2_dep2_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_dep2_dep2_dep0_dep0_term p0 p1_2)

      let mk_dep2_dep2_dep0_dep0_term (p0 : opname) (p1 : var) (p2 : var) (p3 : term) (p4 : var) (p5 : var) (p6 : term) (p7 : term) (p8 : term) =
         let p3_1, p3_2 = p3 in
         let p6_1, p6_2 = p6 in
         let p7_1, p7_2 = p7 in
         let p8_1, p8_2 = p8 in
         merge merge_term "TermOp.mk_dep2_dep2_dep0_dep0_term" (wrap9 TermOp1.mk_dep2_dep2_dep0_dep0_term p0 p1 p2 p3_1 p4 p5 p6_1 p7_1 p8_1) (wrap9 TermOp2.mk_dep2_dep2_dep0_dep0_term p0 p1 p2 p3_2 p4 p5 p6_2 p7_2 p8_2)

      let dest_dep2_dep2_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_dep2_dep2_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_dep2_dep2_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1, res5_1, res6_1, res7_1), (res0_2, res1_2, res2_2, res3_2, res4_2, res5_2, res6_2, res7_2) = merge merge_triv "TermOp.dest_dep2_dep2_dep0_dep0_term" res1 res2 in
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 3" res3_1 res3_2),
         (merge_var "TermOp.dest_dep2_dep2_dep0_dep0_term - 4" res4_1 res4_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 5" res5_1 res5_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 6" res6_1 res6_2),
         (merge_term "TermOp.dest_dep2_dep2_dep0_dep0_term - 7" res7_1 res7_2)

      let is_string_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_string_term" (wrap2 TermOp1.is_string_term p0 p1_1) (wrap2 TermOp2.is_string_term p0 p1_2)

      let mk_string_term (p0 : opname) (p1 : string) =
         merge merge_term "TermOp.mk_string_term" (wrap2 TermOp1.mk_string_term p0 p1) (wrap2 TermOp2.mk_string_term p0 p1)

      let dest_string_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_string "TermOp.dest_string_term" (wrap2 TermOp1.dest_string_term p0 p1_1) (wrap2 TermOp2.dest_string_term p0 p1_2)

      let dest_string_param (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_string "TermOp.dest_string_param" (wrap1 TermOp1.dest_string_param p0_1) (wrap1 TermOp2.dest_string_param p0_2)

      let is_string_string_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_string_string_term" (wrap2 TermOp1.is_string_string_term p0 p1_1) (wrap2 TermOp2.is_string_string_term p0 p1_2)

      let mk_string_string_term (p0 : opname) (p1 : string) (p2 : string) =
         merge merge_term "TermOp.mk_string_string_term" (wrap3 TermOp1.mk_string_string_term p0 p1 p2) (wrap3 TermOp2.mk_string_string_term p0 p1 p2)

      let dest_string_string_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_string_string_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_string_string_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_string_string_term" res1 res2 in
         (merge_string "TermOp.dest_string_string_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_term - 1" res1_1 res1_2)

      let is_var_param_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_var_param_term" (wrap2 TermOp1.is_var_param_term p0 p1_1) (wrap2 TermOp2.is_var_param_term p0 p1_2)

      let mk_var_param_term (p0 : opname) (p1 : var) =
         merge merge_term "TermOp.mk_var_param_term" (wrap2 TermOp1.mk_var_param_term p0 p1) (wrap2 TermOp2.mk_var_param_term p0 p1)

      let dest_var_param_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_var "TermOp.dest_var_param_term" (wrap2 TermOp1.dest_var_param_term p0 p1_1) (wrap2 TermOp2.dest_var_param_term p0 p1_2)

      let is_var_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_var_dep0_term" (wrap2 TermOp1.is_var_dep0_term p0 p1_1) (wrap2 TermOp2.is_var_dep0_term p0 p1_2)

      let mk_var_dep0_term (p0 : opname) (p1 : var) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge merge_term "TermOp.mk_var_dep0_term" (wrap3 TermOp1.mk_var_dep0_term p0 p1 p2_1) (wrap3 TermOp2.mk_var_dep0_term p0 p1 p2_2)

      let dest_var_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_var_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_var_dep0_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_var_dep0_term" res1 res2 in
         (merge_var "TermOp.dest_var_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_var_dep0_term - 1" res1_1 res1_2)

      let dest_var_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_var_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_var_dep0_any_term p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_var_dep0_any_term" res1 res2 in
         (merge_var "TermOp.dest_var_dep0_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_var_dep0_any_term - 1" res1_1 res1_2)

      let is_var_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_var_dep0_dep0_term" (wrap2 TermOp1.is_var_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_var_dep0_dep0_term p0 p1_2)

      let mk_var_dep0_dep0_term (p0 : opname) (p1 : var) (p2 : term) (p3 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_var_dep0_dep0_term" (wrap4 TermOp1.mk_var_dep0_dep0_term p0 p1 p2_1 p3_1) (wrap4 TermOp2.mk_var_dep0_dep0_term p0 p1 p2_2 p3_2)

      let dest_var_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_var_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_var_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_var_dep0_dep0_term" res1 res2 in
         (merge_var "TermOp.dest_var_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_var_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_var_dep0_dep0_term - 2" res2_1 res2_2)

      let is_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_string_dep0_term" (wrap2 TermOp1.is_string_dep0_term p0 p1_1) (wrap2 TermOp2.is_string_dep0_term p0 p1_2)

      let mk_string_dep0_term (p0 : opname) (p1 : string) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge merge_term "TermOp.mk_string_dep0_term" (wrap3 TermOp1.mk_string_dep0_term p0 p1 p2_1) (wrap3 TermOp2.mk_string_dep0_term p0 p1 p2_2)

      let dest_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_string_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_string_dep0_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_string_dep0_term" res1 res2 in
         (merge_string "TermOp.dest_string_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_string_dep0_term - 1" res1_1 res1_2)

      let is_string_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_string_string_dep0_term" (wrap2 TermOp1.is_string_string_dep0_term p0 p1_1) (wrap2 TermOp2.is_string_string_dep0_term p0 p1_2)

      let mk_string_string_dep0_term (p0 : opname) (p1 : string) (p2 : string) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_string_string_dep0_term" (wrap4 TermOp1.mk_string_string_dep0_term p0 p1 p2 p3_1) (wrap4 TermOp2.mk_string_string_dep0_term p0 p1 p2 p3_2)

      let dest_string_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_string_string_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_string_string_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_string_string_dep0_term" res1 res2 in
         (merge_string "TermOp.dest_string_string_dep0_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_term - 2" res2_1 res2_2)

      let dest_string_string_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_string_string_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_string_string_dep0_any_term p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_string_string_dep0_any_term" res1 res2 in
         (merge_string "TermOp.dest_string_string_dep0_any_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_any_term - 2" res2_1 res2_2)

      let is_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_number_dep0_term" (wrap2 TermOp1.is_number_dep0_term p0 p1_1) (wrap2 TermOp2.is_number_dep0_term p0 p1_2)

      let mk_number_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge merge_term "TermOp.mk_number_dep0_term" (wrap3 TermOp1.mk_number_dep0_term p0 p1 p2_1) (wrap3 TermOp2.mk_number_dep0_term p0 p1 p2_2)

      let dest_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_number_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_number_dep0_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_number_dep0_term" res1 res2 in
         (merge_num "TermOp.dest_number_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_number_dep0_term - 1" res1_1 res1_2)

      let dest_number_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_number_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_number_dep0_any_term p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_number_dep0_any_term" res1 res2 in
         (merge_num "TermOp.dest_number_dep0_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_number_dep0_any_term - 1" res1_1 res1_2)

      let is_number_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_number_dep1_term" (wrap2 TermOp1.is_number_dep1_term p0 p1_1) (wrap2 TermOp2.is_number_dep1_term p0 p1_2)

      let mk_number_dep1_term (p0 : opname) (p1 : Lm_num.num) (p2 : var) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_number_dep1_term" (wrap4 TermOp1.mk_number_dep1_term p0 p1 p2 p3_1) (wrap4 TermOp2.mk_number_dep1_term p0 p1 p2 p3_2)

      let dest_number_dep1_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_number_dep1_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_number_dep1_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_number_dep1_term" res1 res2 in
         (merge_num "TermOp.dest_number_dep1_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_number_dep1_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_dep1_term - 2" res2_1 res2_2)

      let dest_number_dep1_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_number_dep1_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_number_dep1_any_term p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_number_dep1_any_term" res1 res2 in
         (merge_num "TermOp.dest_number_dep1_any_term - 0" res0_1 res0_2),
         (merge_var "TermOp.dest_number_dep1_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_dep1_any_term - 2" res2_1 res2_2)

      let is_number_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_number_number_dep0_term" (wrap2 TermOp1.is_number_number_dep0_term p0 p1_1) (wrap2 TermOp2.is_number_number_dep0_term p0 p1_2)

      let mk_number_number_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : Lm_num.num) (p3 : term) =
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_number_number_dep0_term" (wrap4 TermOp1.mk_number_number_dep0_term p0 p1 p2 p3_1) (wrap4 TermOp2.mk_number_number_dep0_term p0 p1 p2 p3_2)

      let dest_number_number_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_number_number_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_number_number_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_number_number_dep0_term" res1 res2 in
         (merge_num "TermOp.dest_number_number_dep0_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_number_dep0_term - 2" res2_1 res2_2)

      let dest_number_number_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_number_number_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_number_number_dep0_any_term p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_number_number_dep0_any_term" res1 res2 in
         (merge_num "TermOp.dest_number_number_dep0_any_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_number_number_dep0_any_term - 2" res2_1 res2_2)

      let is_number_number_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_number_number_string_dep0_term" (wrap2 TermOp1.is_number_number_string_dep0_term p0 p1_1) (wrap2 TermOp2.is_number_number_string_dep0_term p0 p1_2)

      let mk_number_number_string_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : Lm_num.num) (p3 : string) (p4 : term) =
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_number_number_string_dep0_term" (wrap5 TermOp1.mk_number_number_string_dep0_term p0 p1 p2 p3 p4_1) (wrap5 TermOp2.mk_number_number_string_dep0_term p0 p1 p2 p3 p4_2)

      let dest_number_number_string_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_number_number_string_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_number_number_string_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_number_number_string_dep0_term" res1 res2 in
         (merge_num "TermOp.dest_number_number_string_dep0_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_string_dep0_term - 1" res1_1 res1_2),
         (merge_string "TermOp.dest_number_number_string_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_number_number_string_dep0_term - 3" res3_1 res3_2)

      let dest_number_number_string_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_number_number_string_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_number_number_string_dep0_any_term p0_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_number_number_string_dep0_any_term" res1 res2 in
         (merge_num "TermOp.dest_number_number_string_dep0_any_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_string_dep0_any_term - 1" res1_1 res1_2),
         (merge_string "TermOp.dest_number_number_string_dep0_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_number_number_string_dep0_any_term - 3" res3_1 res3_2)

      let is_number_number_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_number_number_string_dep0_dep0_term" (wrap2 TermOp1.is_number_number_string_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_number_number_string_dep0_dep0_term p0 p1_2)

      let mk_number_number_string_dep0_dep0_term (p0 : opname) (p1 : Lm_num.num) (p2 : Lm_num.num) (p3 : string) (p4 : term) (p5 : term) =
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         merge merge_term "TermOp.mk_number_number_string_dep0_dep0_term" (wrap6 TermOp1.mk_number_number_string_dep0_dep0_term p0 p1 p2 p3 p4_1 p5_1) (wrap6 TermOp2.mk_number_number_string_dep0_dep0_term p0 p1 p2 p3 p4_2 p5_2)

      let dest_number_number_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_number_number_string_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_number_number_string_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1), (res0_2, res1_2, res2_2, res3_2, res4_2) = merge merge_triv "TermOp.dest_number_number_string_dep0_dep0_term" res1 res2 in
         (merge_num "TermOp.dest_number_number_string_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_string_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_string "TermOp.dest_number_number_string_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_number_number_string_dep0_dep0_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_number_number_string_dep0_dep0_term - 4" res4_1 res4_2)

      let dest_number_number_string_dep0_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_number_number_string_dep0_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_number_number_string_dep0_dep0_any_term p0_2 in
         let (res0_1, res1_1, res2_1, res3_1, res4_1), (res0_2, res1_2, res2_2, res3_2, res4_2) = merge merge_triv "TermOp.dest_number_number_string_dep0_dep0_any_term" res1 res2 in
         (merge_num "TermOp.dest_number_number_string_dep0_dep0_any_term - 0" res0_1 res0_2),
         (merge_num "TermOp.dest_number_number_string_dep0_dep0_any_term - 1" res1_1 res1_2),
         (merge_string "TermOp.dest_number_number_string_dep0_dep0_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_number_number_string_dep0_dep0_any_term - 3" res3_1 res3_2),
         (merge_term "TermOp.dest_number_number_string_dep0_dep0_any_term - 4" res4_1 res4_2)

      let is_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_string_dep0_dep0_term" (wrap2 TermOp1.is_string_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_string_dep0_dep0_term p0 p1_2)

      let mk_string_dep0_dep0_term (p0 : opname) (p1 : string) (p2 : term) (p3 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermOp.mk_string_dep0_dep0_term" (wrap4 TermOp1.mk_string_dep0_dep0_term p0 p1 p2_1 p3_1) (wrap4 TermOp2.mk_string_dep0_dep0_term p0 p1 p2_2 p3_2)

      let dest_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_string_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_string_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_string_dep0_dep0_term" res1 res2 in
         (merge_string "TermOp.dest_string_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_string_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_dep0_dep0_term - 2" res2_1 res2_2)

      let dest_string_dep0_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_string_dep0_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_string_dep0_dep0_any_term p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermOp.dest_string_dep0_dep0_any_term" res1 res2 in
         (merge_string "TermOp.dest_string_dep0_dep0_any_term - 0" res0_1 res0_2),
         (merge_term "TermOp.dest_string_dep0_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_dep0_dep0_any_term - 2" res2_1 res2_2)

      let mk_string_dep0_dep0_dep0_term (p0 : opname) (p1 : string) (p2 : term) (p3 : term) (p4 : term) =
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_string_dep0_dep0_dep0_term" (wrap5 TermOp1.mk_string_dep0_dep0_dep0_term p0 p1 p2_1 p3_1 p4_1) (wrap5 TermOp2.mk_string_dep0_dep0_dep0_term p0 p1 p2_2 p3_2 p4_2)

      let is_string_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_string_string_dep0_dep0_term" (wrap2 TermOp1.is_string_string_dep0_dep0_term p0 p1_1) (wrap2 TermOp2.is_string_string_dep0_dep0_term p0 p1_2)

      let mk_string_string_dep0_dep0_term (p0 : opname) (p1 : string) (p2 : string) (p3 : term) (p4 : term) =
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_term "TermOp.mk_string_string_dep0_dep0_term" (wrap5 TermOp1.mk_string_string_dep0_dep0_term p0 p1 p2 p3_1 p4_1) (wrap5 TermOp2.mk_string_string_dep0_dep0_term p0 p1 p2 p3_2 p4_2)

      let dest_string_string_dep0_dep0_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_string_string_dep0_dep0_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_string_string_dep0_dep0_term p0 p1_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_string_string_dep0_dep0_term" res1 res2 in
         (merge_string "TermOp.dest_string_string_dep0_dep0_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_dep0_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_term - 3" res3_1 res3_2)

      let dest_string_string_dep0_dep0_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermOp1.dest_string_string_dep0_dep0_any_term p0_1 in
         let res2 = wrap1 TermOp2.dest_string_string_dep0_dep0_any_term p0_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermOp.dest_string_string_dep0_dep0_any_term" res1 res2 in
         (merge_string "TermOp.dest_string_string_dep0_dep0_any_term - 0" res0_1 res0_2),
         (merge_string "TermOp.dest_string_string_dep0_dep0_any_term - 1" res1_1 res1_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_any_term - 2" res2_1 res2_2),
         (merge_term "TermOp.dest_string_string_dep0_dep0_any_term - 3" res3_1 res3_2)

      let is_number_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_number_term" (wrap2 TermOp1.is_number_term p0 p1_1) (wrap2 TermOp2.is_number_term p0 p1_2)

      let mk_number_term (p0 : opname) (p1 : Lm_num.num) =
         merge merge_term "TermOp.mk_number_term" (wrap2 TermOp1.mk_number_term p0 p1) (wrap2 TermOp2.mk_number_term p0 p1)

      let dest_number_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_num "TermOp.dest_number_term" (wrap2 TermOp1.dest_number_term p0 p1_1) (wrap2 TermOp2.dest_number_term p0 p1_2)

      let dest_number_any_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_num "TermOp.dest_number_any_term" (wrap1 TermOp1.dest_number_any_term p0_1) (wrap1 TermOp2.dest_number_any_term p0_2)

      let is_univ_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_univ_term" (wrap2 TermOp1.is_univ_term p0 p1_1) (wrap2 TermOp2.is_univ_term p0 p1_2)

      let mk_univ_term (p0 : opname) (p1 : level_exp) =
         let p1_1, p1_2 = p1 in
         merge merge_term "TermOp.mk_univ_term" (wrap2 TermOp1.mk_univ_term p0 p1_1) (wrap2 TermOp2.mk_univ_term p0 p1_2)

      let dest_univ_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_level_exp "TermOp.dest_univ_term" (wrap2 TermOp1.dest_univ_term p0 p1_1) (wrap2 TermOp2.dest_univ_term p0 p1_2)

      let is_token_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_token_term" (wrap2 TermOp1.is_token_term p0 p1_1) (wrap2 TermOp2.is_token_term p0 p1_2)

      let mk_token_term (p0 : opname) (p1 : opname) =
         merge merge_term "TermOp.mk_token_term" (wrap2 TermOp1.mk_token_term p0 p1) (wrap2 TermOp2.mk_token_term p0 p1)

      let dest_token_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_opname "TermOp.dest_token_term" (wrap2 TermOp1.dest_token_term p0 p1_1) (wrap2 TermOp2.dest_token_term p0 p1_2)

      let dest_token_param (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_opname "TermOp.dest_token_param" (wrap1 TermOp1.dest_token_param p0_1) (wrap1 TermOp2.dest_token_param p0_2)

      let is_token_simple_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermOp.is_token_simple_term" (wrap2 TermOp1.is_token_simple_term p0 p1_1) (wrap2 TermOp2.is_token_simple_term p0 p1_2)

      let mk_token_simple_term (p0 : opname) (p1 : opname) (p2 : term list) =
         let p2_1, p2_2 = split p2 in
         merge merge_term "TermOp.mk_token_simple_term" (wrap3 TermOp1.mk_token_simple_term p0 p1 p2_1) (wrap3 TermOp2.mk_token_simple_term p0 p1 p2_2)

      let dest_token_simple_term (p0 : opname) (p1 : term) =
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermOp1.dest_token_simple_term p0 p1_1 in
         let res2 = wrap2 TermOp2.dest_token_simple_term p0 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermOp.dest_token_simple_term" res1 res2 in
         (merge_opname "TermOp.dest_token_simple_term - 0" res0_1 res0_2),
         (merge_terms "TermOp.dest_token_simple_term - 1" res1_1 res1_2)

   end

   module TermAddr = struct
      module AddrTypes = TermType
      type address = TermType.address

      let string_of_address (a1, a2) =
         sprintf "Impl1 addr: %s; Impl2 addr: %s" (TermAddr1.string_of_address a1) (TermAddr2.string_of_address a2)

      (*
       * To generate the code for this module, run the following:
       *   grep -A1 val refiner/refsig/term_addr_sig.ml | grep -v -- -- | grep -v string_of_address | grep -v end | util/gen_refiner_debug.pl TermAddr > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let null_address =
         merge_address "TermAddr.null_address" (TermAddr1.null_address) (TermAddr2.null_address)

      let make_address (p0 : addr_item list) =
         merge merge_address "TermAddr.make_address" (wrap1 TermAddr1.make_address p0) (wrap1 TermAddr2.make_address p0)

      let dest_address (p0 : address) =
         let p0_1, p0_2 = p0 in
         merge merge_addr_items "TermAddr.dest_address" (wrap1 TermAddr1.dest_address p0_1) (wrap1 TermAddr2.dest_address p0_2)

      let compose_address (p0 : address) (p1 : address) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_address "TermAddr.compose_address" (wrap2 TermAddr1.compose_address p0_1 p1_1) (wrap2 TermAddr2.compose_address p0_2 p1_2)

      let split_clause_address (p0 : address) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermAddr1.split_clause_address p0_1 in
         let res2 = wrap1 TermAddr2.split_clause_address p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermAddr.split_clause_address" res1 res2 in
         (merge_address "TermAddr.split_clause_address - 0" res0_1 res0_2),
         (merge_address "TermAddr.split_clause_address - 1" res1_1 res1_2)

      let subterm_exists (p0 : term) (p1 : address) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermAddr.subterm_exists" (wrap2 TermAddr1.subterm_exists p0_1 p1_1) (wrap2 TermAddr2.subterm_exists p0_2 p1_2)

      let find_subterm (p0 : term) (p1 : (term -> SymbolSet.t -> bool)) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split_taf p1 in
         merge merge_addresss "TermAddr.find_subterm" (wrap2 TermAddr1.find_subterm p0_1 p1_1) (wrap2 TermAddr2.find_subterm p0_2 p1_2)

      let term_subterm (p0 : term) (p1 : address) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermAddr.term_subterm" (wrap2 TermAddr1.term_subterm p0_1 p1_1) (wrap2 TermAddr2.term_subterm p0_2 p1_2)

      let replace_subterm (p0 : term) (p1 : address) (p2 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge merge_term "TermAddr.replace_subterm" (wrap3 TermAddr1.replace_subterm p0_1 p1_1 p2_1) (wrap3 TermAddr2.replace_subterm p0_2 p1_2 p2_2)

      let apply_fun_at_addr (p0 : (term -> term)) (p1 : address) (p2 : term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge merge_term "TermAddr.apply_fun_at_addr" (wrap3 TermAddr1.apply_fun_at_addr p0_1 p1_1 p2_1) (wrap3 TermAddr2.apply_fun_at_addr p0_2 p1_2 p2_2)

      let apply_fun_arg_at_addr (p0 : (term -> term * 'a)) (p1 : address) (p2 : term) =
         let p0_1, p0_2 = split_ttaf p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let res1 = wrap3 TermAddr1.apply_fun_arg_at_addr p0_1 p1_1 p2_1 in
         let res2 = wrap3 TermAddr2.apply_fun_arg_at_addr p0_2 p1_2 p2_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermAddr.apply_fun_arg_at_addr" res1 res2 in
         (merge_term "TermAddr.apply_fun_arg_at_addr - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_fun_arg_at_addr - 1" res1_1 res1_2)

      let apply_var_fun_at_addr (p0 : (SymbolSet.t -> term -> term)) (p1 : address) (p2 : SymbolSet.t) (p3 : term) =
         let p0_1, p0_2 = split_attf p0 in
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         merge merge_term "TermAddr.apply_var_fun_at_addr" (wrap4 TermAddr1.apply_var_fun_at_addr p0_1 p1_1 p2 p3_1) (wrap4 TermAddr2.apply_var_fun_at_addr p0_2 p1_2 p2 p3_2)

      let apply_var_fun_arg_at_addr (p0 : (SymbolSet.t -> term -> term * 'a)) (p1 : address) (p2 : SymbolSet.t) (p3 : term) =
         let p0_1, p0_2 = split_attaf p0 in
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         let res1 = wrap4 TermAddr1.apply_var_fun_arg_at_addr p0_1 p1_1 p2 p3_1 in
         let res2 = wrap4 TermAddr2.apply_var_fun_arg_at_addr p0_2 p1_2 p2 p3_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermAddr.apply_var_fun_arg_at_addr" res1 res2 in
         (merge_term "TermAddr.apply_var_fun_arg_at_addr - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_var_fun_arg_at_addr - 1" res1_1 res1_2)

      let subterm_addresses (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_addresss "TermAddr.subterm_addresses" (wrap1 TermAddr1.subterm_addresses p0_1) (wrap1 TermAddr2.subterm_addresses p0_2)

      let strip_address (p0 : addr_item list) (p1 : address) =
         let p1_1, p1_2 = p1 in
         merge merge_address "TermAddr.strip_address" (wrap2 TermAddr1.strip_address p0 p1_1) (wrap2 TermAddr2.strip_address p0 p1_2)

      let apply_fun_higher (p0 : (term -> term * 'a)) (p1 : term) =
         let p0_1, p0_2 = split_ttaf p0 in
         let p1_1, p1_2 = p1 in
         let res1 = wrap2 TermAddr1.apply_fun_higher p0_1 p1_1 in
         let res2 = wrap2 TermAddr2.apply_fun_higher p0_2 p1_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermAddr.apply_fun_higher" res1 res2 in
         (merge_term "TermAddr.apply_fun_higher - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_fun_higher - 1" res1_1 res1_2)

      let apply_var_fun_higher (p0 : (SymbolSet.t -> term -> term * 'a)) (p1 : SymbolSet.t) (p2 : term) =
         let p0_1, p0_2 = split_attaf p0 in
         let p2_1, p2_2 = p2 in
         let res1 = wrap3 TermAddr1.apply_var_fun_higher p0_1 p1 p2_1 in
         let res2 = wrap3 TermAddr2.apply_var_fun_higher p0_2 p1 p2_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermAddr.apply_var_fun_higher" res1 res2 in
         (merge_term "TermAddr.apply_var_fun_higher - 0" res0_1 res0_2),
         (merge_poly "TermAddr.apply_var_fun_higher - 1" res1_1 res1_2)

   end

   module TermMan = struct
      module ManTypes = TermType

      (*
       * To generate the code for this module, run the following:
       *   grep -A1 val refiner/refsig/term_man_sig.ml | egrep -v 'end|--|analoguos' | sed -e 's|(\*.*||' | util/gen_refiner_debug.pl TermMan > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let mk_const_level_exp (p0 : int) =
         merge merge_level_exp "TermMan.mk_const_level_exp" (wrap1 TermMan1.mk_const_level_exp p0) (wrap1 TermMan2.mk_const_level_exp p0)

      let mk_var_level_exp (p0 : var) =
         merge merge_level_exp "TermMan.mk_var_level_exp" (wrap1 TermMan1.mk_var_level_exp p0) (wrap1 TermMan2.mk_var_level_exp p0)

      let max_level_exp (p0 : level_exp) (p1 : level_exp) (p2 : int) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_level_exp "TermMan.max_level_exp" (wrap3 TermMan1.max_level_exp p0_1 p1_1 p2) (wrap3 TermMan2.max_level_exp p0_2 p1_2 p2)

      let incr_level_exp (p0 : level_exp) =
         let p0_1, p0_2 = p0 in
         merge merge_level_exp "TermMan.incr_level_exp" (wrap1 TermMan1.incr_level_exp p0_1) (wrap1 TermMan2.incr_level_exp p0_2)

      let level_le (p0 : level_exp) (p1 : level_exp) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermMan.level_le" (wrap2 TermMan1.level_le p0_1 p1_1) (wrap2 TermMan2.level_le p0_2 p1_2)

      let level_lt (p0 : level_exp) (p1 : level_exp) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermMan.level_lt" (wrap2 TermMan1.level_lt p0_1 p1_1) (wrap2 TermMan2.level_lt p0_2 p1_2)

      let is_so_var_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_so_var_term" (wrap1 TermMan1.is_so_var_term p0_1) (wrap1 TermMan2.is_so_var_term p0_2)

      let dest_so_var (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermMan1.dest_so_var p0_1 in
         let res2 = wrap1 TermMan2.dest_so_var p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermMan.dest_so_var" res1 res2 in
         (merge_var "TermMan.dest_so_var - 0" res0_1 res0_2),
         (merge_vars "TermMan.dest_so_var - 1" res1_1 res1_2),
         (merge_terms "TermMan.dest_so_var - 2" res2_1 res2_2)

      let mk_so_var_term (p0 : var) (p1 : var list) (p2 : term list) =
         let p2_1, p2_2 = split p2 in
         merge merge_term "TermMan.mk_so_var_term" (wrap3 TermMan1.mk_so_var_term p0 p1 p2_1) (wrap3 TermMan2.mk_so_var_term p0 p1 p2_2)

      let is_fso_var_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_fso_var_term" (wrap1 TermMan1.is_fso_var_term p0_1) (wrap1 TermMan2.is_fso_var_term p0_2)

      let dest_fso_var (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_var "TermMan.dest_fso_var" (wrap1 TermMan1.dest_fso_var p0_1) (wrap1 TermMan2.dest_fso_var p0_2)

      let is_context_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_context_term" (wrap1 TermMan1.is_context_term p0_1) (wrap1 TermMan2.is_context_term p0_2)

      let dest_context (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermMan1.dest_context p0_1 in
         let res2 = wrap1 TermMan2.dest_context p0_2 in
         let (res0_1, res1_1, res2_1, res3_1), (res0_2, res1_2, res2_2, res3_2) = merge merge_triv "TermMan.dest_context" res1 res2 in
         (merge_var "TermMan.dest_context - 0" res0_1 res0_2),
         (merge_term "TermMan.dest_context - 1" res1_1 res1_2),
         (merge_vars "TermMan.dest_context - 2" res2_1 res2_2),
         (merge_terms "TermMan.dest_context - 3" res3_1 res3_2)

      let mk_context_term (p0 : var) (p1 : term) (p2 : var list) (p3 : term list) =
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = split p3 in
         merge merge_term "TermMan.mk_context_term" (wrap4 TermMan1.mk_context_term p0 p1_1 p2 p3_1) (wrap4 TermMan2.mk_context_term p0 p1_2 p2 p3_2)

      let is_fo_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_fo_term" (wrap1 TermMan1.is_fo_term p0_1) (wrap1 TermMan2.is_fo_term p0_2)

      let context_vars_info (p0 : (bool * int * int) SymbolTable.t) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_stables "TermMan.context_vars_info" (wrap2 TermMan1.context_vars_info p0 p1_1) (wrap2 TermMan2.context_vars_info p0 p1_2)

      let so_vars_info (p0 : (int * int) SymbolTable.t) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_stables "TermMan.so_vars_info" (wrap2 TermMan1.so_vars_info p0 p1_1) (wrap2 TermMan2.so_vars_info p0 p1_2)

      let param_vars_info (p0 : SymbolSet.t) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_ss "TermMan.param_vars_info" (wrap2 TermMan1.param_vars_info p0 p1_1) (wrap2 TermMan2.param_vars_info p0 p1_2)

      let context_vars_info_list (p0 : (bool * int * int) SymbolTable.t) (p1 : term list) =
         let p1_1, p1_2 = split p1 in
         merge merge_stables "TermMan.context_vars_info_list" (wrap2 TermMan1.context_vars_info_list p0 p1_1) (wrap2 TermMan2.context_vars_info_list p0 p1_2)

      let so_vars_info_list (p0 : (int * int) SymbolTable.t) (p1 : term list) =
         let p1_1, p1_2 = split p1 in
         merge merge_stables "TermMan.so_vars_info_list" (wrap2 TermMan1.so_vars_info_list p0 p1_1) (wrap2 TermMan2.so_vars_info_list p0 p1_2)

      let param_vars_info_list (p0 : SymbolSet.t) (p1 : term list) =
         let p1_1, p1_2 = split p1 in
         merge merge_ss "TermMan.param_vars_info_list" (wrap2 TermMan1.param_vars_info_list p0 p1_1) (wrap2 TermMan2.param_vars_info_list p0 p1_2)

      let all_contexts (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_ss "TermMan.all_contexts" (wrap1 TermMan1.all_contexts p0_1) (wrap1 TermMan2.all_contexts p0_2)

      let all_meta_variables (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_ss "TermMan.all_meta_variables" (wrap1 TermMan1.all_meta_variables p0_1) (wrap1 TermMan2.all_meta_variables p0_2)

      let context_vars (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermMan1.context_vars p0_1 in
         let res2 = wrap1 TermMan2.context_vars p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMan.context_vars" res1 res2 in
         (merge_ss "TermMan.context_vars - 0" res0_1 res0_2),
         (merge_ss "TermMan.context_vars - 1" res1_1 res1_2)

      let all_vars (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_ss "TermMan.all_vars" (wrap1 TermMan1.all_vars p0_1) (wrap1 TermMan2.all_vars p0_2)

      let all_vars_terms (p0 : term list) =
         let p0_1, p0_2 = split p0 in
         merge merge_ss "TermMan.all_vars_terms" (wrap1 TermMan1.all_vars_terms p0_1) (wrap1 TermMan2.all_vars_terms p0_2)

      let all_vars_info (p0 : var_info SymbolTable.t) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_stables "TermMan.all_vars_info" (wrap2 TermMan1.all_vars_info p0 p1_1) (wrap2 TermMan2.all_vars_info p0 p1_2)

      let explode_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_match_term "TermMan.explode_term" (wrap1 TermMan1.explode_term p0_1) (wrap1 TermMan2.explode_term p0_2)

      let is_sequent_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_sequent_term" (wrap1 TermMan1.is_sequent_term p0_1) (wrap1 TermMan2.is_sequent_term p0_2)

      let mk_sequent_term (p0 : esequent) =
         let p0_1, p0_2 = split_eseq p0 in
         merge merge_term "TermMan.mk_sequent_term" (wrap1 TermMan1.mk_sequent_term p0_1) (wrap1 TermMan2.mk_sequent_term p0_2)

      let explode_sequent (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_esequent "TermMan.explode_sequent" (wrap1 TermMan1.explode_sequent p0_1) (wrap1 TermMan2.explode_sequent p0_2)

      let explode_sequent_and_rename (p0 : term) (p1 : SymbolSet.t) =
         let p0_1, p0_2 = p0 in
         merge merge_esequent "TermMan.explode_sequent_and_rename" (wrap2 TermMan1.explode_sequent_and_rename p0_1 p1) (wrap2 TermMan2.explode_sequent_and_rename p0_2 p1)

      let nth_hyp (p0 : term) (p1 : int) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermMan.nth_hyp" (wrap2 TermMan1.nth_hyp p0_1 p1) (wrap2 TermMan2.nth_hyp p0_2 p1)

      let nth_binding (p0 : term) (p1 : int) =
         let p0_1, p0_2 = p0 in
         merge merge_var "TermMan.nth_binding" (wrap2 TermMan1.nth_binding p0_1 p1) (wrap2 TermMan2.nth_binding p0_2 p1)

      let args (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermMan.args" (wrap1 TermMan1.args p0_1) (wrap1 TermMan2.args p0_2)

      let hyps (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_terms "TermMan.hyps" (wrap1 TermMan1.hyps p0_1) (wrap1 TermMan2.hyps p0_2)

      let concl (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermMan.concl" (wrap1 TermMan1.concl p0_1) (wrap1 TermMan2.concl p0_2)

      let sequent_args = args
      let sequent_hyps = hyps
      let sequent_concl = concl

      let num_hyps (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_int "TermMan.num_hyps" (wrap1 TermMan1.num_hyps p0_1) (wrap1 TermMan2.num_hyps p0_2)

      let declared_vars (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_vars "TermMan.declared_vars" (wrap1 TermMan1.declared_vars p0_1) (wrap1 TermMan2.declared_vars p0_2)

      let get_decl_number (p0 : term) (p1 : var) =
         let p0_1, p0_2 = p0 in
         merge merge_int "TermMan.get_decl_number" (wrap2 TermMan1.get_decl_number p0_1 p1) (wrap2 TermMan2.get_decl_number p0_2 p1)

      let get_hyp_number (p0 : term) (p1 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_int "TermMan.get_hyp_number" (wrap2 TermMan1.get_hyp_number p0_1 p1_1) (wrap2 TermMan2.get_hyp_number p0_2 p1_2)

      let replace_concl (p0 : term) (p1 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMan.replace_concl" (wrap2 TermMan1.replace_concl p0_1 p1_1) (wrap2 TermMan2.replace_concl p0_2 p1_2)

      let is_xrewrite_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xrewrite_term" (wrap1 TermMan1.is_xrewrite_term p0_1) (wrap1 TermMan2.is_xrewrite_term p0_2)

      let mk_xrewrite_term (p0 : term) (p1 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMan.mk_xrewrite_term" (wrap2 TermMan1.mk_xrewrite_term p0_1 p1_1) (wrap2 TermMan2.mk_xrewrite_term p0_2 p1_2)

      let dest_xrewrite (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermMan1.dest_xrewrite p0_1 in
         let res2 = wrap1 TermMan2.dest_xrewrite p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMan.dest_xrewrite" res1 res2 in
         (merge_term "TermMan.dest_xrewrite - 0" res0_1 res0_2),
         (merge_term "TermMan.dest_xrewrite - 1" res1_1 res1_2)

      let is_xnil_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xnil_term" (wrap1 TermMan1.is_xnil_term p0_1) (wrap1 TermMan2.is_xnil_term p0_2)

      let xnil_term =
         merge_term "TermMan.xnil_term" (TermMan1.xnil_term) (TermMan2.xnil_term)

      let is_xcons_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xcons_term" (wrap1 TermMan1.is_xcons_term p0_1) (wrap1 TermMan2.is_xcons_term p0_2)

      let mk_xcons_term (p0 : term) (p1 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMan.mk_xcons_term" (wrap2 TermMan1.mk_xcons_term p0_1 p1_1) (wrap2 TermMan2.mk_xcons_term p0_2 p1_2)

      let dest_xcons (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermMan1.dest_xcons p0_1 in
         let res2 = wrap1 TermMan2.dest_xcons p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMan.dest_xcons" res1 res2 in
         (merge_term "TermMan.dest_xcons - 0" res0_1 res0_2),
         (merge_term "TermMan.dest_xcons - 1" res1_1 res1_2)

      let is_xlist_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xlist_term" (wrap1 TermMan1.is_xlist_term p0_1) (wrap1 TermMan2.is_xlist_term p0_2)

      let dest_xlist (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_terms "TermMan.dest_xlist" (wrap1 TermMan1.dest_xlist p0_1) (wrap1 TermMan2.dest_xlist p0_2)

      let mk_xlist_term (p0 : term list) =
         let p0_1, p0_2 = split p0 in
         merge merge_term "TermMan.mk_xlist_term" (wrap1 TermMan1.mk_xlist_term p0_1) (wrap1 TermMan2.mk_xlist_term p0_2)

      let xconcl_term =
         merge_term "TermMan.xconcl_term" (TermMan1.xconcl_term) (TermMan2.xconcl_term)

      let is_xstring_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xstring_term" (wrap1 TermMan1.is_xstring_term p0_1) (wrap1 TermMan2.is_xstring_term p0_2)

      let mk_xstring_term (p0 : string) =
         merge merge_term "TermMan.mk_xstring_term" (wrap1 TermMan1.mk_xstring_term p0) (wrap1 TermMan2.mk_xstring_term p0)

      let dest_xstring (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_string "TermMan.dest_xstring" (wrap1 TermMan1.dest_xstring p0_1) (wrap1 TermMan2.dest_xstring p0_2)

      let is_xstring_dep0_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xstring_dep0_term" (wrap1 TermMan1.is_xstring_dep0_term p0_1) (wrap1 TermMan2.is_xstring_dep0_term p0_2)

      let mk_xstring_dep0_term (p0 : string) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMan.mk_xstring_dep0_term" (wrap2 TermMan1.mk_xstring_dep0_term p0 p1_1) (wrap2 TermMan2.mk_xstring_dep0_term p0 p1_2)

      let dest_xstring_dep0_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 TermMan1.dest_xstring_dep0_term p0_1 in
         let res2 = wrap1 TermMan2.dest_xstring_dep0_term p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMan.dest_xstring_dep0_term" res1 res2 in
         (merge_string "TermMan.dest_xstring_dep0_term - 0" res0_1 res0_2),
         (merge_term "TermMan.dest_xstring_dep0_term - 1" res1_1 res1_2)

      let is_xbind_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMan.is_xbind_term" (wrap1 TermMan1.is_xbind_term p0_1) (wrap1 TermMan2.is_xbind_term p0_2)

      let mk_xbind_term (p0 : var) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMan.mk_xbind_term" (wrap2 TermMan1.mk_xbind_term p0 p1_1) (wrap2 TermMan2.mk_xbind_term p0 p1_2)

      let construct_redex (p0 : var array) (p1 : term list) (p2 : term list) =
         let p1_1, p1_2 = split p1 in
         let p2_1, p2_2 = split p2 in
         merge merge_term "TermMan.construct_redex" (wrap3 TermMan1.construct_redex p0 p1_1 p2_1) (wrap3 TermMan2.construct_redex p0 p1_2 p2_2)

   end

   module TermSubst = struct
      module SubstTypes = TermType
      type term_subst = (var * term) list

      (*
       * To generate the code for this module, run the following:
       *    grep '^   [v ][a ][l ]' refiner/refsig/term_subst_sig.ml |  util/gen_refiner_debug.pl TermSubst > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let subst (p0 : term) (p1 : var list) (p2 : term list) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split p2 in
         merge merge_term "TermSubst.subst" (wrap3 TermSubst1.subst p0_1 p1 p2_1) (wrap3 TermSubst2.subst p0_2 p1 p2_2)

      let subst1 (p0 : term) (p1 : var) (p2 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         merge merge_term "TermSubst.subst1" (wrap3 TermSubst1.subst1 p0_1 p1 p2_1) (wrap3 TermSubst2.subst1 p0_2 p1 p2_2)

      let apply_subst (p0 : term_subst) (p1 : term) =
         let p0_1, p0_2 = split_term_subst p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermSubst.apply_subst" (wrap2 TermSubst1.apply_subst p0_1 p1_1) (wrap2 TermSubst2.apply_subst p0_2 p1_2)

      let dest_bterm_and_rename (p0 : SymbolSet.t) (p1 : bound_term) =
         let p1_1, p1_2 = p1 in
         merge merge_bterm' "TermSubst.dest_bterm_and_rename" (wrap2 TermSubst1.dest_bterm_and_rename p0 p1_1) (wrap2 TermSubst2.dest_bterm_and_rename p0 p1_2)

      let var_subst (p0 : term) (p1 : term) (p2 : var) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermSubst.var_subst" (wrap3 TermSubst1.var_subst p0_1 p1_1 p2) (wrap3 TermSubst2.var_subst p0_2 p1_2 p2)

      let equal_params (p0 : param) (p1 : param) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermSubst.equal_params" (wrap2 TermSubst1.equal_params p0_1 p1_1) (wrap2 TermSubst2.equal_params p0_2 p1_2)

      let opparam_eq (p0 : param op_param) (p1 : param op_param) =
         let p0_1, p0_2 = split_opparam p0 in
         let p1_1, p1_2 = split_opparam p1 in
         merge merge_bool "TermSubst.opparam_eq" (wrap2 TermSubst1.opparam_eq p0_1 p1_1) (wrap2 TermSubst2.opparam_eq p0_2 p1_2)

      let equal_operators (p0 : operator) (p1 : operator) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermSubst.equal_operators" (wrap2 TermSubst1.equal_operators p0_1 p1_1) (wrap2 TermSubst2.equal_operators p0_2 p1_2)

      let alpha_equal (p0 : term) (p1 : term) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermSubst.alpha_equal" (wrap2 TermSubst1.alpha_equal p0_1 p1_1) (wrap2 TermSubst2.alpha_equal p0_2 p1_2)

      let alpha_equal_vars (p0 : term) (p1 : var list) (p2 : term) (p3 : var list) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         merge merge_bool "TermSubst.alpha_equal_vars" (wrap4 TermSubst1.alpha_equal_vars p0_1 p1 p2_1 p3) (wrap4 TermSubst2.alpha_equal_vars p0_2 p1 p2_2 p3)

      let alpha_equal_fun (p0 : ( term -> 'a -> bool )) (p1 : term) (p2 : var list) (p3 : term) (p4 : 'a list) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = p1 in
         let p3_1, p3_2 = p3 in
         merge merge_bool "TermSubst.alpha_equal_fun" (wrap5 TermSubst1.alpha_equal_fun p0_1 p1_1 p2 p3_1 p4) (wrap5 TermSubst2.alpha_equal_fun p0_2 p1_2 p2 p3_2 p4)

      let standardize (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermSubst.standardize" (wrap1 TermSubst1.standardize p0_1) (wrap1 TermSubst2.standardize p0_2)

      let is_closed_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermSubst.is_closed_term" (wrap1 TermSubst1.is_closed_term p0_1) (wrap1 TermSubst2.is_closed_term p0_2)

      let is_var_free (p0 : var) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermSubst.is_var_free" (wrap2 TermSubst1.is_var_free p0 p1_1) (wrap2 TermSubst2.is_var_free p0 p1_2)

      let is_some_var_free (p0 : var list) (p1 : term) =
         let p1_1, p1_2 = p1 in
         merge merge_bool "TermSubst.is_some_var_free" (wrap2 TermSubst1.is_some_var_free p0 p1_1) (wrap2 TermSubst2.is_some_var_free p0 p1_2)

      let is_some_var_free_list (p0 : var list) (p1 : term list) =
         let p1_1, p1_2 = split p1 in
         merge merge_bool "TermSubst.is_some_var_free_list" (wrap2 TermSubst1.is_some_var_free_list p0 p1_1) (wrap2 TermSubst2.is_some_var_free_list p0 p1_2)

      let free_vars_list (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_vars "TermSubst.free_vars_list" (wrap1 TermSubst1.free_vars_list p0_1) (wrap1 TermSubst2.free_vars_list p0_2)

      let free_vars_set (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_ss "TermSubst.free_vars_set" (wrap1 TermSubst1.free_vars_set p0_1) (wrap1 TermSubst2.free_vars_set p0_2)

      let free_vars_terms (p0 : term list) =
         let p0_1, p0_2 = split p0 in
         merge merge_ss "TermSubst.free_vars_terms" (wrap1 TermSubst1.free_vars_terms p0_1) (wrap1 TermSubst2.free_vars_terms p0_2)

      let match_terms (p0 : term_subst) (p1 : term) (p2 : term) =
         let p0_1, p0_2 = split_term_subst p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge merge_term_subst "TermSubst.match_terms" (wrap3 TermSubst1.match_terms p0_1 p1_1 p2_1) (wrap3 TermSubst2.match_terms p0_2 p1_2 p2_2)

   end

   module TermShape = struct
      include TermType

      (* XXX: BUG: we are reimplementing the modules instead of debugging the underlying implementation *)
      module ShapeCompare =
      struct
         type t = shape
         let compare = Stdlib.compare
      end

(* unused
      let shape_compare = Stdlib.compare
*)

      module ShapeSet = Lm_set.LmMake (ShapeCompare);;
      module ShapeTable = Lm_map.LmMake (ShapeCompare);;
      module ShapeMTable = Lm_map.LmMakeList (ShapeCompare);;

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/term_shape_sig.ml | sed -e 's/(\*.*//' | egrep -v 'end' | util/gen_refiner_debug.pl TermShape > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let shape_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_shape "TermShape.shape_of_term" (wrap1 TermShape1.shape_of_term p0_1) (wrap1 TermShape2.shape_of_term p0_2)

      let eq (p0 : shape) (p1 : shape) =
         merge merge_bool "TermShape.eq" (wrap2 TermShape1.eq p0 p1) (wrap2 TermShape2.eq p0 p1)

      let shape_eq = eq

      let param_type (p0 : param) =
         let p0_1, p0_2 = p0 in
         merge merge_shape_param "TermShape.param_type" (wrap1 TermShape1.param_type p0_1) (wrap1 TermShape2.param_type p0_2)

      let unquote_shape (p0 : shape) =
         merge merge_shape "TermShape.unquote_shape" (wrap1 TermShape1.unquote_shape p0) (wrap1 TermShape2.unquote_shape p0)

      let opname_of_shape (p0 : shape) =
         merge merge_opname "TermShape.opname_of_shape" (wrap1 TermShape1.opname_of_shape p0) (wrap1 TermShape2.opname_of_shape p0)

      let sequent_shape =
         merge_shape "TermShape.sequent_shape" (TermShape1.sequent_shape) (TermShape2.sequent_shape)

      let var_shape =
         merge_shape "TermShape.var_shape" (TermShape1.var_shape) (TermShape2.var_shape)

      let print_shape (p0 : out_channel) (p1 : shape) =
         merge merge_unit "TermShape.print_shape" (wrap2 TermShape1.print_shape p0 p1) (wrap2 TermShape2.print_shape p0 p1)

      let pp_print_shape (p0 : formatter) (p1 : shape) =
         merge merge_unit "TermShape.pp_print_shape" (wrap2 TermShape1.pp_print_shape p0 p1) (wrap2 TermShape2.pp_print_shape p0 p1)

      let string_of_shape (p0 : shape) =
         merge merge_string "TermShape.string_of_shape" (wrap1 TermShape1.string_of_shape p0) (wrap1 TermShape2.string_of_shape p0)

      let short_string_of_shape (p0 : shape) =
         merge merge_string "TermShape.short_string_of_shape" (wrap1 TermShape1.short_string_of_shape p0) (wrap1 TermShape2.short_string_of_shape p0)

      let shape_compare (p0 : shape) (p1 : shape) =
         merge merge_int "TermShape.shape_compare" (wrap2 TermShape1.shape_compare p0 p1) (wrap2 TermShape2.shape_compare p0 p1)

      let canonical_term_of_shape (p0 : shape) =
         merge merge_term "TermShape.canonical_term_of_shape" (wrap1 TermShape1.canonical_term_of_shape p0) (wrap1 TermShape2.canonical_term_of_shape p0)

      let opparam_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_opparam "TermShape.opparam_of_term" (wrap1 TermShape1.opparam_of_term p0_1) (wrap1 TermShape2.opparam_of_term p0_2)

      let shape_of_opparam (p0 : param op_param) =
         let p0_1, p0_2 = split_opparam p0 in
         merge merge_shape "TermShape.shape_of_opparam" (wrap1 TermShape1.shape_of_opparam p0_1) (wrap1 TermShape2.shape_of_opparam p0_2)

      let string_of_opparam (p0 : param op_param) =
         let p0_1, p0_2 = split_opparam p0 in
         merge merge_string "TermShape.string_of_opparam" (wrap1 TermShape1.string_of_opparam p0_1) (wrap1 TermShape2.string_of_opparam p0_2)

      let canonical_term_of_opparam (p0 : param op_param) =
         let p0_1, p0_2 = split_opparam p0 in
         merge merge_term "TermShape.canonical_term_of_opparam" (wrap1 TermShape1.canonical_term_of_opparam p0_1) (wrap1 TermShape2.canonical_term_of_opparam p0_2)

   end

   module TermTy = struct
      include TermType

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/term_ty_sig.ml | util/gen_refiner_debug.pl TermTy > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let term_of_ty (p0 : ty_term) =
         let p0_1, p0_2 = split_ty_term p0 in
         merge merge_term "TermTy.term_of_ty" (wrap1 TermTy1.term_of_ty p0_1) (wrap1 TermTy2.term_of_ty p0_2)

      let string_of_ty_param (p0 : ty_param) =
         let p0_1, p0_2 = split_ty_param p0 in
         merge merge_string "TermTy.string_of_ty_param" (wrap1 TermTy1.string_of_ty_param p0_1) (wrap1 TermTy2.string_of_ty_param p0_2)

      let eq (p0 : ty_term) (p1 : ty_term) =
         let p0_1, p0_2 = split_ty_term p0 in
         let p1_1, p1_2 = split_ty_term p1 in
         merge merge_bool "TermTy.eq" (wrap2 TermTy1.eq p0_1 p1_1) (wrap2 TermTy2.eq p0_2 p1_2)

      let eq_ty (p0 : ty_term) (p1 : ty_term) =
         let p0_1, p0_2 = split_ty_term p0 in
         let p1_1, p1_2 = split_ty_term p1 in
         merge merge_bool "TermTy.eq_ty" (wrap2 TermTy1.eq_ty p0_1 p1_1) (wrap2 TermTy2.eq_ty p0_2 p1_2)

   end

(* unused
   module TermMetaExt = struct
      module MetaTypes = TermType

      type allow_seq_bindings = term -> bool

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/term_meta_sig.ml | util/gen_refiner_debug.pl TermMeta > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let free_vars_mterm (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         merge merge_ss "TermMeta.free_vars_mterm" (wrap1 TermMeta1.free_vars_mterm p0_1) (wrap1 TermMeta2.free_vars_mterm p0_2)

      let all_vars_mterm (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         merge merge_ss "TermMeta.all_vars_mterm" (wrap1 TermMeta1.all_vars_mterm p0_1) (wrap1 TermMeta2.all_vars_mterm p0_2)

      let context_vars (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         let res1 = wrap1 TermMeta1.context_vars p0_1 in
         let res2 = wrap1 TermMeta2.context_vars p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMeta.context_vars" res1 res2 in
         (merge_ss "TermMeta.context_vars - 0" res0_1 res0_2),
         (merge_ss "TermMeta.context_vars - 1" res1_1 res1_2)

      let context_vars_info (p0 : (bool * int * int) SymbolTable.t) (p1 : meta_term) =
         let p1_1, p1_2 = split_meta_term p1 in
         merge merge_stables "TermMeta.context_vars_info" (wrap2 TermMeta1.context_vars_info p0 p1_1) (wrap2 TermMeta2.context_vars_info p0 p1_2)

      let so_vars_info (p0 : (int * int) SymbolTable.t) (p1 : meta_term) =
         let p1_1, p1_2 = split_meta_term p1 in
         merge merge_stables "TermMeta.so_vars_info" (wrap2 TermMeta1.so_vars_info p0 p1_1) (wrap2 TermMeta2.so_vars_info p0 p1_2)

      let meta_alpha_equal (p0 : meta_term) (p1 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         let p1_1, p1_2 = split_meta_term p1 in
         merge merge_bool "TermMeta.meta_alpha_equal" (wrap2 TermMeta1.meta_alpha_equal p0_1 p1_1) (wrap2 TermMeta2.meta_alpha_equal p0_2 p1_2)

      let unfold_mlabeled (p0 : string) (p1 : meta_term) =
         let p1_1, p1_2 = split_meta_term p1 in
         merge merge_term "TermMeta.unfold_mlabeled" (wrap2 TermMeta1.unfold_mlabeled p0 p1_1) (wrap2 TermMeta2.unfold_mlabeled p0 p1_2)

      let unzip_mimplies (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         let res1 = wrap1 TermMeta1.unzip_mimplies p0_1 in
         let res2 = wrap1 TermMeta2.unzip_mimplies p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMeta.unzip_mimplies" res1 res2 in
         (merge_terms "TermMeta.unzip_mimplies - 0" res0_1 res0_2),
         (merge_term "TermMeta.unzip_mimplies - 1" res1_1 res1_2)

      let zip_mimplies (p0 : term list) (p1 : term) =
         let p0_1, p0_2 = split p0 in
         let p1_1, p1_2 = p1 in
         merge merge_meta_term "TermMeta.zip_mimplies" (wrap2 TermMeta1.zip_mimplies p0_1 p1_1) (wrap2 TermMeta2.zip_mimplies p0_2 p1_2)

      let zip_mlabeled (p0 : (string list * term) list) (p1 : term) =
         let p0_1, p0_2 = split_spl p0 in
         let p1_1, p1_2 = p1 in
         merge merge_meta_term "TermMeta.zip_mlabeled" (wrap2 TermMeta1.zip_mlabeled p0_1 p1_1) (wrap2 TermMeta2.zip_mlabeled p0_2 p1_2)

      let unzip_mfunction (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         merge merge_sltotlt "TermMeta.unzip_mfunction" (wrap1 TermMeta1.unzip_mfunction p0_1) (wrap1 TermMeta2.unzip_mfunction p0_2)

      let zip_mfunction (p0 : (term option * term) list) (p1 : term) =
         let p0_1, p0_2 = split_popl p0 in
         let p1_1, p1_2 = p1 in
         merge merge_meta_term "TermMeta.zip_mfunction" (wrap2 TermMeta1.zip_mfunction p0_1 p1_1) (wrap2 TermMeta2.zip_mfunction p0_2 p1_2)

      let strip_mfunction (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         merge merge_meta_term "TermMeta.strip_mfunction" (wrap1 TermMeta1.strip_mfunction p0_1) (wrap1 TermMeta2.strip_mfunction p0_2)

      let unzip_mrewrite (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         let res1 = wrap1 TermMeta1.unzip_mrewrite p0_1 in
         let res2 = wrap1 TermMeta2.unzip_mrewrite p0_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermMeta.unzip_mrewrite" res1 res2 in
         (merge_terms "TermMeta.unzip_mrewrite - 0" res0_1 res0_2),
         (merge_term "TermMeta.unzip_mrewrite - 1" res1_1 res1_2),
         (merge_term "TermMeta.unzip_mrewrite - 2" res2_1 res2_2)

      let iter_mterm (p0 : (term -> unit)) (p1 : meta_term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = split_meta_term p1 in
         merge merge_unit "TermMeta.iter_mterm" (wrap2 TermMeta1.iter_mterm p0_1 p1_1) (wrap2 TermMeta2.iter_mterm p0_2 p1_2)

      let map_mterm (p0 : (term -> term)) (p1 : meta_term) =
         let p0_1, p0_2 = split_ttf p0 in
         let p1_1, p1_2 = split_meta_term p1 in
         merge merge_meta_term "TermMeta.map_mterm" (wrap2 TermMeta1.map_mterm p0_1 p1_1) (wrap2 TermMeta2.map_mterm p0_2 p1_2)

      let term_of_parsed_term (p0 : allow_seq_bindings) (p1 : term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMeta.term_of_parsed_term" (wrap2 TermMeta1.term_of_parsed_term p0_1 p1_1) (wrap2 TermMeta2.term_of_parsed_term p0_2 p1_2)

      let term_of_parsed_term_with_vars (p0 : allow_seq_bindings) (p1 : term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = p1 in
         merge merge_term "TermMeta.term_of_parsed_term_with_vars" (wrap2 TermMeta1.term_of_parsed_term_with_vars p0_1 p1_1) (wrap2 TermMeta2.term_of_parsed_term_with_vars p0_2 p1_2)

      let mterms_of_parsed_mterms (p0 : allow_seq_bindings) (p1 : meta_term) (p2 : term list) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = split_meta_term p1 in
         let p2_1, p2_2 = split p2 in
         let res1 = wrap3 TermMeta1.mterms_of_parsed_mterms p0_1 p1_1 p2_1 in
         let res2 = wrap3 TermMeta2.mterms_of_parsed_mterms p0_2 p1_2 p2_2 in
         let (res0_1, res1_1, res2_1), (res0_2, res1_2, res2_2) = merge merge_triv "TermMeta.mterms_of_parsed_mterms" res1 res2 in
         (merge_meta_term "TermMeta.mterms_of_parsed_mterms - 0" res0_1 res0_2),
         (merge_terms "TermMeta.mterms_of_parsed_mterms - 1" res1_1 res1_2),
         (merge_ttf "TermMeta.mterms_of_parsed_mterms - 2" res2_1 res2_2)

      let rewrite_of_parsed_rewrite (p0 : allow_seq_bindings) (p1 : term) (p2 : term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let res1 = wrap3 TermMeta1.rewrite_of_parsed_rewrite p0_1 p1_1 p2_1 in
         let res2 = wrap3 TermMeta2.rewrite_of_parsed_rewrite p0_2 p1_2 p2_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMeta.rewrite_of_parsed_rewrite" res1 res2 in
         (merge_term "TermMeta.rewrite_of_parsed_rewrite - 0" res0_1 res0_2),
         (merge_term "TermMeta.rewrite_of_parsed_rewrite - 1" res1_1 res1_2)

      let mrewrite_of_parsed_mrewrite (p0 : allow_seq_bindings) (p1 : term list) (p2 : term) =
         let p0_1, p0_2 = split_taf p0 in
         let p1_1, p1_2 = split p1 in
         let p2_1, p2_2 = p2 in
         let res1 = wrap3 TermMeta1.mrewrite_of_parsed_mrewrite p0_1 p1_1 p2_1 in
         let res2 = wrap3 TermMeta2.mrewrite_of_parsed_mrewrite p0_2 p1_2 p2_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "TermMeta.mrewrite_of_parsed_mrewrite" res1 res2 in
         (merge_terms "TermMeta.mrewrite_of_parsed_mrewrite - 0" res0_1 res0_2),
         (merge_term "TermMeta.mrewrite_of_parsed_mrewrite - 1" res1_1 res1_2)

      let display_term_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "TermMeta.display_term_of_term" (wrap1 TermMeta1.display_term_of_term p0_1) (wrap1 TermMeta2.display_term_of_term p0_2)

      let context_subst_of_terms (p0 : term list) (p1 : var) (p2 : int) =
         let p0_1, p0_2 = split p0 in
         merge merge_var_lo "TermMeta.context_subst_of_terms" (wrap3 TermMeta1.context_subst_of_terms p0_1 p1 p2) (wrap3 TermMeta2.context_subst_of_terms p0_2 p1 p2)

      let encode_free_var (p0 : var) =
         merge merge_term "TermMeta.encode_free_var" (wrap1 TermMeta1.encode_free_var p0) (wrap1 TermMeta2.encode_free_var p0)

      let is_encoded_free_var (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMeta.is_encoded_free_var" (wrap1 TermMeta1.is_encoded_free_var p0_1) (wrap1 TermMeta2.is_encoded_free_var p0_2)

      let decode_free_var (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_var "TermMeta.decode_free_var" (wrap1 TermMeta1.decode_free_var p0_1) (wrap1 TermMeta2.decode_free_var p0_2)

      let is_meta_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_bool "TermMeta.is_meta_term" (wrap1 TermMeta1.is_meta_term p0_1) (wrap1 TermMeta2.is_meta_term p0_2)

      let meta_term_of_term (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_meta_term "TermMeta.meta_term_of_term" (wrap1 TermMeta1.meta_term_of_term p0_1) (wrap1 TermMeta2.meta_term_of_term p0_2)

      let term_of_meta_term (p0 : meta_term) =
         let p0_1, p0_2 = split_meta_term p0 in
         merge merge_term "TermMeta.term_of_meta_term" (wrap1 TermMeta1.term_of_meta_term p0_1) (wrap1 TermMeta2.term_of_meta_term p0_2)

end
*)

(* unused
   module RewriteExt = struct
      module RwTypes = TermType
      include TermType

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/rewrite_sig.ml | grep -v unzip_mfunction | sed -e 's/(\*.*//' | util/gen_refiner_debug.pl Rewrite > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let empty_args_spec =
         merge_rwspecs "Rewrite.empty_args_spec" (Rewrite1.empty_args_spec) (Rewrite2.empty_args_spec)

      let empty_rw_args =
         merge_rwargs "Rewrite.empty_rw_args" (Rewrite1.empty_rw_args) (Rewrite2.empty_rw_args)

      let empty_args =
         merge_rewrite_args "Rewrite.empty_args" (Rewrite1.empty_args) (Rewrite2.empty_args)

      let compile_redex (p0 : strict) (p1 : rewrite_args_spec) (p2 : term) =
         let p2_1, p2_2 = p2 in
         merge merge_triv "Rewrite.compile_redex" (wrap3 Rewrite1.compile_redex p0 p1 p2_1) (wrap3 Rewrite2.compile_redex p0 p1 p2_2)

      let compile_redices (p0 : strict) (p1 : rewrite_args_spec) (p2 : term list) =
         let p2_1, p2_2 = split p2 in
         merge merge_triv "Rewrite.compile_redices" (wrap3 Rewrite1.compile_redices p0 p1 p2_1) (wrap3 Rewrite2.compile_redices p0 p1 p2_2)

      let extract_redex_types (p0 : rewrite_redex) =
         let p0_1, p0_2 = p0 in
         merge merge_rwtvl "Rewrite.extract_redex_types" (wrap1 Rewrite1.extract_redex_types p0_1) (wrap1 Rewrite2.extract_redex_types p0_2)

      let test_redex_applicability (p0 : rewrite_redex) (p1 : rw_args) (p2 : term) (p3 : term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split_args p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = split p3 in
         merge merge_unit "Rewrite.test_redex_applicability" (wrap4 Rewrite1.test_redex_applicability p0_1 p1_1 p2_1 p3_1) (wrap4 Rewrite2.test_redex_applicability p0_2 p1_2 p2_2 p3_2)

      let apply_redex (p0 : rewrite_redex) (p1 : rw_args) (p2 : term) (p3 : term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split_args p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = split p3 in
         merge merge_rewrite_items "Rewrite.apply_redex" (wrap4 Rewrite1.apply_redex p0_1 p1_1 p2_1 p3_1) (wrap4 Rewrite2.apply_redex p0_2 p1_2 p2_2 p3_2)

      let term_rewrite (p0 : strict) (p1 : rewrite_args_spec) (p2 : term list) (p3 : term list) =
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split p3 in
         merge merge_triv "Rewrite.term_rewrite" (wrap4 Rewrite1.term_rewrite p0 p1 p2_1 p3_1) (wrap4 Rewrite2.term_rewrite p0 p1 p2_2 p3_2)

      let fun_rewrite (p0 : strict) (p1 : term) (p2 : (term -> term)) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = split_ttf p2 in
         merge merge_triv "Rewrite.fun_rewrite" (wrap3 Rewrite1.fun_rewrite p0 p1_1 p2_1) (wrap3 Rewrite2.fun_rewrite p0 p1_2 p2_2)

      let apply_rewrite (p0 : rewrite_rule) (p1 : rewrite_args) (p2 : term) (p3 : term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split_rewrite_args p1 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = split p3 in
         merge merge_terms "Rewrite.apply_rewrite" (wrap4 Rewrite1.apply_rewrite p0_1 p1_1 p2_1 p3_1) (wrap4 Rewrite2.apply_rewrite p0_2 p1_2 p2_2 p3_2)

      let print_rewrite_redex (p0 : out_channel) (p1 : rewrite_redex) =
         let p1_1, p1_2 = p1 in
         merge merge_unit "Rewrite.print_rewrite_redex" (wrap2 Rewrite1.print_rewrite_redex p0 p1_1) (wrap2 Rewrite2.print_rewrite_redex p0 p1_2)

      let print_rewrite_rule (p0 : out_channel) (p1 : rewrite_rule) =
         let p1_1, p1_2 = p1 in
         merge merge_unit "Rewrite.print_rewrite_rule" (wrap2 Rewrite1.print_rewrite_rule p0 p1_1) (wrap2 Rewrite2.print_rewrite_rule p0 p1_2)

   end
*)

(* unused
   module RefineExt = struct
      include TermType

      (*
       * To generate the code for this module, run the following:
       *   grep '^   [v ][a ][l ]' refiner/refsig/refine_sig.ml | grep -v ' of ' | sed -e 's/(\*.*//' | util/gen_refiner_debug.pl Refine > /tmp/code
       *)

      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)

      let any_sentinal =
         merge_triv "Refine.any_sentinal" (Refine1.any_sentinal) (Refine2.any_sentinal)

      let null_sentinal =
         merge_triv "Refine.null_sentinal" (Refine1.null_sentinal) (Refine2.null_sentinal)

      let refine (p0 : sentinal) (p1 : tactic) (p2 : msequent) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         let res1 = wrap3 Refine1.refine p0_1 p1_1 p2_1 in
         let res2 = wrap3 Refine2.refine p0_2 p1_2 p2_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "Refine.refine" res1 res2 in
         (merge_msequents "Refine.refine - 0" res0_1 res0_2),
         (merge_triv "Refine.refine - 1" res1_1 res1_2)

      let compose (p0 : extract) (p1 : extract list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split p1 in
         merge merge_triv "Refine.compose" (wrap2 Refine1.compose p0_1 p1_1) (wrap2 Refine2.compose p0_2 p1_2)

      let nth_hyp (p0 : int) =
         merge merge_triv "Refine.nth_hyp" (wrap1 Refine1.nth_hyp p0) (wrap1 Refine2.nth_hyp p0)

      let cut (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_triv "Refine.cut" (wrap1 Refine1.cut p0_1) (wrap1 Refine2.cut p0_2)

      let identity (p0 : sentinal) (p1 : msequent) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.identity" (wrap2 Refine1.identity p0_1 p1_1) (wrap2 Refine2.identity p0_2 p1_2)

      let subgoals_of_extract (p0 : extract) =
         let p0_1, p0_2 = p0 in
         merge merge_msequents "Refine.subgoals_of_extract" (wrap1 Refine1.subgoals_of_extract p0_1) (wrap1 Refine2.subgoals_of_extract p0_2)

      let describe_extract (p0 : extract) =
         let p0_1, p0_2 = p0 in
         merge merge_extract_description "Refine.describe_extract" (wrap1 Refine1.describe_extract p0_1) (wrap1 Refine2.describe_extract p0_2)

      let rwaddr (p0 : address) (p1 : rw) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.rwaddr" (wrap2 Refine1.rwaddr p0_1 p1_1) (wrap2 Refine2.rwaddr p0_2 p1_2)

      let rwhigher (p0 : rw) =
         let p0_1, p0_2 = p0 in
         merge merge_triv "Refine.rwhigher" (wrap1 Refine1.rwhigher p0_1) (wrap1 Refine2.rwhigher p0_2)

      let rwtactic (p0 : int) (p1 : rw) =
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.rwtactic" (wrap2 Refine1.rwtactic p0 p1_1) (wrap2 Refine2.rwtactic p0 p1_2)

      let andthenrw (p0 : rw) (p1 : rw) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.andthenrw" (wrap2 Refine1.andthenrw p0_1 p1_1) (wrap2 Refine2.andthenrw p0_2 p1_2)

      let orelserw (p0 : rw) (p1 : rw) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.orelserw" (wrap2 Refine1.orelserw p0_1 p1_1) (wrap2 Refine2.orelserw p0_2 p1_2)

      let crwaddr (p0 : address) (p1 : cond_rewrite) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.crwaddr" (wrap2 Refine1.crwaddr p0_1 p1_1) (wrap2 Refine2.crwaddr p0_2 p1_2)

      let crwtactic (p0 : int) (p1 : cond_rewrite) =
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.crwtactic" (wrap2 Refine1.crwtactic p0 p1_1) (wrap2 Refine2.crwtactic p0 p1_2)

      let candthenrw (p0 : cond_rewrite) (p1 : cond_rewrite) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.candthenrw" (wrap2 Refine1.candthenrw p0_1 p1_1) (wrap2 Refine2.candthenrw p0_2 p1_2)

      let corelserw (p0 : cond_rewrite) (p1 : cond_rewrite) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_triv "Refine.corelserw" (wrap2 Refine1.corelserw p0_1 p1_1) (wrap2 Refine2.corelserw p0_2 p1_2)

      let mk_msequent (p0 : term) (p1 : term list) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = split p1 in
         merge merge_msequent "Refine.mk_msequent" (wrap2 Refine1.mk_msequent p0_1 p1_1) (wrap2 Refine2.mk_msequent p0_2 p1_2)

      let dest_msequent (p0 : msequent) =
         let p0_1, p0_2 = p0 in
         let res1 = wrap1 Refine1.dest_msequent p0_1 in
         let res2 = wrap1 Refine2.dest_msequent p0_2 in
         let (res0_1, res1_1), (res0_2, res1_2) = merge merge_triv "Refine.dest_msequent" res1 res2 in
         (merge_term "Refine.dest_msequent - 0" res0_1 res0_2),
         (merge_terms "Refine.dest_msequent - 1" res1_1 res1_2)

      let msequent_goal (p0 : msequent) =
         let p0_1, p0_2 = p0 in
         merge merge_term "Refine.msequent_goal" (wrap1 Refine1.msequent_goal p0_1) (wrap1 Refine2.msequent_goal p0_2)

      let msequent_num_assums (p0 : msequent) =
         let p0_1, p0_2 = p0 in
         merge merge_int "Refine.msequent_num_assums" (wrap1 Refine1.msequent_num_assums p0_1) (wrap1 Refine2.msequent_num_assums p0_2)

      let msequent_nth_assum (p0 : msequent) (p1 : int) =
         let p0_1, p0_2 = p0 in
         merge merge_term "Refine.msequent_nth_assum" (wrap2 Refine1.msequent_nth_assum p0_1 p1) (wrap2 Refine2.msequent_nth_assum p0_2 p1)

      let msequent_alpha_equal (p0 : msequent) (p1 : msequent) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_bool "Refine.msequent_alpha_equal" (wrap2 Refine1.msequent_alpha_equal p0_1 p1_1) (wrap2 Refine2.msequent_alpha_equal p0_2 p1_2)

      let null_refiner (p0 : string) =
         merge merge_triv "Refine.null_refiner" (wrap1 Refine1.null_refiner p0) (wrap1 Refine2.null_refiner p0)

      let refiner_of_build (p0 : build) =
         let p0_1, p0_2 = p0 in
         merge merge_triv "Refine.refiner_of_build" (wrap1 Refine1.refiner_of_build p0_1) (wrap1 Refine2.refiner_of_build p0_2)

      let extract_term (p0 : refiner) (p1 : opname) (p2 : term list) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split p2 in
         merge merge_term "Refine.extract_term" (wrap3 Refine1.extract_term p0_1 p1 p2_1) (wrap3 Refine2.extract_term p0_2 p1 p2_2)

      let compute_dependencies (p0 : refiner) (p1 : opname) =
         let p0_1, p0_2 = p0 in
         merge merge_dos "Refine.compute_dependencies" (wrap2 Refine1.compute_dependencies p0_1 p1) (wrap2 Refine2.compute_dependencies p0_2 p1)

      let sentinal_of_refiner (p0 : refiner) =
         let p0_1, p0_2 = p0 in
         merge merge_triv "Refine.sentinal_of_refiner" (wrap1 Refine1.sentinal_of_refiner p0_1) (wrap1 Refine2.sentinal_of_refiner p0_2)

      let find_sentinal (p0 : refiner) (p1 : opname) =
         let p0_1, p0_2 = p0 in
         merge merge_triv "Refine.find_sentinal" (wrap2 Refine1.find_sentinal p0_1 p1) (wrap2 Refine2.find_sentinal p0_2 p1)

      let create_rule (p0 : build) (p1 : string) (p2 : rewrite_args_spec) (p3 : term list) (p4 : meta_term) =
         let p0_1, p0_2 = p0 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = split_meta_term p4 in
         merge merge_prim_tactic "Refine.create_rule" (wrap5 Refine1.create_rule p0_1 p1 p2 p3_1 p4_1) (wrap5 Refine2.create_rule p0_2 p1 p2 p3_2 p4_2)

      let create_ml_rule (p0 : build) (p1 : string) (p2 : ml_rule) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split_ml_rule p2 in
         merge merge_prim_tactic "Refine.create_ml_rule" (wrap3 Refine1.create_ml_rule p0_1 p1 p2_1) (wrap3 Refine2.create_ml_rule p0_2 p1 p2_2)

      let check_rule (p0 : string) (p1 : rewrite_args_spec) (p2 : term list) (p3 : meta_term) =
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split_meta_term p3 in
         merge merge_unit "Refine.check_rule" (wrap4 Refine1.check_rule p0 p1 p2_1 p3_1) (wrap4 Refine2.check_rule p0 p1 p2_2 p3_2)

      let check_prim_rule (p0 : string) (p1 : rewrite_args_spec) (p2 : term list) (p3 : meta_term) (p4 : term list) (p5 : term) =
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split_meta_term p3 in
         let p4_1, p4_2 = split p4 in
         let p5_1, p5_2 = p5 in
         merge merge_unit "Refine.check_prim_rule" (wrap6 Refine1.check_prim_rule p0 p1 p2_1 p3_1 p4_1 p5_1) (wrap6 Refine2.check_prim_rule p0 p1 p2_2 p3_2 p4_2 p5_2)

      let prim_rule (p0 : build) (p1 : string) (p2 : rewrite_args_spec) (p3 : term list) (p4 : meta_term) (p5 : term list) (p6 : term) =
         let p0_1, p0_2 = p0 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = split_meta_term p4 in
         let p5_1, p5_2 = split p5 in
         let p6_1, p6_2 = p6 in
         merge merge_unit "Refine.prim_rule" (wrap7 Refine1.prim_rule p0_1 p1 p2 p3_1 p4_1 p5_1 p6_1) (wrap7 Refine2.prim_rule p0_2 p1 p2 p3_2 p4_2 p5_2 p6_2)

      let derived_rule (p0 : build) (p1 : string) (p2 : rewrite_args_spec) (p3 : term list) (p4 : meta_term) (p5 : unit) (p6 : extract) =
         let p0_1, p0_2 = p0 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = split_meta_term p4 in
         let p6_1, p6_2 = p6 in
         merge merge_unit "Refine.derived_rule" (wrap7 Refine1.derived_rule p0_1 p1 p2 p3_1 p4_1 p5 p6_1) (wrap7 Refine2.derived_rule p0_2 p1 p2 p3_2 p4_2 p5 p6_2)

      let delayed_rule (p0 : build) (p1 : string) (p2 : rewrite_args_spec) (p3 : term list) (p4 : meta_term) (p5 : unit) (p6 : (unit -> extract)) =
         let p0_1, p0_2 = p0 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = split_meta_term p4 in
         let p6_1, p6_2 = split_utriv p6 in
         merge merge_unit "Refine.delayed_rule" (wrap7 Refine1.delayed_rule p0_1 p1 p2 p3_1 p4_1 p5 p6_1) (wrap7 Refine2.delayed_rule p0_2 p1 p2 p3_2 p4_2 p5 p6_2)

      let create_rewrite (p0 : build) (p1 : string) (p2 : term) (p3 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_prim_rewrite "Refine.create_rewrite" (wrap4 Refine1.create_rewrite p0_1 p1 p2_1 p3_1) (wrap4 Refine2.create_rewrite p0_2 p1 p2_2 p3_2)

      let create_input_form (p0 : build) (p1 : string) (p2 : bool) (p3 : term) (p4 : term) =
         let p0_1, p0_2 = p0 in
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_prim_rewrite "Refine.create_input_form" (wrap5 Refine1.create_input_form p0_1 p1 p2 p3_1 p4_1) (wrap5 Refine2.create_input_form p0_2 p1 p2 p3_2 p4_2)

      let create_ml_rewrite (p0 : build) (p1 : string) (p2 : ml_rewrite) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split_ttf p2 in
         merge merge_prim_rewrite "Refine.create_ml_rewrite" (wrap3 Refine1.create_ml_rewrite p0_1 p1 p2_1) (wrap3 Refine2.create_ml_rewrite p0_2 p1 p2_2)

      let prim_rewrite (p0 : build) (p1 : string) (p2 : term) (p3 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_unit "Refine.prim_rewrite" (wrap4 Refine1.prim_rewrite p0_1 p1 p2_1 p3_1) (wrap4 Refine2.prim_rewrite p0_2 p1 p2_2 p3_2)

      let definitional_rewrite (p0 : build) (p1 : string) (p2 : term) (p3 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         merge merge_unit "Refine.definitional_rewrite" (wrap4 Refine1.definitional_rewrite p0_1 p1 p2_1 p3_1) (wrap4 Refine2.definitional_rewrite p0_2 p1 p2_2 p3_2)

      let derived_rewrite (p0 : build) (p1 : string) (p2 : term) (p3 : term) (p4 : extract) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = p4 in
         merge merge_unit "Refine.derived_rewrite" (wrap5 Refine1.derived_rewrite p0_1 p1 p2_1 p3_1 p4_1) (wrap5 Refine2.derived_rewrite p0_2 p1 p2_2 p3_2 p4_2)

      let delayed_rewrite (p0 : build) (p1 : string) (p2 : term) (p3 : term) (p4 : (unit -> extract)) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = p2 in
         let p3_1, p3_2 = p3 in
         let p4_1, p4_2 = split_utriv p4 in
         merge merge_unit "Refine.delayed_rewrite" (wrap5 Refine1.delayed_rewrite p0_1 p1 p2_1 p3_1 p4_1) (wrap5 Refine2.delayed_rewrite p0_2 p1 p2_2 p3_2 p4_2)

      let create_cond_rewrite (p0 : build) (p1 : string) (p2 : rewrite_args_spec) (p3 : term list) (p4 : term list) (p5 : term) (p6 : term) =
         let p0_1, p0_2 = p0 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = split p4 in
         let p5_1, p5_2 = p5 in
         let p6_1, p6_2 = p6 in
         merge merge_prim_rewrite "Refine.create_cond_rewrite" (wrap7 Refine1.create_cond_rewrite p0_1 p1 p2 p3_1 p4_1 p5_1 p6_1) (wrap7 Refine2.create_cond_rewrite p0_2 p1 p2 p3_2 p4_2 p5_2 p6_2)

      let create_ml_cond_rewrite (p0 : build) (p1 : string) (p2 : ml_cond_rewrite) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split_ml_cond_rewrite p2 in
         merge merge_prim_rewrite "Refine.create_ml_cond_rewrite" (wrap3 Refine1.create_ml_cond_rewrite p0_1 p1 p2_1) (wrap3 Refine2.create_ml_cond_rewrite p0_2 p1 p2_2)

      let prim_cond_rewrite (p0 : build) (p1 : string) (p2 : term list) (p3 : term list) (p4 : term) (p5 : term) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         merge merge_unit "Refine.prim_cond_rewrite" (wrap6 Refine1.prim_cond_rewrite p0_1 p1 p2_1 p3_1 p4_1 p5_1) (wrap6 Refine2.prim_cond_rewrite p0_2 p1 p2_2 p3_2 p4_2 p5_2)

      let derived_cond_rewrite (p0 : build) (p1 : string) (p2 : term list) (p3 : term list) (p4 : term) (p5 : term) (p6 : extract) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         let p6_1, p6_2 = p6 in
         merge merge_unit "Refine.derived_cond_rewrite" (wrap7 Refine1.derived_cond_rewrite p0_1 p1 p2_1 p3_1 p4_1 p5_1 p6_1) (wrap7 Refine2.derived_cond_rewrite p0_2 p1 p2_2 p3_2 p4_2 p5_2 p6_2)

      let delayed_cond_rewrite (p0 : build) (p1 : string) (p2 : term list) (p3 : term list) (p4 : term) (p5 : term) (p6 : (unit -> extract)) =
         let p0_1, p0_2 = p0 in
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         let p6_1, p6_2 = split_utriv p6 in
         merge merge_unit "Refine.delayed_cond_rewrite" (wrap7 Refine1.delayed_cond_rewrite p0_1 p1 p2_1 p3_1 p4_1 p5_1 p6_1) (wrap7 Refine2.delayed_cond_rewrite p0_2 p1 p2_2 p3_2 p4_2 p5_2 p6_2)

      let check_rewrite (p0 : string) (p1 : rewrite_args_spec) (p2 : term list) (p3 : term list) (p4 : term) (p5 : term) =
         let p2_1, p2_2 = split p2 in
         let p3_1, p3_2 = split p3 in
         let p4_1, p4_2 = p4 in
         let p5_1, p5_2 = p5 in
         merge merge_unit "Refine.check_rewrite" (wrap6 Refine1.check_rewrite p0 p1 p2_1 p3_1 p4_1 p5_1) (wrap6 Refine2.check_rewrite p0 p1 p2_2 p3_2 p4_2 p5_2)

      let check_iform (p0 : string) (p1 : term) (p2 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge merge_unit "Refine.check_iform" (wrap3 Refine1.check_iform p0 p1_1 p2_1) (wrap3 Refine2.check_iform p0 p1_2 p2_2)

      let check_definition (p0 : string) (p1 : term) (p2 : term) =
         let p1_1, p1_2 = p1 in
         let p2_1, p2_2 = p2 in
         merge merge_unit "Refine.check_definition" (wrap3 Refine1.check_definition p0 p1_1 p2_1) (wrap3 Refine2.check_definition p0 p1_2 p2_2)

      let mk_rewrite_hack (p0 : term) =
         let p0_1, p0_2 = p0 in
         merge merge_term "Refine.mk_rewrite_hack" (wrap1 Refine1.mk_rewrite_hack p0_1) (wrap1 Refine2.mk_rewrite_hack p0_2)

      let label_refiner (p0 : build) (p1 : string) =
         let p0_1, p0_2 = p0 in
         merge merge_triv "Refine.label_refiner" (wrap2 Refine1.label_refiner p0_1 p1) (wrap2 Refine2.label_refiner p0_2 p1)

      let join_refiner (p0 : build) (p1 : refiner) =
         let p0_1, p0_2 = p0 in
         let p1_1, p1_2 = p1 in
         merge merge_unit "Refine.join_refiner" (wrap2 Refine1.join_refiner p0_1 p1_1) (wrap2 Refine2.join_refiner p0_2 p1_2)

   end
*)

   module TermMetaInt =
      Term_meta_gen.TermMeta (TermType) (Term) (TermSubst) (TermOp) (TermMan) (RefineError)
   module RewriteInt =
      Rewrite.Rewrite (TermType) (Term) (TermOp) (TermMan) (TermAddr) (TermSubst) (TermShape) (RefineError)
   module RefineInt =
      Refine.Refine (TermType) (Term) (TermOp) (TermMan) (TermSubst) (TermAddr) (TermMetaInt) (TermShape) (RewriteInt) (RefineError)

   (* Debug internal interfaces *)
   module TermMeta = TermMetaInt
   module Rewrite = RewriteInt
   module Refine = RefineInt

(*
   (* Debug external interfaces *)
   module TermMeta = TermMetaExt
   module Rewrite = RewriteExt
   module Refine = RefineExt
*)

   (* XXX; TODO: the underlying implementations are ignored and are not debugged! *)
   module TermMod = struct
      module TermType = TermType
      module Term = Term
      module TermSubst = TermSubst
      module TermMan = TermMan
      module TermMeta = TermMeta
      module TermShape = TermShape
      module Refine = Refine
   end
   module TermHash = Term_hash.TermHash (TermMod)
   module TermNorm = Term_norm.TermNorm (TermMod) (TermHash)

   module TermHeaderConstr (FromTerm : TermModuleSig) =
   struct
      module THC = Term_header_constr.TermHeaderConstr (FromTerm) (TermMod) (TermHash);;

      let make_term_header = THC.make_term_header
      let make_meta_term_header = THC.make_meta_term_header
      let make_msequent_header = THC.make_msequent_header
   end

end

