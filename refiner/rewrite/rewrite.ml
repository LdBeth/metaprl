(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

INCLUDE "refine_error.mlh"

open Printf
open Mp_debug
open Opname
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_meta_sig
open Term_shape_sig
open Refine_error_sig

open Rewrite_types
open Rewrite_util
open Rewrite_debug
open Rewrite_compile_redex
open Rewrite_compile_contractum
open Rewrite_match_redex
open Rewrite_build_contractum
open Rewrite_meta

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite%t"

let debug_rewrite = load_debug "rewrite"

(*
 * Rewrite module.
 *)
module Rewrite (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term = TermType.term
    with type term' = TermType.term'
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term'
    with type operator = TermType.operator
    with type operator' = TermType.operator'
    with type param = TermType.param
    with type param' = TermType.param'
    with type level_exp = TermType.level_exp
    with type level_exp' = TermType.level_exp'
    with type level_exp_var = TermType.level_exp_var
    with type level_exp_var' = TermType.level_exp_var'
    with type object_id = TermType.object_id
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals
    with type hypothesis = TermType.hypothesis)
   (TermMan : TermManSig
    with type term = TermType.term
    with type level_exp = TermType.level_exp
    with type esequent = TermType.esequent)
   (TermAddr : TermAddrSig
    with type term = TermType.term)
   (TermSubst : TermSubstSig
    with type term = TermType.term
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term')
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals)
=
struct
   (************************************************************************
    * MODULES                                                              *
    ************************************************************************)

   open TermType
   open Term
   open TermMan
   open TermAddr
   open TermSubst
   open RefineError

   module RewriteTypes = MakeRewriteTypes (TermType) (TermAddr)
   module RewriteUtil = MakeRewriteUtil (TermType) (Term) (RefineError) (RewriteTypes)
   module RewriteDebug = MakeRewriteDebug (TermType) (Term) (TermAddr) (RefineError) (RewriteTypes)
   module RewriteCompileRedex =
      MakeRewriteCompileRedex (TermType) (Term) (TermMan) (TermAddr) (**)
         (TermSubst) (RefineError) (RewriteTypes) (RewriteUtil)
   module RewriteCompileContractum =
      MakeRewriteCompileContractum (TermType) (Term) (TermMan) (TermAddr) (**)
         (TermSubst) (RefineError) (RewriteTypes) (RewriteUtil) (RewriteDebug)
   module RewriteMatchRedex =
      MakeRewriteMatchRedex (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (**)
         (RefineError) (RewriteTypes) (RewriteUtil) (RewriteDebug)
   module RewriteBuildContractum =
      MakeRewriteBuildContractum (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (**)
         (RefineError) (RewriteTypes) (RewriteUtil) (RewriteDebug)
   module RewriteMeta =
      MakeRewriteMeta (TermType) (Term) (TermMan) (RefineError) (RewriteTypes)

   open RewriteTypes
   open RewriteUtil
   open RewriteDebug
   open RewriteCompileRedex
   open RewriteCompileContractum
   open RewriteMatchRedex
   open RewriteBuildContractum
   open RewriteMeta

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type term = Term.term
   type level_exp = Term.level_exp
   type operator = Term.operator
   type address = TermAddr.address

   type rewrite_rule = RewriteTypes.rewrite_rule
   type rewrite_redex = RewriteTypes.rewrite_redex
   type rewrite_contractum = RewriteTypes.rewrite_contractum
   type rewrite_stack = RewriteTypes.rewrite_stack

   (*
    * Types for redex matching.
    *)
   type rewrite_type =
      RewriteTermType of string
    | RewriteFunType of string
    | RewriteContextType of string
    | RewriteStringType of string
    | RewriteNumType of string
    | RewriteLevelType of string

   type rewrite_item =
      RewriteTerm of term
    | RewriteFun of (term list -> term)
    | RewriteContext of (term -> term list -> term)
    | RewriteString of string
    | RewriteNum of Mp_num.num
    | RewriteLevel of level_exp

   type strict = RewriteTypes.strict = Strict | Relaxed

   (************************************************************************
    * IMPORTS                                                              *
    ************************************************************************)

   let relevant_rule = RewriteMeta.relevant_rule
   let rewrite_operator = RewriteMeta.rewrite_operator
   let rewrite_eval_flags = RewriteMeta.rewrite_eval_flags

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   let opname_exn = RefineError ("Rewrite.apply_rewrite", RewriteStringError "opnames do not match")

   (*
    * To do the rewrite. match agaist the redex, then
    * instantiate the contractum.
    *)
   let apply_rewrite
       { rr_redex = redex;
         rr_contractum = contractum;
         rr_gstacksize = gstacksize
       } (addrs, names, bnames) goal params =
      let _ =
         (* Check the opnames to short-circuit applications that quickly fail *)
         match redex with
            RWComposite { rw_op = { rw_name = opname1 } } :: _ ->
               if not (Opname.eq opname1 (opname_of_term goal)) then
                  REF_RAISE(opname_exn)
          | _ ->
               ()
      in
      let gstack = Array.create gstacksize StackVoid in
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "Rewrite.apply_rewrite: match_redex%t" eflush
         ENDIF;
         match_redex addrs gstack goal params redex;
         let result =
            match contractum with
               RWCTerm (con, enames) ->
                  let names =
                     if Array.length enames = 0 then
                        names
                     else
                        let vars = free_vars_terms (goal::params) in
                        let enames = contracta_enames vars bnames enames in
                           Array.append names enames
                  in
                     IFDEF VERBOSE_EXN THEN
                        if !debug_rewrite then
                           eprintf "Rewrite.apply_rewrite: build_contractum%t" eflush
                     ENDIF;
                     List.map (build_contractum names bnames gstack) con
             | RWCFunction f ->
                  if params == [] then
                     [f goal]
                  else
                     REF_RAISE(RefineError ("apply_rewrite", RewriteBadMatch (TermMatch xnil_term)))
         in
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.apply_rewrite: done%t" eflush
            ENDIF;
            result

   (*
    * Compute the redex types.
    *)
   let extract_redex_type = function
      FOVarPattern s -> RewriteTermType s
    | SOVarPattern (s, _)
    | SOVarInstance (s, _) -> RewriteFunType s
    | CVar s -> RewriteContextType s
    | PVar (s, ShapeNumber) -> RewriteNumType s
    | FOVar s
    | PVar (s, (ShapeString | ShapeToken | ShapeVar)) -> RewriteStringType s
    | PVar (s, ShapeLevel) -> RewriteLevelType s

   let extract_redex_types { redex_stack = stack } =
      let l = Array.length stack in
      let rec aux j =
         if j < l then
            (extract_redex_type stack.(j))::(aux (j + 1))
         else
            []
      in
         aux 0

   (*
    * Given the two stack, extract values that can be used in a program.
    * For each object:
    *    1. A second order variable becomes a function
    *       that takes a list of subexpressions and performs the substitution.
    *    2. A first order variable becomes the string with the name.
    *    3. A context variable is converted to a function
    *       that takes the hole and the subexpressions and
    *       performs the substitution
    *    4. A param variable becaome the param that was matched.
    *)
   let extract_exn = RefineError ("extract_redex_values", RewriteStringError "stack entry is not valid")

   let extract_redex_values_aux gstack = function
      FOVarPattern _ ->
         begin
            match gstack with
               StackBTerm (t, []) -> RewriteTerm t
             | _ -> REF_RAISE(extract_exn)
         end
    | SOVarPattern _ ->
         begin
            match gstack with
               StackBTerm (t, l) ->
                  RewriteFun (subst t l)
             | _ -> REF_RAISE(extract_exn)
         end
    | SOVarInstance _ ->
         failwith "extract_redex_values: SOVarInstance"
    | CVar _ ->
         begin
            match gstack with
               StackContext (l, t, addr) ->
                  RewriteContext (fun c l' -> subst (replace_subterm t addr c) l l')
             | _ -> REF_RAISE(extract_exn)
         end
    | PVar (_, ShapeNumber) ->
         begin
            match gstack with
               StackNumber i -> RewriteNum i
             | StackMString s -> RewriteString s
             | _ -> REF_RAISE(extract_exn)
         end
    | FOVar _
    | PVar (_, (ShapeString | ShapeToken | ShapeVar)) ->
         begin
            match gstack with
               StackString s | StackMString s -> RewriteString s
             | _ -> REF_RAISE(extract_exn)
         end
    | PVar (_, ShapeLevel) ->
         begin
            match gstack with
               StackLevel l -> RewriteLevel l
             | StackMString s -> RewriteString s
             | _ -> REF_RAISE(extract_exn)
         end

   let extract_redex_values gstack stack=
      let l = Array.length gstack in
      let rec aux' i =
         if i < l then
            (extract_redex_values_aux gstack.(i) stack.(i))::(aux' (i + 1))
         else
            []
      in
         aux' 0

   (*
    * Match with a redex, and extract the forms to be bound.
    *)
   let apply_redex { redex_stack = stack; redex_redex = redex } addrs term terms =
      let gstack = Array.create (Array.length stack) StackVoid in
         match_redex addrs gstack term terms redex;
         gstack

   let apply_redex' { redex_stack = stack; redex_redex = redex } addrs term terms =
      let gstack = Array.create (Array.length stack) StackVoid in
         match_redex addrs gstack term terms redex;
         gstack, extract_redex_values gstack stack

   (*
    * Build a contractum from the spec and a stack.
    *)
   let make_contractum { con_contractum = con } gstack =
      build_contractum [||] [] gstack con

   (*
    * Compile redex and contractum, and form a rewrite rule.
    *)
   let term_rewrite strict (addrs, names) redex contracta =
      let stack, redex' = compile_so_redex strict addrs redex in
      let enames, contracta' = compile_so_contracta strict names stack contracta in
         { rr_redex = redex';
           rr_contractum = RWCTerm (contracta', enames);
           rr_gstacksize = Array.length stack
         }

   (*
    * Make a ML function rewrite.
    *)
   let fun_rewrite strict redex f =
      let stack, redex' = compile_so_redex strict [||] [redex] in
         { rr_redex = redex';
           rr_contractum = RWCFunction f;
           rr_gstacksize = Array.length stack
         }

   (*
    * Compile just the redex.
    *)
   let compile_redices strict addrs redices =
      let stack, redices = compile_so_redex strict addrs redices in
         { redex_stack = stack; redex_redex = redices }

   let compile_redex strict addrs redex =
      let redex = compile_redices strict addrs [redex] in
         match redex.redex_redex with
            [_] ->
               redex
          | _ ->
               failwith "compile_redex: too many redices"

   (*
    * Compile a contractum, given the previous redex.
    *)
   let compile_contractum strict { redex_stack = stack } contractum =
      let enames, contractum = compile_so_contractum strict [||] stack contractum in
         { con_contractum = contractum;
           con_new_vars = enames
         }
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
