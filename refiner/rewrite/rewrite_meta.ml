(*
 * Meta operations on rewrites.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

INCLUDE "refine_error.mlh"

open Printf
open Mp_debug
open Opname
open Term_sig
open Term_base_sig
open Term_man_sig
open Refine_error_sig

open Rewrite_type_sig

module MakeRewriteMeta
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
    with type object_id = TermType.object_id)
   (TermMan : TermManSig
    with type term = TermType.term
    with type level_exp = TermType.level_exp
    with type esequent = TermType.esequent)
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
   (RewriteTypes : RewriteTypesSig
    with type operator = TermType.operator
    with type level_exp = TermType.level_exp
    with type object_id = TermType.object_id)
   =
struct
   open TermType
   open Term
   open TermMan
   open RefineError
   open RewriteTypes

   (*
    * See if an operator generalizes another.
    *)
   let compare_levels a b =
      let { le_const = c; le_vars = vars } = dest_level a in
      let { rw_le_const = c'; rw_le_vars = vars' } = b in
      let rec prune o' = function
         { le_offset = o } as h :: t ->
            if o < o' then
               h :: prune o' t
            else
               prune o' t
       | [] ->
            []
      in
      let rec compare_level_vars notfound = function
         { rw_le_offset = o' } :: t ->
            compare_level_vars (prune o' notfound) t
       | [] ->
            notfound = []
      in
         not (c > c' && vars' = []) && compare_level_vars (List.map dest_level_var vars) vars'

   let compare_params p rwp =
      match dest_param p, rwp with
         Number n, RWNumber rn ->
            n = rn
       | String s, RWString rs
       | Token s, RWToken rs
       | Var s, RWVar rs ->
            s = rs
       | MNumber _, RWNumber _
       | MNumber _, RWMNumber _
       | MString _, RWString _
       | MString _, RWMString _
       | MToken _, RWToken _
       | MToken _, RWMToken _
       | MLevel _, RWMLevel1 _
       | MVar _, RWVar _
       | MVar _, RWMVar _ ->
            true
       | MLevel a, RWMLevel2 b ->
            compare_levels a b
       | _ -> false

   let compare_param_lists = List_util.for_all2 compare_params

   let relevant_operator op rw =
      match dest_op op, rw with
         { op_name = name1; op_params = params1 },
         { rw_name = name2; rw_params = params2 } ->
            name1 = name2 & compare_param_lists params1 params2

   (*
    * See if the bterms can be described by these arities.
    *)
   let rec relevant_bterms = function
      arity::arities, { rw_bvars = vars }::t ->
         if vars = arity then
            relevant_bterms (arities, t)
         else
            false
    | [], [] -> true
    | _ -> false

   (*
    * See if a rule is relevant to a term description.
    *)
   let relevant_rule op1 arities = function
      { rr_redex = (RWComposite { rw_op = op2; rw_bterms = bterms })::_ } ->
         if relevant_operator op1 op2 then
            relevant_bterms (arities, bterms)
         else
            false
     | _ -> false

   (*
    * Get the operator of a rewrite rule.
    *)
   let convert_level { rw_le_const = c; rw_le_vars = vars } =
      let convert { rw_le_var = v; rw_le_offset = o } =
         mk_level_var ("v" ^ string_of_int v) o
      in
         mk_level c (List.map convert vars)

   let rec convert_param' = function
      RWNumber i -> Number(i)
    | RWString s -> String s
    | RWToken t -> Token t
    | RWVar v -> Var v
    | RWMNumber i -> MNumber ("v" ^ (string_of_int i))
    | RWMString i -> MString ("v" ^ (string_of_int i))
    | RWMToken i -> MToken ("v" ^ (string_of_int i))
    | RWMLevel1 i -> MLevel (mk_var_level_exp ("v" ^ (string_of_int i)))
    | RWMLevel2 l -> MLevel (convert_level l)
    | RWMVar i -> MVar ("v" ^ (string_of_int i))
    | RWObId id -> ObId id
    | RWParamList l -> ParamList (List.map convert_param l)
   and convert_param p =
      make_param (convert_param' p)

   let rewrite_operator = function
      { rr_redex = (RWComposite { rw_op = { rw_name = name; rw_params = params } })::_ } ->
           mk_op name (List.map convert_param params)
    | _ ->
         REF_RAISE(RefineError ("rewrite_operator", RewriteNoRuleOperator))

   (*
    * Get the arities of the subterms.
    *)
   let bterm_eval_flags = function
      { rw_bvars = bvars; rw_bterm = bterm } ->
         bvars, (if bvars = 0 then
                     match bterm with
                        RWSOVar _ ->
                           true
                      | _ ->
                           false
                  else
                     false)

   let rewrite_eval_flags = function
      { rr_redex = (RWComposite { rw_bterms = bterms })::_ } ->
         List.map bterm_eval_flags bterms
    | _ ->
         REF_RAISE(RefineError ("rewrite_eval_flags", RewriteNoRuleOperator))
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
