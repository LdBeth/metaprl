(*
 * Manifest terms.
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Authors: Alexey Nogin
 *)

#include "refine_error.h"

open Printf
open Mp_debug

open Opname
open Refine_error_sig
open Term_ds_sig
open Term_ds
open Term_op_sig
open Term_addr_sig
open Term_subst_sig

open Term_addr_ds

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_man_gen%t" eflush

let debug_address =
   create_debug (**)
      { debug_name = "address";
        debug_description = "show term addressing operations";
        debug_value = false
      }

(*
 * Module builds on term implementation.
 *)
module TermMan (**)
   (Term : TermDsSig
    with type level_exp_var = TermType.level_exp_var
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type operator = TermType.operator
    with type term = TermType.term
    with type term_core = TermType.term_core
    with type bound_term = TermType.bound_term
    with type esequent = TermType.esequent
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals
    with type hypothesis = TermType.hypothesis

    with type level_exp_var' = TermType.level_exp_var'
    with type level_exp' = TermType.level_exp'
    with type object_id = TermType.object_id
    with type param' = TermType.param'
    with type operator' = TermType.operator'
    with type term' = TermType.term'
    with type bound_term' = TermType.bound_term')
   (TermOp : TermOpSig
    with type term = Term.term)
   (TermAddr : TermAddrSig
    with type term = Term.term
    with type address = Term_addr_ds.addr)
   (TermSubst : TermSubstSig
    with type term = Term.term
    with type param = Term.param)
   (RefineError : RefineErrorSig
    with type term = Term.term
    with type address = TermAddr.address) =
struct
   open Term
   open TermType
   open TermOp
   open TermAddr
   open TermSubst
   open RefineError

   type term = Term.term
   type operator = Term.operator
   type level_exp = Term.level_exp
   type address = TermAddr.address
   type esequent = TermType.esequent

   (************************************************************************
    * Level expressions                                                    *
    ************************************************************************)

   (* Simplified level expression constructors *)
   let mk_const_level_exp i =
      { le_const = i; le_vars = [] }

   let mk_var_level_exp v =
      { le_const = 0; le_vars = [{ le_var = v; le_offset = 0 }] }

   (*
    * Increment a level exp
    *)
   let incr_level_exp = function
      ({ le_const = c; le_vars = vars } : level_exp) ->
         let add1 = function
            { le_var = v; le_offset = o } ->
               { le_var = v; le_offset = o + 1 }
         in
            { le_const = c + 1; le_vars = List.map add1 vars }

   (*
    * Build a level expression out of the max of two level
    * expressions.
    *)
   let max_level_exp
      ({ le_const = c1; le_vars = l1 } : level_exp)
      ({ le_const = c2; le_vars = l2 } : level_exp)
      o3 =
         (* Max of two expressions; sort the variables *)
         let rec join = function
            ({ le_var = v1; le_offset = o1 } as h1::t1 as l1),
            ({ le_var = v2; le_offset = o2 } as h2::t2 as l2) ->
               if v1 = v2 then
                  { le_var = v1; le_offset = max o1 (o2 + o3) } :: join (t1, t2)
               else if v1 < v2 then
                  h1 :: join (t1, l2)
               else if o3 = 0 then
                  h2 :: join (l1, t2)
               else
                  { le_var = v2; le_offset = o2 + o3 } :: join (l1, t2)
          | [], l2 ->
               if o3 = 0 then
                  l2
               else
                  let add_off { le_var = v2; le_offset = o2 } =
                     { le_var = v2; le_offset = o2 + o3 }
                  in
                     List.map add_off l2
          | l1, [] ->
               l1
         in
            { le_const = max c1 (c2 + o3); le_vars = join (l1, l2) }


   (*
    * See if the first level is contained in the second.
    *)
   let level_le = fun
      { le_const = const1; le_vars = vars1 }
      { le_const = const2; le_vars = vars2 } ->
         let rec caux = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            { le_var = v2; le_offset = o2 }::t2 ->
               if v1 = v2 then
                  if o1 <= o2 then
                     caux (t1, t2)
                  else
                     false
               else if v2 < v1 then
                  caux (l1, t2)
               else
                  false
          | [], _ -> true
          | _, [] -> false
         in
            if const1 <= const2 then
               caux (vars1, vars2)
            else
               false

   let level_lt = fun
      { le_const = const1; le_vars = vars1 }
      { le_const = const2; le_vars = vars2 } ->
         let rec caux = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            { le_var = v2; le_offset = o2 }::t2 ->
               if v1 = v2 then
                  if o1 < o2 then
                     caux (t1, t2)
                  else
                     false
               else if v2 < v1 then
                  caux (l1, t2)
               else
                  false
          | [], _ ->
               true
          | _, [] ->
               false
         in
            if const1 < const2 then
               caux (vars1, vars2)
            else
               false

   (************************************************************************
    * PRIMITIVE FORMS                                                      *
    ************************************************************************)

   (*
    * Lists.
    *)
   let xnil_opname = mk_opname "nil" xperv
   let xcons_opname = mk_opname "cons" xperv

   let xnil_term = mk_simple_term xnil_opname []
   let is_xnil_term = is_no_subterms_term xnil_opname

   let is_xcons_term = is_dep0_dep0_term xcons_opname
   let mk_xcons_term = mk_dep0_dep0_term xcons_opname
   let dest_xcons = dest_dep0_dep0_term xcons_opname

   let rec is_xlist_term t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = []; bterm = _ }; { bvars = []; bterm = b }]
         } -> Opname.eq opname xcons_opname
       | Term { term_op = { op_name = opname; op_params = [] }; term_terms = [] } -> Opname.eq opname xnil_opname
       | _ ->
            false

   let dest_xlist t =
      let rec aux trm =
         match dest_term trm with
            { term_op = { op_name = opname; op_params = [] };
              term_terms = [{ bvars = []; bterm = a };
                            { bvars = []; bterm = b }]
            } when Opname.eq opname xcons_opname ->
               a::(aux b)
          | { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when Opname.eq opname xnil_opname ->
               []
          | _ ->
               ref_raise(RefineError ("dest_xlist", TermMatchError (t, "not a list")))
      in
         aux t

   let rec mk_xlist_term = function
      h::t ->
         mk_term (**)
            { op_name = xcons_opname; op_params = [] }
            [mk_simple_bterm h; mk_simple_bterm (mk_xlist_term t)]
    | [] ->
         xnil_term

   (*
    * Strings.
    *)
   let string_opname = mk_opname "string" xperv

   let is_xstring_term t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [String _] };
                term_terms = []
         } when Opname.eq opname string_opname ->
            true
       | _ ->
            false

   let dest_xstring t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = []
         } when Opname.eq opname string_opname ->
            s
       | _ ->
            ref_raise(RefineError ("dest_xstring", TermMatchError (t, "not a string")))

   let mk_xstring_term s =
      let op = { op_name = string_opname; op_params = [String s] } in
         mk_term op []

   (*
    * String with one subterm.
    *)
   let is_xstring_dep0_term t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [String _] };
                term_terms = [{ bvars = [] }]
         } when Opname.eq opname string_opname ->
            true
       | _ ->
            false

   let dest_xstring_dep0_term t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [{ bvars = []; bterm = t }]
         } when Opname.eq opname string_opname ->
            s, t
       | _ ->
            ref_raise(RefineError ("dest_xstring_dep0_term", TermMatchError (t, "not a string")))

   let mk_xstring_dep0_term s t =
      let op = { op_name = string_opname; op_params = [String s] } in
         mk_term op [mk_simple_bterm t]

   (****************************************
    * LAMBDA                               *
    ****************************************)

   let xlambda_opname =
      mk_opname "lambda" xperv

   let mk_xlambda_term =
      mk_dep1_term xlambda_opname

   (*************************
    * Sequents              *                                              *
    *************************)

   let is_sequent_term t =
      match get_core t with
         Sequent _ -> true
       | _ -> false

   let mk_sequent_term = Term.mk_sequent_term

   let explode_sequent t =
      match get_core t with
         Sequent s ->
            s
       | _ ->
            ref_raise(RefineError ("Term_man_ds.explode_sequent", TermMatchError (t, "not a sequent")))

   let args_of_sequent t =
      match get_core t with
         Sequent s ->
            s.sequent_args
       | _ ->
            ref_raise(RefineError ("Term_man_ds.args_of_sequent", TermMatchError (t, "not a sequent")))

   (*
    * Find the address of the hyp. Numbers start with 1
    * We just check to make sure the address is valid.
    *)
   let nth_hyp_addr_name = "Term_man_ds.nth_hyp_addr"
   let nth_hyp_addr t n =
      if n <= 0 then
         ref_raise(RefineError (nth_hyp_addr_name, StringError "negative address"))
      else
         match get_core t with
            Sequent s ->
               if n <= SeqHyp.length s.sequent_hyps then
                  HypAddr (pred n)
               else
                  ref_raise(RefineError (nth_hyp_addr_name, TermMatchError (t, "not enough hyps")))
          | _ ->
               ref_raise(RefineError (nth_hyp_addr_name, TermMatchError (t, "not a sequent")))

   (*
    * Find the address of the conclusion. Numbers start with 1
    *)
   let nth_concl_addr_name = "Term_man_ds.nth_concl_addr"
   let nth_concl_addr t n =
      if n <= 0 then
         ref_raise(RefineError (nth_concl_addr_name, StringError "negative address"))
      else
         match get_core t with
            Sequent s ->
               if n <= SeqGoal.length s.sequent_goals then
                  GoalAddr (pred n)
               else
                  ref_raise(RefineError (nth_concl_addr_name, TermMatchError (t, "not enough hyps")))
          | _ ->
               ref_raise(RefineError (nth_concl_addr_name, TermMatchError (t, "not a sequent")))

   (*
    * Conclusion is number 0,
    * negative numbers index from last hyp towards first.
    *)
   let nth_clause_addr_name = "Term_man_ds.nth_clause_addr"
   let nth_clause_addr t i =
      match get_core t with
         Sequent s ->
            let hlen = SeqHyp.length s.sequent_hyps in
               if (i = 0) then
                  GoalAddr 0
               else if (i > 0) then
                  if i <= hlen then
                     HypAddr (pred i)
                  else
                     ref_raise(RefineError (nth_clause_addr_name, TermMatchError (t, "not enough hyps")))
               else if (-i) <= hlen then
                  HypAddr (hlen + i)
               else
                  ref_raise(RefineError (nth_clause_addr_name, TermMatchError (t, "not enough hyps for a negative addressing")))
       | _ ->
            ref_raise(RefineError (nth_clause_addr_name, TermMatchError (t, "not a sequent")))

   (*
    * Count the hyps.
    *)
   let num_hyps_name = "Term_man_ds.num_hyps"
   let rec num_hyps t =
      match t.core with
         Sequent s ->
            SeqHyp.length s.sequent_hyps
       | Subst _ ->
            let _ = get_core t in
            num_hyps t
       | _ ->
            ref_raise(RefineError (num_hyps_name, TermMatchError (t, "not a sequent")))

   (*
    * Addresses to be used in sequent rewriting.
    *)
   let hyp_range_addr t i =
      HypAddr i

   let hyp_indices_addr t i =
      let count = num_hyps t in
      if i < 0 then
         HypAddr (count + i), HypAddr ((-1) - i)
      else
         HypAddr (i - 1), HypAddr (count - i)

   let concl_range_addr t i =
      GoalAddr i

   (*
    * Fast access to hyp and concl. Counting starts from 1.
    *)
   let nth_hyp_name = "Term_man_ds.nth_hyp"
   let rec nth_hyp t i =
      match t.core with
         Sequent s ->
            if i <= 0 then
               ref_raise(RefineError (nth_hyp_name, StringError "negative address"))
            else
               let i = pred i in
                  if i < SeqHyp.length s.sequent_hyps then
                     match SeqHyp.get s.sequent_hyps i with
                        Hypothesis (v, t) ->
                           (v, t)
                      | Context _ ->
                           ref_raise(RefineError (nth_hyp_name, TermMatchError (t, "it's a context")))
                  else
                     ref_raise(RefineError (nth_hyp_name, TermMatchError (t, "not enough hyps")))
       | Subst _ ->
            let _ = get_core t in
            nth_hyp t i
       | _ ->
            ref_raise(RefineError (nth_hyp_name, TermMatchError (t, "not a sequent")))

   let nth_concl_name = "Term_man_ds.nth_concl"
   let nth_concl t i =
      if i <= 0 then
         ref_raise(RefineError (nth_concl_name, StringError "negative address"))
      else
         let i = pred i in
            match get_core t with
               Sequent s ->
                  if i < SeqGoal.length s.sequent_goals then
                     SeqGoal.get s.sequent_goals i
                  else
                     ref_raise(RefineError (nth_concl_name, TermMatchError (t, "not enough hyps")))
             | _ ->
                  ref_raise(RefineError (nth_concl_name, TermMatchError (t, "not a sequent")))

   (*
    * Collect the vars.
    *)
   let rec declared_vars_aux hyps i =
      if i < 0 then [] else
         let rem = declared_vars_aux hyps (pred i) in
            match SeqHyp.get hyps i with
               Hypothesis (v,_) -> v::rem
             | Context _ -> rem

   let declared_vars_name = "Term_man_ds.declared_vars"
   let declared_vars t =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
               declared_vars_aux hyps (SeqHyp.length s.sequent_hyps - 1)
       | _ ->
            ref_raise(RefineError (declared_vars_name, TermMatchError (t, "not a sequent")))

   (*
    * Get the number of the hyp with the given var.
    *)
   let get_decl_number_name = "Term_man_ds.get_decl_number"
   let get_decl_number t v =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
            let hlen = SeqHyp.length hyps in
            let rec aux i =
               if i = hlen then
                  ref_raise(RefineError (get_decl_number_name, TermMatchError (t, "declaration not found")))
               else
                  match SeqHyp.get hyps i with
                     Hypothesis (v',_) when v' = v ->
                        succ i
                   | _ ->
                        aux (succ i)
            in
               aux 0
       | _ ->
            ref_raise(RefineError (get_decl_number_name, TermMatchError (t, "not a sequent")))

   (*
    * See if a var is free in the rest of the sequent.
    *)
   let is_free_seq_var_name = "Term_man_ds.is_free_seq_var"
   let is_free_seq_var i v t =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
            let hlen = SeqHyp.length hyps in
            let rec is_free_hyp_var i =
               if i = hlen then
                  let goals = s.sequent_goals in
                  let rec is_free_concl_var i =
                     if i < 0 then
                        false
                     else
                        is_free_var v (SeqGoal.get goals i) || is_free_concl_var (pred i)
                  in
                     is_free_concl_var (SeqGoal.length goals - 1)
               else
                  (match SeqHyp.get hyps i with
                     Hypothesis (v',t) ->
                        is_free_var v t
                   | Context (v,ts) ->
                        List.exists (is_free_var v) ts)
                  || (is_free_hyp_var (succ i))
            in
               is_free_hyp_var i
       | _ ->
            ref_raise(RefineError (is_free_seq_var_name, TermMatchError (t, "not a sequent")))

   let replace_goal_name = "Term_man_ds.replace_goal"
   let replace_goal t goal =
      match get_core t with
         Sequent s ->
            mk_sequent_term {sequent_args = s.sequent_args; sequent_hyps = s.sequent_hyps; sequent_goals = SeqGoal.singleton goal}
       | _ ->
            ref_raise(RefineError (replace_goal_name, TermMatchError (t, "not a sequent")))

   (*
    * Rewrite
    *)
   let xrewrite_op = mk_opname "rewrite" xperv

   let is_xrewrite_term = is_dep0_dep0_term xrewrite_op
   let mk_xrewrite_term = mk_dep0_dep0_term xrewrite_op
   let dest_xrewrite = dest_dep0_dep0_term xrewrite_op

   (************************************************************************
    * Rewrite rules                                                        *
    ************************************************************************)

   (*
    * Build a redex.
    *)
   let construct_redex vars params terms =
      let t = mk_xlist_term (params @ terms) in
      let l = Array.length vars in
      let rec aux i =
         if i < l then
            mk_xlambda_term vars.(i) (aux (i + 1))
         else
            t
      in
         aux 0
end

