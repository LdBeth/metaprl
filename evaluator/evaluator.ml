(*
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)

open Printf
open Mp_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermEval

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Evaluator%t" eflush

type eval_rule = term  * (param' list -> term)

let (evaltable : (shape, eval_rule) Hashtbl.t) = Hashtbl.create 499

let rec check_lhs_bvars = function
   [] -> () |
   h::t ->
   if List.mem h t
      then raise (Invalid_argument "All bound variables should be different in lhs of reduction rules")
      else check_lhs_bvars t

let rec get_arg_bvars = function
   [] -> [] |
   h::t ->
      let hd = dest_bterm h in
      hd.bvars @ (get_term_bvars hd.bterm @ get_arg_bvars t)

and get_term_bvars trm = get_arg_bvars (dest_term trm).term_terms

let check_lhs lhs = check_lhs_bvars (get_term_bvars lhs)

let add_new s (t,f) =
   check_lhs t;
   Hashtbl.add evaltable s (t,f)

let add_reduction_rule t f = add_new (shape_of_term t) (t,f)
let add_abstraction lhs rhs = add_reduction_rule lhs  (function pl -> rhs)
let add_simple_rule = add_abstraction

let eval_subst term =
   let sterm = dest_subst term in
   let substitution = snd sterm in
   subst (fst sterm) (snd substitution) (fst substitution)

let rec add_prin_args prin_list arglist =
   match arglist with
      [] -> prin_list |
      h::t -> ( match prin_list with
         [] -> not(is_var_term (dest_bterm h).bterm)::add_prin_args [] t |
         ha::ta -> (ha & not(is_var_term (dest_bterm h).bterm))::add_prin_args ta t )

let rec get_prin_args = function
   [] -> [] |
   h::t -> add_prin_args (get_prin_args t) (dest_term(fst h)).term_terms

let rec check_rule_args rule_args args = function
   [] -> check_rule_args rule_args args [true;true;true;true;true] |
   h::t ->
   begin
      match (args,rule_args) with
      [],[] -> true |
      ha::ta,hr::tr ->
         (if h then
            let r_term = (dest_bterm hr).bterm in
            is_var_term r_term
               or is_canon_var_term r_term
               or let ha_term = (dest_bterm ha).bterm in
                  (shape_of_term r_term = shape_of_term ha_term
                     & check_rule (dest_term ha_term).term_terms [] r_term )
         else true)
            & check_rule_args tr ta t |
      _ -> false
   end

(*
 * prin_args = [] - all arguments are principal
 *)
and check_rule args prin_args rule_term =
   check_rule_args (dest_term rule_term).term_terms args prin_args

let rec get_arg_par_list = function
   arg::args, rule_arg::rule_args ->
      let r_term = (dest_bterm rule_arg).bterm in
      ( if is_var_term r_term or is_canon_var_term r_term then [] else
         get_par_list (dest_term (dest_bterm arg).bterm) (dest_term r_term) )
      @ get_arg_par_list (args,rule_args) |
   _ -> []

and get_par_list trm rule_trm =
   (dest_op trm.term_op).op_params @ get_arg_par_list (trm.term_terms,rule_trm.term_terms)

let add_substs (bvs1,vs1) (bvs2,vs2) = (bvs1@bvs2,vs1@vs2)

let rec get_term_substs term lhs =
   if is_var_term lhs then
      let v = dest_var lhs in
      ([],[v,term])
   else if is_canon_var_term lhs then
      let v = dest_canon_var lhs in
      ([],[v,term])
   else get_args_substs ((dest_term term).term_terms,(dest_term lhs).term_terms)

and get_args_substs = function
   [],[] -> [],[] |
   ht::tt, hr::tr ->
      let htd = dest_bterm ht in
      let hrd = dest_bterm hr in
      add_substs (List.combine hrd.bvars htd.bvars,[]) (add_substs (get_term_substs htd.bterm hrd.bterm) (get_args_substs (tt,tr))) |
   _ -> raise (Failure "Bug in the evaluator: get_args_substs got wrong arguments")

let var_assoc v vvsub =
   try List.assoc v vvsub with
   Not_found -> v

let vterm_assoc v vtsub =
   try List.assoc v vtsub with
   Not_found -> mk_var_term v

let rec make_substs bvsub vsub term =
   if is_var_term term then vterm_assoc (dest_var term) vsub else
   let trm = dest_term term in
   make_term {term_op = trm.term_op; term_terms = make_arg_substs bvsub vsub trm.term_terms}

and make_arg_substs bvsub vsub = function
   [] -> [] |
   h::t ->
      let hd = dest_bterm h in
      make_bterm {
         bvars=List.map (function v -> var_assoc v bvsub) hd.bvars;
         bterm = make_substs bvsub vsub hd.bterm
      } :: make_arg_substs bvsub vsub t


let do_reduce term rule_lhs rule_rhs =
   let substs = get_term_substs term rule_lhs in
   let bvsub = fst substs in
   let vsub = snd substs in
   make_substs bvsub vsub rule_rhs

let apply_matching_rule term rule =
   let rule_lhs = fst rule in
   let rule_func = snd rule in
   let trm = dest_term term in
   let rule_rhs = rule_func (List.map dest_param (get_par_list trm (dest_term rule_lhs))) in
   do_reduce term rule_lhs rule_rhs

let rec eval_prin_args args prin_args =
   match args with
      [] -> [] |
      ha::ta -> (match prin_args with
         [] -> args |
         hp::tp ->
            (if hp then
               let arg=dest_bterm ha in
               mk_bterm arg.bvars (evaluate_term arg.bterm)
             else ha) :: eval_prin_args ta tp)

and eval_term_with_rules term rules =
   match rules with
   [] -> term,false |
   hrule :: trules ->
      let prin_args = get_prin_args rules in
      let trm = dest_term term in
      let args = eval_prin_args trm.term_terms prin_args in
      let term = make_term {term_op = trm.term_op; term_terms = args}  in
      let new_rules = List_util.filter (function r -> check_rule args prin_args (fst r)) rules in
      if List.length rules != List.length new_rules then eval_term_with_rules term new_rules else
      if trules = [] then (apply_matching_rule term hrule),true else
      let try_first = eval_term_with_rules term (hrule::[]) in
      if snd try_first then try_first else eval_term_with_rules (fst try_first) trules

and evaluate_term term =
   if is_subst_term term then evaluate_term (eval_subst term) else
   let shape = shape_of_term term in
   let rules = Hashtbl.find_all evaltable shape in
   let eval = eval_term_with_rules term rules in
   let new_term = fst eval in
   if snd eval then evaluate_term new_term else new_term

(*
 *let del_all s =
 *   let del_it s
 *let del_reduction_rule = Hashtbl.remove evaltable
 *let del_abstraction t = Hashtbl.remove evaltable (shape_of_term t)
 *)
