(*
 * Compile a term with second order variables into
 * a rewrite term.
 *
 * The stack contains a list of variable names.
 * All second order variables and first order
 * binding variables go on the stack.
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

#include "refine_error.h"

open Printf
open Mp_debug
open Opname
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Refine_error_sig

open Rewrite_type_sig
open Rewrite_util_sig
open Rewrite_debug_sig

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Rewrite_debug%t" eflush

module MakeRewriteCompileRedex
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
    with type esequent = TermType.esequent)
   (TermAddr : TermAddrSig
    with type term = TermType.term)
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
   (RewriteTypes : RewriteTypesSig
    with type level_exp = TermType.level_exp
    with type term = TermType.term
    with type address = TermAddr.address)
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = RewriteTypes.rstack)
   =
struct
   open TermType
   open Term
   open TermMan
   open TermAddr
   open TermSubst
   open RefineError
   open RewriteTypes
   open RewriteUtil

   type term = TermType.term
   type rstack = RewriteTypes.rstack
   type rwterm = RewriteTypes.rwterm

   (*
    *
    * The bvars is an association list of the binding
    * variables at the current point being compiled, mapping
    * binding names to stack locations.
    *
    * Return the stack and the compiled term.
    *)
   let bname i _ =
      StackName i

   let rec bnames i = function
      h::t -> (StackName i)::(bnames (i + 1) t)
    | [] -> []

   (* Add new bvars *)
   let new_bvar_item i v =
      (v, i)

   let rec new_bvar_items i = function
      h::t -> (h, i)::(new_bvar_items (i + 1) t)
    | [] -> []

   (* Determine if a term is a bound variable *)
   let is_bound_var bvars v =
      is_var_term v & List.mem_assoc (dest_var v) bvars

   (* Find the free variables and their indices *)
   let gen_subterms bvars subterms =
      let fv = List_util.intersect (free_vars_terms subterms) (List_util.fst_split bvars) in
         List.map (svar_index bvars) fv, fv, subterms

   let rec compile_so_redex_term addrs stack bvars term =
      (* Check for variables and contexts *)
      if is_so_var_term term then
         let v, subterms = dest_so_var term in
            (* This is a first or second order variable *)
            if List.mem_assoc v bvars then
               (* This is a first order variable instance *)
               if subterms <> [] then
                  ref_raise(RefineError ("compile_so_redex_term", RewriteBoundSOVar v))
               else
                  stack, RWCheckVar(svar_index bvars v)

            else if List.for_all (is_bound_var bvars) subterms then
               (* This is a second order variable, and all subterms are vars *)
               if rstack_so_mem v stack then
                  (* Treat this as an instance, but record that a pattern was found *)
                  let _ = rstack_check_arity v (List.length subterms) stack in
                     (rstack_upgrade v stack),
                     RWSOMatch(rstack_so_index v stack, gen_subterms bvars subterms)
               else if subterms = [] then
                  (stack @ [FOVarPattern v]),
                  RWSOVar(List.length stack, [])
               else
                  (stack @ [SOVarPattern (v, List.length subterms)]),
                  RWSOVar(List.length stack, List.map (var_index bvars) subterms)

            (* This is a second order variable instance *)
            else if rstack_so_mem v stack then
               let _ = rstack_check_arity v (List.length subterms) stack in
                  stack, RWSOMatch(rstack_so_index v stack, gen_subterms bvars subterms)
            else
               (stack @ [SOVarInstance (v, List.length subterms)]),
               RWSOMatch(List.length stack, gen_subterms bvars subterms)

      else if is_context_term term then
         let v, term, vars = dest_context term in
            if rstack_mem v stack then
               (* The context should have a unique name *)
               ref_raise(RefineError ("is_context_term", RewriteBoundSOVar v))

            else if Array_util.mem v addrs then
               (* All the vars should be free variables *)
               let stack' = stack @ [CVar v] in
               let stack'', term' = compile_so_redex_term addrs stack' bvars term in
                  stack'', RWSOContext(Array_util.index v addrs,
                                       List.length stack,
                                       term',
                                       List.map (var_index bvars) vars)
            else
               (* No argument for this context *)
               ref_raise(RefineError ("is_context_term", RewriteMissingContextArg v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         compile_so_redex_sequent addrs stack bvars term

      else
         (* This is normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let stack2, params2 = compile_so_redex_params stack params in
         let stack3, bterms3 = compile_so_redex_bterms addrs stack2 bvars bterms in
            stack3, RWComposite { rw_op = { rw_name = name; rw_params = params2 };
                                  rw_bterms = bterms3 }

   (*
    * We also compile parameters, and bind meta-variables.
    *)
   and compile_so_redex_params stack = function
      [] ->
         stack, []
    | param::params ->
         (* Do this param *)
         let stack', param' = compile_so_redex_param stack param in
         let stack'', params' = compile_so_redex_params stack' params in
            stack'', param'::params'

   and meta_param' stack pvar v =
      if rstack_p_mem v stack then
         (* This param is not free, do a match *)
         stack, rstack_p_index v stack
      else
         (* Add it *)
         stack @ [pvar v], List.length stack

   and meta_param stack const pvar v =
      let stack, v = meta_param' stack pvar v in
         stack, const v

   and meta_level stack l =
      let { le_const = c; le_vars = vars } = dest_level l in
      let vars = List.map dest_level_var vars in
         match c, vars with
            0, [{ le_var = v; le_offset = 0 }] ->
               meta_param stack (fun l -> RWMLevel1 l) (fun l -> PLVar l) v
          | _ ->
               let rec collect stack = function
                  { le_var = v; le_offset = i } :: tl ->
                     let stack, v = meta_param' stack (fun l -> PLVar l) v in
                     let v = { rw_le_var = v; rw_le_offset = i } in
                     let stack, vars = collect stack tl in
                        stack, v :: vars
                | [] ->
                     stack, []
               in
               let stack, vars = collect stack vars in
                  stack, RWMLevel2 { rw_le_const = c; rw_le_vars = vars }

   and compile_so_redex_param stack param =
      match dest_param param with
         MNumber v -> meta_param stack (fun i -> RWMNumber i) (fun v -> PIVar v) v
       | MString v -> meta_param stack (fun s -> RWMString s) (fun s -> PSVar s) v
       | MToken v -> meta_param stack (fun t -> RWMToken t) (fun s -> PSVar s) v
       | MLevel l -> meta_level stack l
       | MVar v -> meta_param stack (fun v -> RWMVar v) (fun v -> PSVar v) v
       | Number i -> stack, RWNumber i
       | String s -> stack, RWString s
       | Token t -> stack, RWToken t
       | Var v -> stack, RWVar v
       | _ -> ref_raise(RefineError ("compile_so_redex_param", RewriteBadMatch (ParamMatch param)))

   (*
    * In bterms, have to add these vars to the binding stack.
    *)
   and compile_so_redex_bterms addrs stack bvars = function
      [] ->
         stack, []
    | bterm::bterms ->
         let stack', bterm' = compile_so_redex_bterm addrs stack bvars bterm in
         let stack'', bterms' = compile_so_redex_bterms addrs stack' bvars bterms in
            stack'', bterm'::bterms'

   and compile_so_redex_bterm addrs stack bvars bterm =
      let { bvars = vars; bterm = term } = dest_bterm bterm in
      let stack' = stack @ (List.map (fun v -> FOVar v) vars) in
      let l = List.length stack in
      let bvars' = bvars @ (new_bvar_items l vars) in

      (* Compile the term *)
      let stack'', term' = compile_so_redex_term addrs stack' bvars' term in
         stack'', { rw_bvars = List.length vars; rw_bnames = bnames l vars; rw_bterm = term' }

   (*
    * The contexts are handled differently within sequents.
    * Each context refers to a subrange of hypotheses or goals.
    *)
   and compile_so_redex_sequent addrs stack bvars term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
      let stack, arg = compile_so_redex_term addrs stack bvars arg in
      let stack, hyps, goals =
         compile_so_redex_sequent_inner addrs stack bvars 0 (SeqHyp.length hyps) hyps goals in
         stack, RWSequent (arg, hyps, goals)

   and compile_so_redex_sequent_inner addrs stack bvars i len hyps goals =
      if i = len then
         let stack, goals =
            compile_so_redex_goals addrs stack bvars 0 (SeqGoal.length goals) goals
         in
            stack, [], goals
      else
         match SeqHyp.get hyps i with
            Context (v, vars) ->
               if rstack_mem v stack then
                  (* The context should have a unique name *)
                  ref_raise(RefineError ("is_context_term", RewriteBoundSOVar v))

               else if Array_util.mem v addrs then
                  (* All the vars should be free variables *)
                  let stack = stack @ [CVar v] in
                  let term =
                     RWSeqContext (Array_util.index v addrs,
                                   List.length stack - 1,
                                   List.map (var_index bvars) vars)
                  in
                  let stack, hyps, goals =
                     compile_so_redex_sequent_inner addrs stack bvars (i + 1) len hyps goals
                  in
                     stack, term :: hyps, goals
               else
                  (* No argument for this context *)
                  ref_raise(RefineError ("is_context_term", RewriteMissingContextArg v))

          | Hypothesis (v, term) ->
               let stack, term = compile_so_redex_term addrs stack bvars term in
               let l = List.length stack in
               let stack = stack @ [FOVar v] in
               let bvars = bvars @ [new_bvar_item l v] in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner addrs stack bvars (i + 1) len hyps goals
               in
                  stack, RWSeqHyp (bname l v, term) :: hyps, goals

   and compile_so_redex_goals addrs stack bvars i len goals =
      if i = len then
         stack, []
      else
         let stack, goal = compile_so_redex_term addrs stack bvars (SeqGoal.get goals i) in
         let stack, goals = compile_so_redex_goals addrs stack bvars (i + 1) len goals in
            stack, goal :: goals

   (*
    * This is the final version.
    *)
   let rec compile_so_redex_aux addrs stack = function
      [] ->
         stack, []
    | term::t ->
         let stack', term' = compile_so_redex_term addrs stack [] term in
         let stack'', terms' = compile_so_redex_aux addrs stack' t in
            stack'', term'::terms'

   let check_stack = function
      SOVarInstance (n, _) ->
         ref_raise(RefineError ("check_stack", RewriteAllSOInstances n))
    | _ ->
         ()

   let compile_so_redex addrs t =
      let stack, terms = compile_so_redex_aux addrs [] t in
         List.iter check_stack stack;
         Array.of_list stack, terms
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
