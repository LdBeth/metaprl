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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

INCLUDE "refine_error.mlh"

open String_set
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
   show_loading "Loading Rewrite_compile_redex%t"

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
   type strict = RewriteTypes.strict

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

   let new_bvar_item i v =
      (v, i)

   (* Determine if a term is a bound variable *)
   let is_bound_var bvars v =
      is_var_term v & List.mem_assoc (dest_var v) bvars

   let bvar_ind ((_:string),(i:int)) = i

   let rec compile_so_redex_term allow_so_patterns strict addrs stack bvars term =
      (* Check for variables and contexts *)
      if is_so_var_term term then
         let v, subterms = dest_so_var term in
            (* This is a first or second order variable *)
            if List.mem_assoc v bvars then
               (* This is a first order variable instance *)
               if subterms <> [] then
                  REF_RAISE(RefineError ("compile_so_redex_term", RewriteBoundSOVar v))
               else
                  stack, RWCheckVar(svar_index bvars v)

            else if allow_so_patterns && List.for_all (is_bound_var bvars) subterms && not (rstack_pattern_mem v stack) then
               (* This is a second order variable, all subterms are vars *
                * and we do not have a pattern yet                       *)
               let so_mem = rstack_so_mem v stack in
               if so_mem then rstack_check_arity v (List.length subterms) stack;
               let stack = if so_mem then rstack_upgrade v stack else
                  if subterms = [] then stack @ [FOVarPattern v] else
                     stack @ [SOVarPattern (v, List.length subterms)] in
               let v = rstack_so_index v stack in
               let term, restrict_free =
                  if subterms = [] then
                     RWSOVar(v, []),
                     if strict then List.map bvar_ind bvars else []
                  else
                     let args = List.map (var_index bvars) subterms in
                     RWSOVar(v, args),
                     if strict then List_util.subtract (List.map bvar_ind bvars) args else []
               in
                  if restrict_free = [] then stack, term else stack, RWFreeVars(term,restrict_free)

            (* This is a second order variable instance *)
            else if rstack_so_mem v stack then begin
               rstack_check_arity v (List.length subterms) stack;
               (* we set allow_so_patterns to false here since the term that is going to
                  match the SO variable v may have no free occurences of this argument
                  and this would prevent us from establishing an SO pattern *)
               let stack, terms = compile_so_redex_terms false strict addrs stack bvars subterms in
               stack, RWSOMatch(rstack_so_index v stack, terms)
            end else
               let stack' = (stack @ [SOVarInstance (v, List.length subterms)]) in
               let stack', terms =
                  compile_so_redex_terms false strict addrs stack' bvars subterms in
               stack', RWSOMatch(List.length stack, terms)

      else if is_context_term term then
         let v, term, vars = dest_context term in
            if rstack_mem v stack then
               (* The context should have a unique name *)
               REF_RAISE(RefineError ("is_context_term", RewriteBoundSOVar v))

            else if Array_util.mem v addrs then
               (* All the vars should be free variables *)
               let vars' = List.map (var_index bvars) vars in
               let stack' = stack @ [CVar v] in
               let stack'', term' = compile_so_redex_term allow_so_patterns strict addrs stack' bvars term in
               let term'' = RWSOContext(Array_util.index v addrs,
                                        List.length stack,
                                        term', vars') in
               let restrict_free = if strict then List_util.subtract (List.map bvar_ind bvars) vars' else [] in
                  stack'', if restrict_free = [] then term'' else
                     (* RWFreeVars(term'',restrict_free) *)
                     raise (Invalid_argument "compile_so_redex_term: free variable restrictions on SO contexts are not implemented")
            else
               (* No argument for this context *)
               REF_RAISE(RefineError ("is_context_term", RewriteMissingContextArg v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         compile_so_redex_sequent strict addrs stack bvars term

      else
         (* This is normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let stack2, params2 = compile_so_redex_params stack params in
         let stack3, bterms3 = compile_so_redex_bterms allow_so_patterns strict addrs stack2 bvars bterms in
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
       | _ -> REF_RAISE(RefineError ("compile_so_redex_param", RewriteBadMatch (ParamMatch param)))

   (*
    * In bterms, have to add these vars to the binding stack.
    *)
   and compile_so_redex_bterms allow_so_patterns strict addrs stack bvars = function
      [] ->
         stack, []
    | bterm::bterms ->
         let stack', bterm' = compile_so_redex_bterm allow_so_patterns strict addrs stack bvars bterm in
         let stack'', bterms' = compile_so_redex_bterms allow_so_patterns strict addrs stack' bvars bterms in
            stack'', bterm'::bterms'

   (* HACK! This is not absolutely safe - we may accidentally "capture" an SO variable *)
   and rename_repeated_vars i stack bnames bvars term = function
      [] ->
         stack,bnames,bvars,term
    | v::vs ->
         let v',term' =
            if rstack_mem v stack then
               let fvs = free_vars_set term in
               let v' = String_util.vnewname v (fun v -> (rstack_mem v stack) || (StringSet.mem fvs v)) in
               v', subst1 term v (mk_var_term v')
            else v,term
         in
         rename_repeated_vars (succ i) (stack @ [FOVar v']) (bnames @ [bname i v']) (bvars @ [new_bvar_item i v']) term' vs

   and compile_so_redex_bterm allow_so_patterns strict addrs stack bvars bterm =
      let { bvars = vars; bterm = term } = dest_bterm bterm in
      let stack', bnames, bvars', term' = rename_repeated_vars (List.length stack) stack [] bvars term vars in
      (* Compile the term *)
      let stack'', term'' = compile_so_redex_term allow_so_patterns strict addrs stack' bvars' term' in
         stack'', { rw_bvars = List.length vars; rw_bnames = bnames; rw_bterm = term'' }

   (*
    * The contexts are handled differently within sequents.
    * Each context refers to a subrange of hypotheses or goals.
    *)
   and compile_so_redex_sequent strict addrs stack bvars term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
      let stack, arg = compile_so_redex_term true strict addrs stack bvars arg in
      let stack, hyps, goals =
         compile_so_redex_sequent_inner strict addrs stack bvars 0 (SeqHyp.length hyps) hyps goals in
         stack, RWSequent (arg, hyps, goals)

   and compile_so_redex_sequent_inner strict addrs stack bvars i len hyps goals =
      if i = len then
         let stack, goals =
            compile_so_redex_goals strict addrs stack bvars 0 (SeqGoal.length goals) goals
         in
            stack, [], goals
      else
         match SeqHyp.get hyps i with
            Context (v, vars) ->
               if rstack_mem v stack then
                  (* The context should have a unique name *)
                  REF_RAISE(RefineError ("is_context_term", RewriteBoundSOVar v))

               else if Array_util.mem v addrs then
                  (* All the vars should be free variables *)
                  let vars' = List.map (var_index bvars) vars in
                  let stack = stack @ [CVar v] in
                  let restrict_free = if strict then List_util.subtract (List.map bvar_ind bvars) vars' else [] in
                  let term =
                     if restrict_free = [] then
                        RWSeqContext (Array_util.index v addrs,
                                      List.length stack - 1,
                                      vars')
                     else
                        RWSeqFreeVarsContext (restrict_free,
                                              Array_util.index v addrs,
                                              List.length stack - 1,
                                              vars')
                  in
                  let stack, hyps, goals =
                     compile_so_redex_sequent_inner strict addrs stack bvars (i + 1) len hyps goals
                  in
                     stack, term :: hyps, goals
               else
                  (* No argument for this context *)
                  REF_RAISE(RefineError ("compile_so_redex_sequent_inner", RewriteMissingContextArg v))

          | Hypothesis (v, term) ->
               if rstack_mem v stack then
                  REF_RAISE(RefineError ("compile_so_redex_sequent_inner", StringStringError ("repeated variable", v)));
               let stack, term = compile_so_redex_term true strict addrs stack bvars term in
               let l = List.length stack in
               let stack = stack @ [FOVar v] in
               let bvars = bvars @ [new_bvar_item l v] in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner strict addrs stack bvars (i + 1) len hyps goals
               in
                  stack, RWSeqHyp (bname l v, term) :: hyps, goals

   and compile_so_redex_goals strict addrs stack bvars i len goals =
      if i = len then
         stack, []
      else
         let stack, goal = compile_so_redex_term true strict addrs stack bvars (SeqGoal.get goals i) in
         let stack, goals = compile_so_redex_goals strict addrs stack bvars (i + 1) len goals in
            stack, goal :: goals

   and compile_so_redex_terms allow_so_patterns strict addrs stack bvars = function
      [] ->
         stack, []
    | term::t ->
         let stack', term' = compile_so_redex_term allow_so_patterns strict addrs stack bvars term in
         let stack'', terms' = compile_so_redex_terms allow_so_patterns strict addrs stack' bvars t in
            stack'', term'::terms'

   let check_stack = function
      SOVarInstance (n, _) ->
         REF_RAISE(RefineError ("check_stack", RewriteAllSOInstances n))
    | _ ->
         ()

   let compile_so_redex strict addrs terms =
      let stack, terms = compile_so_redex_terms true (strict=Strict) addrs [] [] terms in
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
