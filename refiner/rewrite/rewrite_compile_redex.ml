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

open Lm_debug
open Lm_symbol

open Lm_printf
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_shape_sig
open Refine_error_sig

open Rewrite_util_sig
open Rewrite_debug_sig
open Rewrite_types

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite_compile_redex%t"

module MakeRewriteCompileRedex
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (RefineError : RefineErrorSig with module ErrTypes.Types = TermType)
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack)
   (RewriteDebug : RewriteDebugSig
    with type rwterm = MakeRewriteTypes(TermType)(TermAddr).rwterm
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack
    with type varname = MakeRewriteTypes(TermType)(TermAddr).varname)
   =
struct
   module RewriteTypes = MakeRewriteTypes(TermType)(TermAddr)
   open TermType
   open Term
   open TermMan
   open TermSubst
   open RefineError
   open RewriteTypes
   open RewriteUtil

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

   (* Determine if all terms in a list are distinct bound vars *)
   let rec are_bound_vars bvars = function
      [] ->
         true
    | v::ts ->
         is_var_term v
            && let v = dest_var v in
               List.mem_assoc v bvars && are_bound_vars (List.remove_assoc v bvars) ts

   let bvar_ind ((_:var),(i:int)) = i

   (* Add an extra free variable restriction to a hypothesis *)
   let restrict_free_in_hyp v = function
      RWSeqContext (i, l, vs) -> RWSeqFreeVarsContext ([], [v], i, l, vs)
    | RWSeqFreeVarsContext (cs, rs, i, l, vs) -> RWSeqFreeVarsContext (cs, v::rs, i, l, vs)
    | RWSeqHyp (v', RWFreeVars(t, cs, rs)) -> RWSeqHyp (v', RWFreeVars(t, cs, v::rs))
    | RWSeqHyp (v', t) -> RWSeqHyp (v', RWFreeVars(t, [], [v]))
    | _ -> raise(Invalid_argument("Rewrite_compile_redex.restrict_free_in_hyp"))

   (* Add an extra free variable restriction to a term *)
   let restrict_free_in_term v = function
      RWFreeVars(t, rconts, rs) -> RWFreeVars(t, rconts, v::rs)
    | t -> RWFreeVars(t, [], [v])

   let rec restrict_cont c v = function
      [] -> REF_RAISE(RefineError ("rewrite_compile_redex", RewriteFreeContextVar(v,c)))
    | ((v', _) as hd) :: tl ->
         if Lm_symbol.eq v v' then tl else hd :: (restrict_cont c v tl)

   let rec restricted_conts c bconts = function
      [] -> List.map bvar_ind bconts
    | v :: tl -> restricted_conts c (restrict_cont c v bconts) tl

   let rec lastcontext hyps i =
      let i = pred i in
      if i < 0 then i else
         match SeqHyp.get hyps i with
            Context _ -> i
          | Hypothesis _ -> lastcontext hyps i

   let rec compile_so_redex_term allow_so_patterns restrict addrs stack svars bconts bvars term =
      (* Check for variables and contexts *)
      if is_var_term term then
         let v = dest_var term in
            if List.mem_assoc v bvars then
               stack, RWCheckVar(svar_index bvars v)
            else
               let stack =
                  let mem = rstack_freefo_mem v stack in
                  if not mem && rstack_mem v stack then
                     REF_RAISE(RefineError ("compile_so_redex_term", StringVarError("Free FO variable clashes with another variable ", v)));
                  if allow_so_patterns && not (rstack_pattern_mem v stack) then
                     if mem then rstack_upgrade v stack else stack @ [ FreeFOVarPattern v ]
                  else if mem then stack else stack @ [ FreeFOVarInstance v ]
               in
                  stack, RWMatchFreeFOVar(
                     rstack_freefo_index v stack,
                     (if restrict then List.map bvar_ind bconts else []),
                     (if restrict then List.map bvar_ind bvars else [])
                  )
      else if is_so_var_term term then
         let v, conts, subterms = dest_so_var term in
            if List.mem_assoc v bvars then
               REF_RAISE(RefineError ("compile_so_redex_term", RewriteBoundSOVar v))
            else if allow_so_patterns && are_bound_vars bvars subterms && not (rstack_pattern_mem v stack) then
               (* This is a second order variable, all subterms are vars *
                * and we do not have a pattern yet                       *)
               let so_mem = rstack_so_mem v stack in
               if so_mem then
                  rstack_check_arity v conts (List.length subterms) stack
               else
                  if rstack_mem v stack then REF_RAISE(RefineError ("compile_so_redex_term", RewriteBoundSOVar v));
               let stack = if so_mem then rstack_upgrade v stack else
                  stack @ [SOVarPattern (v, conts, List.length subterms)] in
               let v' = rstack_so_index v stack in
               let args = if subterms = [] then [] else List.map (var_index bvars) subterms in
               let term = RWSOVar(v', args) in
               let restrict_free =
                  if restrict then
                     let bvars = List.map bvar_ind bvars in
                        if args = [] then bvars else Lm_list_util.subtract bvars args
                  else []
               in
               let restrict_conts = if restrict then restricted_conts v bconts conts else [] in
                  stack, if restrict_free = [] && restrict_conts = [] then term
                         else RWFreeVars(term,restrict_conts,restrict_free)

            (* This is a second order variable instance *)
            else if rstack_so_mem v stack then begin
               rstack_check_arity v conts (List.length subterms) stack;
               (* we set allow_so_patterns to false here since the term that is going to
                  match the SO variable v may have no free occurences of this argument
                  and this would prevent us from establishing an SO pattern *)
               let stack, terms = compile_so_redex_terms false restrict addrs stack svars bconts bvars subterms in
               stack, RWSOInstance(rstack_so_index v stack, terms)
            end else
               let stack' = (stack @ [SOVarInstance (v, conts, List.length subterms)]) in
               let stack', terms =
                  compile_so_redex_terms false restrict addrs stack' svars bconts bvars subterms in
               stack', RWSOInstance(List.length stack, terms)

      else if is_context_term term then
         let v, term, conts, vars = dest_context term in
            if rstack_mem v stack then
               (* The context should have a unique name *)
               REF_RAISE(RefineError ("compile_so_redex_term", RewriteBoundSOVar v))

            else if Lm_array_util.mem v addrs then
               (* All the vars should be free variables *)
               let vars' = List.map (var_index bvars) vars in
               let index = List.length stack in
               let stack = stack @ [CVar (v, conts, List.length vars)] in
               let stack, term = compile_so_redex_term allow_so_patterns restrict addrs stack svars ((v,index)::bconts) bvars term in
               let term = RWSOContext(Lm_array_util.index v addrs, index, term, vars') in
               let restrict_free = if restrict then Lm_list_util.subtract (List.map bvar_ind bvars) vars' else [] in
               let restrict_conts = if restrict then restricted_conts v bconts conts else [] in
                  stack, if restrict_free = [] && restrict_conts = [] then term else
                     (* RWFreeVars(term,restrict_conts,restrict_free) *)
                     raise (Invalid_argument "compile_so_redex_term: free variable restrictions on SO contexts are not implemented")
            else
               (* No argument for this context *)
               REF_RAISE(RefineError ("compile_so_redex_term", RewriteMissingContextArg v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         compile_so_redex_sequent restrict addrs stack svars bconts bvars term

      else
         (* This is normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let stack2, params2 = compile_so_redex_params stack restrict params in
         let stack3, bterms3 = compile_so_redex_bterms allow_so_patterns restrict addrs stack2 svars bconts bvars bterms in
            stack3, RWComposite { rw_op = { rw_name = name; rw_params = params2 };
                                  rw_bterms = bterms3 }

   (*
    * We also compile parameters, and bind meta-variables.
    *)
   and compile_so_redex_params stack restrict = function
      [] ->
         stack, []
    | param::params ->
         (* Do this param *)
         let stack', param' = compile_so_redex_param stack restrict param in
         let stack'', params' = compile_so_redex_params stack' restrict params in
            stack'', param'::params'

   and meta_param' stack ptype v =
      if rstack_p_mem ptype v stack then
         (* This param is not free, do a match *)
         stack, rstack_p_index ptype v stack
      else begin
         if rstack_mem v stack then
            REF_RAISE(RefineError("meta_param", StringVarError("Parameter meta-variable has different meanings in context",v)));
         (* Add it *)
         stack @ [PVar (v, ptype)], List.length stack
      end

   and meta_param stack const ptype v =
      let stack, v = meta_param' stack ptype v in
         stack, const v

   and meta_level stack l =
      let { le_const = c; le_vars = vars } = dest_level l in
      let vars = List.map dest_level_var vars in
         match c, vars with
            0, [{ le_var = v; le_offset = 0 }] ->
               meta_param stack (fun l -> RWMLevel1 l) ShapeLevel v
          | _ ->
               let rec collect stack = function
                  { le_var = v; le_offset = i } :: tl ->
                     let stack, v = meta_param' stack ShapeLevel v in
                     let v = { rw_le_var = v; rw_le_offset = i } in
                     let stack, vars = collect stack tl in
                        stack, v :: vars
                | [] ->
                     stack, []
               in
               let stack, vars = collect stack vars in
                  stack, RWMLevel2 { rw_le_const = c; rw_le_vars = vars }

   and compile_so_redex_param stack restrict param =
      match dest_param param with
         MNumber v -> meta_param stack (fun i -> RWMNumber i) ShapeNumber v
       | MString v -> meta_param stack (fun s -> RWMString s) ShapeString v
       | MToken v -> meta_param stack (fun t -> RWMToken t) ShapeToken v
       | Var v when not restrict -> meta_param stack (fun v -> RWMVar v) ShapeVar v
       | MLevel l -> meta_level stack l
       | Number i -> stack, RWNumber i
       | String s -> stack, RWString s
       | Token t -> stack, RWToken t
       | Quote -> stack, RWQuote
       | _ -> REF_RAISE(RefineError ("compile_so_redex_param", RewriteBadMatch (ParamMatch param)))

   (*
    * In bterms, have to add these vars to the binding stack.
    *)
   and compile_so_redex_bterms allow_so_patterns restrict addrs stack svars bconts bvars = function
      [] ->
         stack, []
    | bterm::bterms ->
         let stack', bterm' = compile_so_redex_bterm allow_so_patterns restrict addrs stack svars bconts bvars bterm in
         let stack'', bterms' = compile_so_redex_bterms allow_so_patterns restrict addrs stack' svars bconts bvars bterms in
            stack'', bterm'::bterms'

   and rename_repeated_vars i stack bnames bvars = function
      [] ->
         stack,bnames,bvars
    | v::vs ->
         rename_repeated_vars (succ i) (stack @ [FOVar v]) (bnames @ [bname i v]) ((new_bvar_item i v) :: bvars) vs

   and compile_so_redex_bterm allow_so_patterns restrict addrs stack svars bconts bvars bterm =
      let svars = SymbolSet.add_list svars (List.map rstack_var stack) in
      let { bvars = vars; bterm = term } = dest_bterm_and_rename bterm svars in
      let stack', bnames, bvars' = rename_repeated_vars (List.length stack) stack [] bvars vars in
      (* Compile the term *)
      let stack'', term' = compile_so_redex_term allow_so_patterns restrict addrs stack' svars bconts bvars' term in
         stack'', { rw_bvars = List.length vars; rw_bnames = bnames; rw_bterm = term' }

   (*
    * The contexts are handled differently within sequents.
    * Each context refers to a subrange of hypotheses or goals.
    *)
   and compile_so_redex_sequent restrict addrs stack svars bconts bvars term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals;
          } = explode_sequent term
      in
      let stack, arg = compile_so_redex_term true restrict addrs stack svars bconts bvars arg in
      let l = SeqHyp.length hyps in
      let stack, hyps, goals =
         compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars 0 l (lastcontext hyps l) hyps goals in
         stack, RWSequent (arg, hyps, goals)

   and compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars i len mc hyps goals =
      if i = len then
         let stack, goals =
            compile_so_redex_goals restrict addrs stack svars bconts bvars 0 (SeqGoal.length goals) goals
         in
            stack, [], goals
      else
         match SeqHyp.get hyps i with
            Context (v, conts, terms) ->
               let instance = rstack_c_mem v stack in
               if not instance && rstack_mem v stack then
                  (* The context should have a unique name *)
                  REF_RAISE(RefineError ("compile_so_redex_term", RewriteBoundSOVar v))
               else
               let stack, term, ind =
                  if instance then begin
                     rstack_check_arity v conts (List.length terms) stack;
                     let stack, terms = compile_so_redex_terms false restrict addrs stack svars bconts bvars terms in
                     let ind = rstack_c_index v stack in
                        stack, RWSeqContextInstance (ind, terms), ind
                  end else
                     let index =
                        if i = mc then
                           if Lm_array_util.mem v addrs then
                              REF_RAISE(RefineError ("compile_so_redex_sequent_inner", StringVarError("Last context of the sequent does not need to be passed in as an argument",v)))
                           else
                              i - len
                        else
                           if Lm_array_util.mem v addrs then
                              Lm_array_util.index v addrs
                           else
                              REF_RAISE(RefineError ("compile_so_redex_sequent_inner", RewriteMissingContextArg v))
                     in
                     (* All the vars should be free variables *)
                     let vars' = List.map (var_index bvars) terms in
                     let stack = stack @ [CVar (v, conts, List.length terms)] in
                     let restrict_free = if restrict then Lm_list_util.subtract (List.map bvar_ind bvars) vars' else [] in
                     let restrict_conts = if restrict then restricted_conts v bconts conts else [] in
                     let stack_ind = List.length stack - 1 in
                     let term =
                        if restrict_free = [] && restrict_conts = [] then
                           RWSeqContext (index, stack_ind, vars')
                        else
                           RWSeqFreeVarsContext (restrict_conts, restrict_free, index, stack_ind, vars')
                     in
                        stack, term, stack_ind
               in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner restrict addrs stack svars ((v, ind)::bconts) bvars (i + 1) len mc hyps goals
               in
                  stack, term :: hyps, goals

          | Hypothesis (v, term) ->
               if List.mem_assoc v bvars then
                  REF_RAISE(RefineError ("compile_so_redex_sequent_inner", StringVarError ("repeated variable", v)));
               let stack, term = compile_so_redex_term true restrict addrs stack svars bconts bvars term in
               let l = List.length stack in
               let stack = stack @ [FOVar v] in
               let bvars = (new_bvar_item l v) :: bvars in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars (i + 1) len mc hyps goals
               in
                  stack, RWSeqHyp (bname l v, term) :: hyps, goals

   and compile_so_redex_goals restrict addrs stack svars bconts bvars i len goals =
      if i = len then
         stack, []
      else
         let stack, goal = compile_so_redex_term true restrict addrs stack svars bconts bvars (SeqGoal.get goals i) in
         let stack, goals = compile_so_redex_goals restrict addrs stack svars bconts bvars (i + 1) len goals in
            stack, goal :: goals

   and compile_so_redex_terms allow_so_patterns restrict addrs stack svars bconts bvars = function
      [] ->
         stack, []
    | term::t ->
         let stack', term' = compile_so_redex_term allow_so_patterns restrict addrs stack svars bconts bvars term in
         let stack'', terms' = compile_so_redex_terms allow_so_patterns restrict addrs stack' svars bconts bvars t in
            stack'', term'::terms'

   let check_stack = function
      SOVarInstance (n, _, _)
    | FreeFOVarInstance n ->
         REF_RAISE(RefineError ("check_stack", RewriteAllSOInstances n))
    | _ ->
         ()

   let rec stack_cvars i = function
      CVar (v, _, _) :: tl -> (v, i) :: stack_cvars (i + 1) tl
    | _ :: tl -> stack_cvars (i + 1) tl
    | [] -> []

   let compile_so_redex strict addrs = function
      [] -> [||], []
    | (goal::args) as all ->
         let restrict = (strict=Strict) in
         let svars = free_vars_terms all in
         let stack, goal = compile_so_redex_term true restrict addrs [] svars [] [] goal in
         let stack, args = compile_so_redex_terms true restrict addrs stack svars (stack_cvars 0 stack) [] args in
            List.iter check_stack stack;
            Array.of_list stack, (goal::args)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
