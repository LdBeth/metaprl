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

open Lm_symbol

open Lm_debug
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_shape_sig
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
    with type term = TermType.term
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term')
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
   (RewriteDebug : RewriteDebugSig
    with type rwterm = RewriteTypes.rwterm
    with type rstack = RewriteTypes.rstack
    with type varname = RewriteTypes.varname)
   =
struct
   open TermType
   open Term
   open TermMan
   open TermSubst
   open RefineError
   open RewriteTypes
   open RewriteUtil
   open RewriteDebug

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
    | RWSeqHypBnd (v', RWFreeVars(t, cs, rs)) -> RWSeqHypBnd (v', RWFreeVars(t, cs, v::rs))
    | RWSeqHypBnd (v', t) -> RWSeqHypBnd (v', RWFreeVars(t, [], [v]))
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

   let rec compile_so_redex_term allow_so_patterns restrict addrs stack svars bconts bvars term =
      (* Check for variables and contexts *)
      if is_var_term term then
         let v = dest_var term in
            if List.mem_assoc v bvars then
               stack, RWCheckVar(svar_index bvars v)
            else
               REF_RAISE(RefineError ("compile_so_redex_term", StringVarError("First order variables must be bound", v)))
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
               stack, RWSOMatch(rstack_so_index v stack, terms)
            end else
               let stack' = (stack @ [SOVarInstance (v, conts, List.length subterms)]) in
               let stack', terms =
                  compile_so_redex_terms false restrict addrs stack' svars bconts bvars subterms in
               stack', RWSOMatch(List.length stack, terms)

      else if is_context_term term then
         let v, term, conts, vars = dest_context term in
            if rstack_mem v stack then
               (* The context should have a unique name *)
               REF_RAISE(RefineError ("is_context_term", RewriteBoundSOVar v))

            else if Lm_array_util.mem v addrs then
               (* All the vars should be free variables *)
               let vars' = List.map (var_index bvars) vars in
               let index = List.length stack in
               let stack = stack @ [CVar v] in
               let stack, term = compile_so_redex_term allow_so_patterns restrict addrs stack svars ((v,index)::bconts) bvars term in
               let term = RWSOContext(Lm_array_util.index v addrs, index, term, vars') in
               let restrict_free = if restrict then Lm_list_util.subtract (List.map bvar_ind bvars) vars' else [] in
               let restrict_conts = if restrict then restricted_conts v bconts conts else [] in
                  stack, if restrict_free = [] && restrict_conts = [] then term else
                     (* RWFreeVars(term,restrict_conts,restrict_free) *)
                     raise (Invalid_argument "compile_so_redex_term: free variable restrictions on SO contexts are not implemented")
            else
               (* No argument for this context *)
               REF_RAISE(RefineError ("is_context_term", RewriteMissingContextArg v))

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
      let stack, hyps, goals =
         compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars 0 (SeqHyp.length hyps) hyps goals in
         stack, RWSequent (arg, hyps, goals)

   and compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars i len hyps goals =
      if i = len then
         let stack, goals =
            compile_so_redex_goals restrict addrs stack svars bconts bvars 0 (SeqGoal.length goals) goals
         in
            stack, [], goals
      else
         match SeqHyp.get hyps i with
            Context (v, conts, vars) ->
               if rstack_c_mem v stack then
                  raise(Invalid_argument("Rewrite_compile_redex.compile_so_redex_sequent: context " ^ string_of_symbol v ^ " appears more than once in redeces, which is currently unsupported"))
               else if rstack_mem v stack then
                  (* The context should have a unique name *)
                  REF_RAISE(RefineError ("is_context_term", RewriteBoundSOVar v))
               else let index =
                  if i = (len - 1) then
                     if Lm_array_util.mem v addrs then
                        REF_RAISE(RefineError ("compile_so_redex_sequent_inner", StringVarError("Context at the end of the sequent does not need to be passed in as an argument",v)))
                     else
                        -1
                  else
                     if Lm_array_util.mem v addrs then
                        Lm_array_util.index v addrs
                     else
                        REF_RAISE(RefineError ("compile_so_redex_sequent_inner", RewriteMissingContextArg v))
               in
               (* All the vars should be free variables *)
               let vars' = List.map (var_index bvars) vars in
               let stack = stack @ [CVar v] in
               let restrict_free = if restrict then Lm_list_util.subtract (List.map bvar_ind bvars) vars' else [] in
               let restrict_conts = if restrict then restricted_conts v bconts conts else [] in
               let stack_ind = List.length stack - 1 in
               let term =
                  if restrict_free = [] && restrict_conts = [] then
                     RWSeqContext (index, stack_ind, vars')
                  else
                     RWSeqFreeVarsContext (restrict_conts, restrict_free, index, stack_ind, vars')
               in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner restrict addrs stack svars ((v, stack_ind)::bconts) bvars (i + 1) len hyps goals
               in
                  stack, term :: hyps, goals

          | HypBinding (v, term) ->
               if List.mem_assoc v bvars then
                  REF_RAISE(RefineError ("compile_so_redex_sequent_inner", StringVarError ("repeated variable", v)));
               let stack, term = compile_so_redex_term true restrict addrs stack svars bconts bvars term in
               let l = List.length stack in
               let stack = stack @ [FOVar v] in
               let bvars = (new_bvar_item l v) :: bvars in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars (i + 1) len hyps goals
               in
                  stack, RWSeqHypBnd (bname l v, term) :: hyps, goals

          | Hypothesis term ->
               let stack, term = compile_so_redex_term true restrict addrs stack svars bconts bvars term in
               let stack, hyps, goals =
                  compile_so_redex_sequent_inner restrict addrs stack svars bconts bvars (i + 1) len hyps goals
               in
               let l = List.length stack in
               let hyps = RWSeqHypBnd (bname l "", term) :: List.map (restrict_free_in_hyp l) hyps in
                  stack @ [FOVar (Lm_symbol.add "")], hyps, List.map (restrict_free_in_term l) goals

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
      SOVarInstance (n, _, _) ->
         REF_RAISE(RefineError ("check_stack", RewriteAllSOInstances n))
    | _ ->
         ()

   let rec stack_cvars i = function
      CVar v :: tl -> (v, i) :: stack_cvars (i + 1) tl
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
