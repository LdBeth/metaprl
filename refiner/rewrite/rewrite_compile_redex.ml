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

open Term_sig
open Term_base_sig
open Term_op_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_shape_sig
open Refine_error_sig

open Rewrite_sig
open Rewrite_util_sig
open Rewrite_debug_sig
open Rewrite_types

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite_compile_redex%t"

module MakeRewriteCompileRedex (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (RefineError : RefineErrorSig with module Types = TermType)
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack)
   (RewriteDebug : RewriteDebugSig
    with type rwterm = MakeRewriteTypes(TermType)(TermAddr).rwterm
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack
    with type varname = MakeRewriteTypes(TermType)(TermAddr).varname)
=
struct
   module RewriteTypes = MakeRewriteTypes(TermType)(TermAddr);;
   open TermType
   open Term
   open TermOp
   open TermMan
   open TermSubst
   open RefineError
   open RewriteTypes
   open RewriteUtil

   type state =
      { st_ints : var array;
        st_addrs : var array;
        st_strict : bool;               (* Whether we should be using the strict semantics *)
        st_patterns : bool;             (* Whether SO variable instances are allowed to define patterns *)
        st_svars : SymbolSet.t;         (* Stack variables to avoid clashing with *)
        st_bvars : (var * int) list;    (* All the bindings we are under, and their stack locations *)
        st_bconts : (var * int) list;   (* All the contexts we are in, and their stack locations  *)
        st_arg : bool;                  (* Whether we are compiling an argument ("semiformal") term *)
        st_restricts : (int * var list) list ref
                                        (* Output: additional restrictions: SO var location * SO conts locations *)
      }

   let bname i _ =
      StackName i

   let new_bvar_item (i: int) (v: var) =
      (v, i)

   (* Determine if all terms in a list are distinct bound vars *)
   let rec are_bound_vars bvars = function
      [] ->
         true
    | v::ts ->
         is_var_term v
         && (let v = dest_var v in
                List.mem_assoc v bvars && are_bound_vars (List.remove_assoc v bvars) ts)

   let bvar_ind ((_ : var), (i : int)) =
      i

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
      [] -> REF_RAISE(RefineError ("Rewrite_compile_redex", RewriteFreeContextVar(v,c)))
    | ((v', _) as hd) :: tl ->
         if Lm_symbol.eq v v' then
            tl
         else
            hd :: restrict_cont c v tl

   let rec restricted_conts c bconts = function
      [] -> List.map bvar_ind bconts
    | v :: tl -> restricted_conts c (restrict_cont c v bconts) tl

   let check_cont c bconts v =
      if not (List.mem_assoc v bconts) then
         REF_RAISE(RefineError("Rewrite_compile_redex",RewriteFreeContextVar(v,c)))

   let rec lastcontext hyps i =
      let i = pred i in
         if i < 0 then
            i
         else
            match SeqHyp.get hyps i with
               Context _ -> i
             | Hypothesis _ -> lastcontext hyps i

   let rec collect_bvars i stack bnames bvars = function
      [] ->
         stack,bnames,bvars
    | v::vs ->
         collect_bvars (succ i) (stack @ [FOVar v]) (bnames @ [bname i v]) ((new_bvar_item i v) :: bvars) vs

   let rec compile_so_redex_term st stack term =
      (* Check for variables and contexts *)
      if is_var_term term then
         let v = dest_var term in
            if List.mem_assoc v st.st_bvars then
               stack, RWCheckVar(svar_index st.st_bvars v)
            else
               let stack =
                  let mem = rstack_freefo_mem v stack in
                     if not mem && rstack_mem v stack then
                        REF_RAISE(RefineError ("compile_so_redex_term", StringVarError("Free FO variable clashes with another variable ", v)));
                     if st.st_patterns then
                        if mem then
                           rstack_upgrade v stack
                        else
                           stack @ [ FreeFOVarPattern v ]
                     else if mem then
                        stack
                     else
                        stack @ [ FreeFOVarInstance v ]
               in
               let index = rstack_freefo_index v stack in
               let restrict_conts =
                  if st.st_strict then
                     if st.st_arg then begin
                        st.st_restricts := (index, []) :: !(st.st_restricts);
                        []
                     end else
                        List.map bvar_ind st.st_bconts
                  else
                     []
               in
               let restrict_vars = if st.st_strict then List.map bvar_ind st.st_bvars else [] in
                  stack, RWMatchFreeFOVar (index, restrict_conts, restrict_vars)

      else if is_so_var_term term then
         let v, conts, subterms = dest_so_var term in
         let so_mem = rstack_so_mem v stack in
            if st.st_patterns && are_bound_vars st.st_bvars subterms then
               (* This is a second order variable, all subterms are vars *
                * and we do not have a pattern yet                       *)
               let () =
                  if so_mem then begin
                     rstack_check_arity v conts (List.length subterms) stack;
                     if not st.st_arg && st.st_strict then List.iter (check_cont v st.st_bconts) conts
                  end
               in
               let stack =
                  if so_mem then
                     rstack_upgrade v stack
                  else
                     stack @ [SOVarPattern (v, conts, List.length subterms)]
               in
               let v' = rstack_so_index v stack in
               let args = if subterms = [] then [] else List.map (var_index st.st_bvars) subterms in
               let term = RWSOVar(v', args) in
               let restrict_free =
                  if st.st_strict then
                     let bvars = List.map bvar_ind st.st_bvars in
                        if args = [] then
                           bvars
                        else
                           Lm_list_util.subtract bvars args
                  else
                     []
               in
               let restrict_conts =
                  if st.st_strict then
                     if st.st_arg then begin
                        st.st_restricts := (v', conts) :: !(st.st_restricts);
                        []
                     end else
                        restricted_conts v st.st_bconts conts
                  else
                     []
               in
               let t =
                  if restrict_free = [] && restrict_conts = [] then
                     term
                  else
                     RWFreeVars (term, restrict_conts,restrict_free)
               in
                  stack, t

            (* This is a second order variable instance *)
            else if so_mem then
               begin
                  rstack_check_arity v conts (List.length subterms) stack;
                  if not st.st_arg && st.st_strict then List.iter (check_cont v st.st_bconts) conts;
                  (*
                   * we set st_patterns to false here since the term that is going to
                   * match the SO variable v may have no free occurences of this argument
                   * and this would prevent us from establishing an SO pattern.
                   *)
                  let stack, terms = compile_so_redex_terms { st with st_patterns = false } stack subterms in
                     stack, RWSOInstance(rstack_so_index v stack, terms)
               end
            else
               let index = List.length stack in
               let stack = stack @ [SOVarInstance (v, conts, List.length subterms)] in
               let stack, terms = compile_so_redex_terms { st with st_patterns = false } stack subterms in
                  stack, RWSOInstance(index, terms)

      else if is_context_term term then
         let v, term, conts, vars = dest_context term in
            if rstack_c_mem v stack then
               (* XXX: TODO *)
               raise (Invalid_argument "Matching non-sequent context intances is not supported yet")

            else if Lm_array_util.mem v st.st_addrs then
               (* All the vars should be free variables *)
               let vars' = List.map (var_index st.st_bvars) vars in
               let index = List.length stack in
               let stack = stack @ [CVar (v, conts, List.length vars)] in
               let stack, term = compile_so_redex_term { st with st_bconts = (v,index)::st.st_bconts } stack term in
               let aindex = Lm_array_util.index v st.st_addrs in
               let restrict_free = if st.st_strict then Lm_list_util.subtract (List.map bvar_ind st.st_bvars) vars' else [] in
               let restrict_conts =
                  if st.st_strict then
                     if st.st_arg then begin
                        st.st_restricts := (index, conts) :: !(st.st_restricts);
                        []
                     end else
                        restricted_conts v st.st_bconts conts
                  else
                     []
               in
               let t =
                  if restrict_free = [] && restrict_conts = [] then
                     RWSOContext(aindex, index, term, vars')
                  else
                     RWSOFreeVarsContext(restrict_conts, restrict_free, aindex, index, term, vars')
               in
                  stack, t
            else
               (* No argument for this context *)
               REF_RAISE(RefineError ("compile_so_redex_term", RewriteMissingContextArg v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         let { sequent_args = arg;
               sequent_hyps = hyps;
               sequent_concl = concl;
             } = explode_sequent term
         in
         let stack, arg = compile_so_redex_term st stack arg in
         let l = SeqHyp.length hyps in
         let stack, hyps, concl =
            compile_so_redex_sequent_inner st stack 0 l (lastcontext hyps l) hyps concl in
            stack, RWSequent (arg, hyps, concl)

      else
         (* This is normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let stack, params = compile_so_redex_params st stack params in
         let stack, bterms = compile_so_redex_bterms st stack bterms in
            stack, RWComposite { rw_op = { rw_name = name; rw_params = params }; rw_bterms = bterms }

   (*
    * We also compile parameters, and bind meta-variables.
    *)
   and compile_so_redex_params st stack = function
      [] ->
         stack, []
    | param::params ->
         (* Do this param *)
         let stack, param = compile_so_redex_param st stack param in
         let stack, params = compile_so_redex_params st stack params in
            stack, param::params

   and meta_param' stack ptype v =
      if rstack_p_mem ptype v stack then
         (* This param is not free, do a match *)
         stack, rstack_p_index ptype v stack
      else
         begin
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

   and compile_so_redex_param st stack param =
      match dest_param param with
         MNumber v -> meta_param stack (fun i -> RWMNumber i) ShapeNumber v
       | MString v -> meta_param stack (fun s -> RWMString s) ShapeString v
       | MToken v -> meta_param stack (fun t -> RWMToken t) ShapeToken v
       | Var v when not st.st_strict -> meta_param stack (fun v -> RWMVar v) ShapeVar v
       | MLevel l -> meta_level stack l
       | Number i -> stack, RWNumber i
       | String s -> stack, RWString s
       | Token t -> stack, RWToken t
       | Quote -> stack, RWQuote
       | _ -> REF_RAISE(RefineError ("compile_so_redex_param", RewriteBadMatch (ParamMatch param)))

   (*
    * In bterms, have to add these vars to the binding stack.
    *)
   and compile_so_redex_bterms st stack = function
      [] ->
         stack, []
    | bterm::bterms ->
         let stack, bterm = compile_so_redex_bterm st stack bterm in
         let stack, bterms = compile_so_redex_bterms st stack bterms in
            stack, bterm::bterms

   and compile_so_redex_bterm st stack bterm =
      let svars = SymbolSet.add_list st.st_svars (List.map rstack_var stack) in
      let { bvars = vars; bterm = term } = dest_bterm_and_rename bterm svars in
      let stack, bnames, bvars = collect_bvars (List.length stack) stack [] st.st_bvars vars in
      (* Compile the term *)
      let stack, term = compile_so_redex_term { st with st_bvars = bvars; st_svars = svars } stack term in
         stack, { rw_bvars = List.length vars; rw_bnames = bnames; rw_bterm = term }

   and compile_so_redex_sequent_inner st stack i len mc hyps concl =
      if i = len then
         let stack, concl =
            compile_so_redex_term st stack concl
         in
            stack, [], concl
      else
         match SeqHyp.get hyps i with
            Context (v, conts, terms) ->
               let instance = rstack_c_mem v stack in
               let stack, term, ind =
                  if instance then
                     begin
                        rstack_check_arity v conts (List.length terms) stack;
                        if not st.st_arg && st.st_strict then List.iter (check_cont v st.st_bconts) conts;
                        let stack, terms = compile_so_redex_terms { st with st_patterns = false } stack terms in
                        let ind = rstack_c_index v stack in
                           stack, RWSeqContextInstance (ind, terms), ind
                     end
                  else
                     let index =
                        if i = mc then
                           if Lm_array_util.mem v st.st_ints then
                              REF_RAISE (RefineError ("compile_so_redex_sequent_inner", StringVarError("Last context of the sequent does not need to be passed in as an argument",v)))
                           else
                              i - len
                        else if Lm_array_util.mem v st.st_ints then
                           Lm_array_util.index v st.st_ints
                        else
                           REF_RAISE (RefineError ("compile_so_redex_sequent_inner", RewriteMissingContextArg v))
                     in
                     (* All the vars should be free variables *)
                     let vars' = List.map (var_index st.st_bvars) terms in
                     let stack_ind = List.length stack in
                     let stack = stack @ [CVar (v, conts, List.length terms)] in
                     let restrict_free = if st.st_strict then Lm_list_util.subtract (List.map bvar_ind st.st_bvars) vars' else [] in
                     let restrict_conts =
                        if st.st_strict then
                           if st.st_arg then begin
                              st.st_restricts := (stack_ind, conts) :: !(st.st_restricts);
                              []
                           end else
                              restricted_conts v st.st_bconts conts
                        else
                           []
                     in
                     let term =
                        if restrict_free = [] && restrict_conts = [] then
                           RWSeqContext (index, stack_ind, vars')
                        else
                           RWSeqFreeVarsContext (restrict_conts, restrict_free, index, stack_ind, vars')
                     in
                        stack, term, stack_ind
               in
               let stack, hyps, concl =
                  compile_so_redex_sequent_inner { st with st_bconts = (v, ind) :: st.st_bconts } stack (i + 1) len mc hyps concl
               in
                  stack, term :: hyps, concl

          | Hypothesis (v, term) ->
               let stack, term = compile_so_redex_term st stack term in
               let l = List.length stack in
               let stack = stack @ [FOVar v] in
               let st = { st with st_bvars = (new_bvar_item l v) :: st.st_bvars } in
               let stack, hyps, concl =
                  compile_so_redex_sequent_inner st stack (i + 1) len mc hyps concl
               in
                  stack, RWSeqHyp (bname l v, term) :: hyps, concl

   and compile_so_redex_terms st stack = function
      [] ->
         stack, []
    | term::terms ->
         let stack, term = compile_so_redex_term st stack term in
         let stack, terms = compile_so_redex_terms st stack terms in
            stack, term::terms

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

   let filter_restricts bconts (i, conts) =
      let restrict = restricted_conts empty_var bconts conts in
         if restrict = [] then None else Some (i, restrict)

   let rec map_restricts_term restr = function
      RWComposite t ->
         RWComposite { rw_op = t.rw_op; rw_bterms = List.map (map_restricts_bterm restr) t.rw_bterms }
    | RWCompositeSimple t ->
         RWCompositeSimple { rws_op = t.rws_op; rws_bterms = List.map (map_restricts_bterm restr) t.rws_bterms }
    | RWSequent (t, hyps, concl) ->
         RWSequent (map_restricts_term restr t, List.map (map_restricts_hyp restr) hyps, map_restricts_term restr concl)
    | RWSOVar (i, _) as t ->
         if List.mem_assoc i restr then
            RWFreeVars(t, List.assoc i restr, [])
         else
            t
    | RWSOInstance _ as t ->
         t (* No need to map the subterms since there can not be any patterns there *)
    | RWSOContext (i, j, t, il) ->
         let t = map_restricts_term restr t in
            if List.mem_assoc j restr then
               RWSOFreeVarsContext (List.assoc j restr, [], i, j, t, il)
            else
               RWSOContext (i, j, t, il)
    | RWSOFreeVarsContext(rconts, rvars, i, j, t, il) ->
         let t = map_restricts_term restr t in
            if List.mem_assoc j restr then
               RWSOFreeVarsContext((List.assoc i restr) @ rconts, rvars, i, j, t, il)
            else
               RWSOFreeVarsContext(rconts, rvars, i, j, t, il)
    | RWFreeVars(t, rconts, rvars) ->
         begin match map_restricts_term restr t with
            RWFreeVars(t, rconts', rvars') ->
               RWFreeVars(t, rconts' @ rconts, rvars' @ rvars)
          | t ->
               RWFreeVars(t, rconts, rvars)
         end
    | RWCheckVar _ as t ->
         t
    | RWMatchFreeFOVar (i, conts, vars) as t ->
         if List.mem_assoc i restr then
            RWMatchFreeFOVar (i, (List.assoc i restr) @ conts, vars)
         else
            t
    | RWStackVar _
    | RWSOContextSubst _ ->
         (* These are only used in contractum *)
         raise (Invalid_argument "Rewrite_compile_redex.map_restricts_term: internal error")

   and map_restricts_bterm restr bt =
      { bt with rw_bterm = map_restricts_term restr bt.rw_bterm }

   and map_restricts_hyp restr = function
      RWSeqHyp (v, t) ->
         RWSeqHyp (v, map_restricts_term restr t)
    | RWSeqContext (i, j, il) as t ->
         if List.mem_assoc j restr then
            RWSeqFreeVarsContext (List.assoc j restr, [], i, j, il)
         else
            t
    | RWSeqFreeVarsContext (rconts, rvars, i, j, il) as t ->
         if List.mem_assoc j restr then
            RWSeqFreeVarsContext ((List.assoc j restr) @ rconts, rvars, i, j, il)
         else
            t
    | RWSeqContextInstance (i, ts) ->
         RWSeqContextInstance (i, List.map (map_restricts_term restr) ts)

   let xbinder_opname = Opname.mk_opname "xbinder" Opname.xperv

   let rec find_bvars bvars stack = function
      [] -> bvars
    | t :: ts when is_dep0_term xbinder_opname t ->
         let t = dest_dep0_term xbinder_opname t in
         let bvars =
            if is_var_term t then
               let v = dest_var t in
                  if rstack_freefo_mem v stack then
                     (v, rstack_freefo_index v stack) :: bvars
                  else
                     bvars
            else
               bvars
         in
            find_bvars bvars stack ts
    | _ :: ts ->
         find_bvars bvars stack ts

   let compile_so_redex strict params = function
      [] -> [||], []
    | (goal :: args) as allargs ->
         let st =
            { st_patterns = true;
              st_strict = (strict=Strict);
              st_ints = params.spec_ints;
              st_addrs = params.spec_addrs;
              st_svars = List.fold_left SymbolSet.union (free_vars_terms allargs) (List.map all_meta_variables allargs);
              st_bvars = [];
              st_bconts = [];
              st_arg = true;
              st_restricts = ref []
            }
         in
         let stack, args' = compile_so_redex_terms st [] args in
         let st = { st with st_arg = false; st_bvars = find_bvars [] stack args } in
         let stack, goal = compile_so_redex_term st stack goal in
         let () = List.iter check_stack stack in
         let bconts = stack_cvars 0 stack in
         let extra_restricts = Lm_list_util.some_map (filter_restricts bconts) !(st.st_restricts) in
         let args = if extra_restricts = [] then args' else List.map (map_restricts_term extra_restricts) args' in
            Array.of_list stack, goal :: args
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
