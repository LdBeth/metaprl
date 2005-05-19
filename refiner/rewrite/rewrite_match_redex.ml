(*
 * Match a particular term against the previously compiled redex.
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

open Opname
open Term_sig
open Term_base_sig
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
   show_loading "Loading Rewrite_match_redex%t"

let debug_rewrite = load_debug "rewrite"

IFDEF VERBOSE_EXN THEN
   DEFINE PARAM_REASON = p
ELSE
   DEFINE PARAM_REASON = NOTHING
ENDIF

module MakeRewriteMatchRedex (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermShape : TermShapeSig with type term = TermType.term)
   (RefineError : RefineErrorSig with module Types = TermType)
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack)
   (RewriteDebug : RewriteDebugSig
    with type rwterm = MakeRewriteTypes(TermType)(TermAddr).rwterm
    with type stack = MakeRewriteTypes(TermType)(TermAddr).stack
    with type varname = MakeRewriteTypes(TermType)(TermAddr).varname)
=
struct
   module RewriteTypes = MakeRewriteTypes(TermType)(TermAddr);;
   open TermType
   open Term
   open TermMan
   open TermAddr
   open TermSubst
   open TermShape
   open RefineError
   open RewriteTypes
   open RewriteUtil
   open RewriteDebug

   let varHACK = Lm_symbol.add "__@@extract_bvarsemptyvariableHACK@@__"

   let rec extract_bvars stack = function
      [] -> []
    | v::tl ->
         let s =
            match stack.(v) with
               StackString s -> Lm_symbol.add s
             | StackVar v -> v
             | StackVoid -> varHACK
             | _ -> raise(Invalid_argument("Rewrite_match_redex.extract_bvars: invalid stack entry"))
         in
            s :: extract_bvars stack tl

   let check_var_restrictions stack vars v =
      match stack.(v) with
         StackVar v ->
            if SymbolSet.mem vars v then
                  REF_RAISE(RefineError ("check_var_restrictions",
                     StringVarError("Variable appears free where it is not allowed to", v)))
       | _ ->
            raise(Invalid_argument("Rewrite_match_redex.check_var_restrictions: internal error"))

   let rec bvars_of_cont hyps vars i count =
      if count = 0 then
         vars
      else
         let vars = match SeqHyp.get hyps i with Context(v, _, _) | Hypothesis (v, _) -> SymbolSet.add vars v in
            bvars_of_cont hyps vars (i + 1) (count - 1)

   let rec check_cont_bvars_hyps hyps vars i count =
      if count > 0 then begin
         begin
            match SeqHyp.get hyps i with
                Context(v, _, _)
              | Hypothesis (v, _) ->
                  if SymbolSet.mem vars v then
                     REF_RAISE(RefineError ("check_cont_bvars_hyps",
                        StringVarError("Variable appears free where it is not allowed to", v)))
         end;
         check_cont_bvars_hyps hyps vars (i+1) (count-1)
      end

   let restrict_context_vars stack vars v =
      match stack.(v) with
         StackVoid ->
            stack.(v) <- StackContextRestrict(vars)
       | StackContextRestrict(vars') ->
            stack.(v) <- StackContextRestrict(SymbolSet.union vars' vars)
       | StackSeqContext (_, (ind, count, hyps)) ->
            check_cont_bvars_hyps hyps vars ind count
       | StackContext _ ->
            (* XXX: TODO *)
            raise (Invalid_argument "Rewrite_match_redex.restrict_context_vars: non-sequent contexts not supported")
       | StackITerm _
       | StackBTerm _
       | StackLevel _
       | StackOpname _
       | StackVar _
       | StackString _
       | StackShape _
       | StackNumber _  -> raise (Invalid_argument "Rewrite_match_redex.restrict_context_vars: internal error")

   let restrict_vars stack var_set conts vars =
      List.iter (check_var_restrictions stack var_set) vars;
      List.iter (restrict_context_vars stack var_set) conts

   let check_free_vars_set vars t =
      if is_some_var_free vars t then
         REF_RAISE(RefineError ("Rewrite_match_redex.check_free_vars_set",
                                StringTermError("term has free occurrences of some variables it is not allowed to have", t)))

   let rec hyp_free_vars vars hyps min i =
      if i = min then
         vars
      else
         let i = i - 1 in
         match SeqHyp.get hyps i with
            Hypothesis (var, term) ->
               hyp_free_vars (SymbolSet.union (SymbolSet.remove vars var) (free_vars_set term)) hyps min i
          | Context (var, conts, terms) ->
               let vars = SymbolSet.add_list (SymbolSet.union (SymbolSet.remove vars var) (free_vars_terms terms)) conts in
                  hyp_free_vars vars hyps min i

   let rec hyp_compute_sub hyps all_bvars vars i count sub =
      if count = 0 then
         sub
      else match SeqHyp.get hyps i with
         Hypothesis (v, _) ->
            if SymbolSet.mem all_bvars v then
               let v' = new_name v (SymbolSet.mem all_bvars) in
                  hyp_compute_sub hyps (SymbolSet.add all_bvars v') vars (i+1) (count-1) ((v, mk_var_term v') ::sub)
            else
               hyp_compute_sub hyps (SymbolSet.add all_bvars v) vars (i+1) (count-1) sub
       | Context (v, _, _) ->
            if SymbolSet.mem vars v then
               REF_RAISE(RefineError("Rewrite_match_redex.hyp_compute_sub", StringVarError("Context variable occures freely where it is not allowed", v)));
            hyp_compute_sub hyps (SymbolSet.add all_bvars v) vars (i+1) (count-1) sub

   let check_instance_term vars t =
      if SymbolSet.intersectp vars (free_vars_set t) then
         REF_RAISE(RefineError("Rewrite_match_redex.check_instance_term", StringTermError("term in the inner sequent is bound by the outer context", t)))

   let rec check_instance_hyps concl vars hyps i len =
      if i = len then
         check_instance_term vars concl
      else
         match SeqHyp.get hyps i with
            Context(_, _, ts) ->
               List.iter (check_instance_term vars) ts;
               check_instance_hyps concl vars hyps (i+1) len
          | Hypothesis (v,t) ->
               check_instance_term vars t;
               let vars = SymbolSet.remove vars v in
                  if not (SymbolSet.is_empty vars) then
                     check_instance_hyps concl vars hyps (i+1) len

   (*
    * Assign the bvars.
    *)
   let set_bvar stack v = function
      StackName i ->
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "Rewrite.set_bvars: stack(%d)/%d with %a%t" i (Array.length stack) output_symbol v eflush
         ENDIF;
         stack.(i) <- StackVar v
    | _ ->
         raise(Invalid_argument("Rewrite_match_redex.set_bvar"))

   let set_bvars stack names vars =
      IFDEF VERBOSE_EXN THEN
         if !debug_rewrite then
            eprintf "Rewrite.set_bvars %d/%d%t" (List.length vars) (List.length names) eflush
      ENDIF;
      iter2_1 set_bvar stack vars names

   let hyp_apply_subst sub = function
      Hypothesis (v, t) -> Hypothesis (v, apply_subst sub t)
    | Context(v, conts, ts) -> Context (v, conts, Lm_list_util.smap (apply_subst sub) ts)

   let hyp_apply_and_rename_subst sub = function
      Hypothesis (v, t) ->
         let v' = if List.mem_assoc v sub then dest_var (List.assoc v sub) else v in
            Hypothesis (v', apply_subst sub t)
   | Context(v, conts, ts) ->
         Context (v, conts, Lm_list_util.smap (apply_subst sub) ts)

   (*
    * Matching functions.
    *)
   let match_redex_level stack l' l p =
      let { rw_le_const = c'; rw_le_vars = vars' } = l' in
      let { le_const = c; le_vars = vars } = dest_level l in
      let rec collect_var o' = function
         { le_var = v; le_offset = o } as h :: t ->
            let found, notfound = collect_var o' t in
               if o >= o' then
                  mk_level_var v (o - o') :: found, notfound
               else
                  found, h :: notfound
       | [] ->
            [], []
      in
      let rec collect c notfound = function
         { rw_le_var = v'; rw_le_offset = o' } :: t ->
            let found, notfound = collect_var o' notfound in
            let l = mk_level (max (c - o') 0) found in
               stack.(v') <- StackLevel l;
               collect c notfound t
       | [] ->
            if notfound <> [] then
               REF_RAISE(RefineError ("Rewrite_match_redex.match_redex_level", RewriteBadMatch (ParamMatch p)))
      in
      let c =
         if c <= c' then
            0
         else if vars' = [] then
            REF_RAISE(RefineError ("Rewrite_match_redex.match_redex_level", RewriteBadMatch (ParamMatch p)))
         else
            c
      in
         collect c (List.map dest_level_var vars) vars'

   let update_redex_param stack i sp PARAM_REASON =
      match stack.(i) with
         StackVoid ->
            stack.(i) <- sp
       | sp' ->
            if (sp <> sp') then
               REF_RAISE(RefineError ("update_redex_params", RewriteBadMatch (ParamMatch PARAM_REASON)));
            ()

   let match_redex_params stack p' p =
      match p', dest_param p with
         (* Literal matches *)
         RWNumber i, Number j ->
            if not (Lm_num.eq_num i j) then
               REF_RAISE(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
       | RWString s', String s ->
            if (s' <> s) then
               REF_RAISE(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
       | RWToken t', Token t ->
            if not (Opname.eq t' t) then
               REF_RAISE(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
       | RWShape sh', Shape sh ->
            if not (TermShape.eq sh' sh) then
               REF_RAISE(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
         (* Variable matches *)
       | RWQuote, Quote -> ()
       | RWMNumber i, Number j ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMNumber: stack(%d)/%d <- %s%t" (**)
                     i (Array.length stack) (Lm_num.string_of_num j) eflush
            ENDIF;
            update_redex_param stack i (StackNumber j) PARAM_REASON
       | RWMString i, String s ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMString: stack(%d)/%d <- %s%t" (**)
                     i (Array.length stack) s eflush
            ENDIF;
            update_redex_param stack i (StackString s) PARAM_REASON
       | RWMToken i, Token t ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMToken: stack(%d)/%d <- %s%t" (**)
                     i (Array.length stack) (string_of_opname t) eflush
            ENDIF;
            update_redex_param stack i (StackOpname t) PARAM_REASON
       | RWMShape i, Shape sh ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMShape: stack(%d)/%d <- %s%t" (**)
                     i (Array.length stack) (string_of_shape sh) eflush
            ENDIF;
            update_redex_param stack i (StackShape sh) PARAM_REASON
       | RWMVar i, Var v ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMVar: stack(%d)/%d <- %a%t" (**)
                     i (Array.length stack) output_symbol v eflush
            ENDIF;
            update_redex_param stack i (StackVar v) PARAM_REASON
       | RWMLevel1 i, MLevel l ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMLevel1: stack(%d)/%d%t" (**)
                     i (Array.length stack) eflush
            ENDIF;
            update_redex_param stack i (StackLevel l) PARAM_REASON
       | RWMLevel2 l', MLevel l ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMLevel2: stack/%d%t" (**)
                     (Array.length stack) eflush
            ENDIF;
            match_redex_level stack l' l p
       | RWMNumber i, MNumber s ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMNumber: stack(%d)/%d <- %a%t" (**)
                     i (Array.length stack) output_symbol s eflush
            ENDIF;
            update_redex_param stack i (StackVar s) PARAM_REASON
       | RWMString i, MString s ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMString: stack(%d)/%d <- %a%t" (**)
                     i (Array.length stack) output_symbol s eflush
            ENDIF;
            update_redex_param stack i (StackVar s) PARAM_REASON
       | RWMToken i, MToken v ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMToken: stack(%d)/%d <- %a%t" (**)
                     i (Array.length stack) output_symbol v eflush
            ENDIF;
            update_redex_param stack i (StackVar v) PARAM_REASON
       | RWMShape i, MShape v ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex_params.RWMShape: stack(%d)/%d <- %a%t" (**)
                     i (Array.length stack) output_symbol v eflush
            ENDIF;
            update_redex_param stack i (StackVar v) PARAM_REASON

       | _ -> REF_RAISE(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))

   let rec match_redex_params_iter stack pl1 pl2 =
      match (pl1, pl2) with
         p1::pl1, p2::pl2 ->
            match_redex_params stack p1 p2;
            match_redex_params_iter stack pl1 pl2
       | [], [] -> ()
       | _ -> REF_RAISE(redex_params_iter_exn)

   (*
    * Check that two terms are equal under the given var equivalence
    *)
   let check_simple_match t vs t' vs' =
      if not (alpha_equal_vars t vs t' vs') then
         REF_RAISE(RefineError ("check_simple_match", RewriteBadMatch (TermMatch t)))

   (*
    * Check that the terms are all equivalent under the given instantiations
    *)
   let rec check_match addrs stack all_bvars t' vs t subterms =
      if not (alpha_equal_fun (match_redex_term_pred addrs stack all_bvars) t' vs t subterms) then
         REF_RAISE(RefineError ("check_match", RewriteBadMatch (TermMatch2 (t', t))))

   and match_redex_term_pred addrs stack all_bvars t t' =
      match_redex_term addrs stack all_bvars t' t;
      true

   and check_matches addrs stack all_bvars t' vs = function
      (t, subterms) :: tl ->
         check_match addrs stack all_bvars t' vs t subterms;
         check_matches addrs stack all_bvars t' vs tl
    | [] ->
         ()

    (*
     * Match a term against the redex.
     *)
   and match_redex_term addrs stack all_bvars t' t =
      match t' with
         RWComposite { rw_op = op'; rw_bterms = bterms' } ->
            if Opname.eq (opname_of_term t) op'.rw_name then
               begin
                  let term = dest_term t in
                  let params = (dest_op term.term_op).op_params in
                     IFDEF VERBOSE_EXN THEN
                        if !debug_rewrite then
                           eprintf "Rewrite.match_redex.RWComposite: %s[%d](%d)/%a[%d](%d)%t" (**)
                              (string_of_opname op'.rw_name) (List.length op'.rw_params) (List.length bterms')
                              debug_print t (List.length params) (List.length term.term_terms)
                              eflush
                     ENDIF;
                     match_redex_params_iter stack op'.rw_params params;
                     iter2_3 match_redex_bterms addrs stack all_bvars bterms' term.term_terms;
                     IFDEF VERBOSE_EXN THEN
                        if !debug_rewrite then
                           eprintf "Rewrite.match_redex.RWComposite done%t" eflush
                     ENDIF;
               end
            else begin
               IFDEF VERBOSE_EXN THEN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWComposite: %s[%d](%d)@." (**)
                        (string_of_opname op'.rw_name) (List.length op'.rw_params) (List.length bterms');
               ENDIF;
               REF_RAISE(RefineError ("match_redex_term", RewriteBadMatch (TermMatch t)))
            end

       | RWSequent (arg, hyps, concl) ->
            let s = explode_sequent_and_rename t all_bvars in
               match_redex_term addrs stack all_bvars arg s.sequent_args;
               let hyps' = s.sequent_hyps in
                  match_redex_sequent_hyps addrs stack concl s.sequent_concl all_bvars hyps hyps' 0 (SeqHyp.length hyps')

       | RWCheckVar i ->
            begin
               let v = dest_var t in
                  IFDEF VERBOSE_EXN THEN
                     if !debug_rewrite then
                        eprintf "Rewrite.match_redex.RWCheckVar: %d/%a%t" i output_symbol v eflush
                  ENDIF;
                  match stack.(i) with
                     StackString v' ->
                        let v' = Lm_symbol.add v' in
                           IFDEF VERBOSE_EXN THEN
                              if !debug_rewrite then
                                 eprintf "Rewrite.match_redex.RWCheckVar: %a/%a%t" output_symbol v' output_symbol v eflush
                           ENDIF;
                           if v' <> v then
                              REF_RAISE(RefineError ("match_redex_term", RewriteBadMatch (VarMatch v)))
                   | StackVar v' ->
                        IFDEF VERBOSE_EXN THEN
                           if !debug_rewrite then
                              eprintf "Rewrite.match_redex.RWCheckVar: %a/%a%t" output_symbol v' output_symbol v eflush
                        ENDIF;
                        if v' <> v then
                           REF_RAISE(RefineError ("match_redex_term", RewriteBadMatch (VarMatch v)))
                   | _ ->
                        REF_RAISE(RefineError ("match_redex_term", RewriteStringError "stack entry is not a string"))
            end

       | RWMatchFreeFOVar (i, cs, vs) ->
            begin
               let v = dest_var t in
                  restrict_vars stack (SymbolSet.singleton v) cs vs;
                  match stack.(i) with
                     StackVoid ->
                        stack.(i) <- StackVar v
                   | StackVar v' ->
                        if v <> v' then
                           REF_RAISE(RefineError ("match_redex_term", RewriteBadMatch (VarMatch v)))
                   | _ ->
                        REF_RAISE(RefineError ("match_redex_term", RewriteStringError "stack entry is not a variable"))
            end

       | RWSOVar (i, l)
       | RWSOFreeVarsVar(_, _, i, l) ->
            (* Save the term at i *)
            let vars = extract_bvars stack l in
            begin match t' with
               RWSOFreeVarsVar(rconts, rvars, _, _) ->
                  restrict_vars stack (SymbolSet.subtract_list (free_vars_set t) vars) rconts rvars
             | _ ->
                  ()
            end;
            begin
               IFDEF VERBOSE_EXN THEN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOVar: stack(%d)/%d with %a%t"
                     i (Array.length stack) print_term t eflush
               ENDIF;
               match stack.(i) with
                  StackVoid ->
                     IFDEF VERBOSE_EXN THEN
                        if !debug_rewrite then
                           eprintf "\tRWSoVar: Void%t" eflush
                     ENDIF;
                     stack.(i) <- StackBTerm (t, vars);

                | StackBTerm (t', vars') ->
                     begin
                        IFDEF VERBOSE_EXN THEN
                           if !debug_rewrite then
                              eprintf "\tRWSOVar: Bterm: check_simple_match%t" eflush
                        ENDIF
                     end;
                     check_simple_match t vars t' vars';
                     begin
                        IFDEF VERBOSE_EXN THEN
                           if !debug_rewrite then
                              eprintf "\tRWSOVar: Bterm: check_simple_match: ok%t" eflush
                        ENDIF
                     end
                | StackITerm l ->
                     begin
                        IFDEF VERBOSE_EXN THEN
                           if !debug_rewrite then
                              eprintf "\tRWSOVar: ITerm: check_matches%t" eflush
                        ENDIF
                     end;
                     check_matches addrs stack all_bvars t vars l;
                     begin
                        IFDEF VERBOSE_EXN THEN
                           if !debug_rewrite then
                              eprintf "\tRWSOVar: ITerm: check_matches: ok%t" eflush
                        ENDIF
                     end;
                     stack.(i) <- StackBTerm (t, vars)
                | _ ->
                     REF_RAISE(RefineError ("match_redex_term", RewriteStringError "stack entry is not valid"))
            end

       | RWSOInstance (i, subterms) ->
            (* See if the term matches *)
            begin
               IFDEF VERBOSE_EXN THEN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOMatch: stack(%d)/%d with %a%t"
                     i (Array.length stack) print_term t eflush
               ENDIF;
               match stack.(i) with
                  StackVoid ->
                     stack.(i) <- StackITerm [t, subterms]
                | StackBTerm (t'', vars'') ->
                     check_match addrs stack all_bvars t'' vars'' t subterms
                | StackITerm l ->
                     stack.(i) <- StackITerm ((t, subterms)::l)
                | _ ->
                     raise(Invalid_argument "match_redex_term: stack entry is not valid")
            end

       | RWSOContext (addr, i, term', l) ->
            (* Pull an address out of the addr argument *)
            (* XXX: TODO *)
            let addr = addrs.arg_addrs.(addr) in
               begin match stack.(i) with
                  StackVoid -> ()
                | StackContextRestrict (vars) ->
                     (* XXX: TODO *)
                     raise(Invalid_argument "Delayed restriction on non-sequent contexts not currently supported")
                | _ -> raise(Invalid_argument "Rewrite_match_redex: RWSOContext: internal error")
               end;
               IFDEF VERBOSE_EXN THEN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOContext: %s%t" (string_of_address addr) eflush
               ENDIF;
               let term = term_subterm t addr in
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOContext: stack(%d)/%d%t" i (Array.length stack) eflush;
                  stack.(i) <- StackContext (extract_bvars stack l, t, addr);
                  match_redex_term addrs stack all_bvars term' term

       | RWSOFreeVarsContext _ ->
            (* XXX: TODO *)
            raise(Invalid_argument "Rewrite_match_redex: RWSOFreeVarsContext: not implemented")

       | _ ->
            REF_RAISE(RefineError ("match_redex_term", RewriteBadMatch (TermMatch t)))

   and match_redex_bterms addrs stack all_bvars bt' bt =
      if bt'.rw_bvars = 0 then
         let dbt = dest_bterm bt in
            if dbt.bvars = [] then
               match_redex_term addrs stack all_bvars bt'.rw_bterm dbt.bterm
            else
               REF_RAISE(RefineError ("match_redex_bterms", RewriteBadMatch (BTermMatch bt)))
      else
         let dbt = dest_bterm_and_rename all_bvars bt in
            if bt'.rw_bvars = List.length dbt.bvars then
               begin
                  set_bvars stack bt'.rw_bnames dbt.bvars;
                  match_redex_term addrs stack (**)
                     (SymbolSet.add_list all_bvars dbt.bvars)
                     bt'.rw_bterm dbt.bterm
               end
            else
               REF_RAISE(RefineError ("match_redex_bterms", RewriteBadMatch (BTermMatch bt)))

   and match_redex_sequent_hyps addrs stack concl' concl all_bvars hyps' hyps i len =
      match hyps' with
         [] ->
            if i <> len then
               REF_RAISE(RefineError ("match_redex_sequent_hyps", RewriteBadMatch (HypMatch hyps)));
            match_redex_term addrs stack all_bvars concl' concl
       | (RWSeqContext (addr, j, l) as hyp') :: hyps'
       | (RWSeqFreeVarsContext (_, _, addr, j, l) as hyp') :: hyps' ->
            let count =
               if addr < 0 then
                  let count = len - i + addr + 1 in
                     if count >= 0 then count
                     else
                        REF_RAISE(RefineError ("match_redex_sequent_hyps", RewriteBadMatch (HypMatch hyps)))
               else
                  let count = addrs.arg_ints.(addr) in
                     if count > 0 then count - 1 else len - i + count
            in
               IFDEF VERBOSE_EXN THEN
                  if !debug_rewrite then
                     eprintf "RWSeqContext/RWSeqFreeVarsContext (%d, %d, [%a])%t" count j print_int_list l eflush
               ENDIF;
               if count + i > len then
                  REF_RAISE(RefineError ("Rewrite_match_redex.match_redex_sequent_hyps", StringError "not enough hypotheses"));
               begin
                  match hyp' with
                     RWSeqFreeVarsContext (rconts, rvars, _, _, _) ->
                        restrict_vars stack (hyp_free_vars SymbolSet.empty hyps i (i+count)) rconts rvars
                   | _ -> ()
               end;
               let bvars = extract_bvars stack l in
               let sub =
                  match stack.(j) with
                     StackVoid -> []
                   | StackContextRestrict (vars) ->
                        hyp_compute_sub hyps (SymbolSet.union vars all_bvars) vars i count []
                   | _ -> raise(Invalid_argument "Rewrite_match_redex.match_redex_sequent_hyps : internal error")
               in
               (* XXX: BUG? hyp_apply_and_rename_subst is not exactly right :-( *)
               let hyps = if sub = [] then hyps else SeqHyp.lazy_apply (hyp_apply_and_rename_subst sub) hyps in
               let concl = if sub = [] then concl else apply_subst sub concl in
               let all_bvars = bvars_of_cont hyps all_bvars i count in
                  stack.(j) <- StackSeqContext (bvars, (i, count, hyps));
                  match_redex_sequent_hyps addrs stack concl' concl all_bvars hyps' hyps (i+count) len

       | RWSeqContextInstance (j, ts) :: hyps' ->
            begin
               match stack.(j) with
                  StackSeqContext (bvars, (k, count, hyps'')) ->
                     if count + i > len then
                        REF_RAISE(RefineError ("Rewrite_match_redex.match_redex_sequent_hyps", StringError "not enough hypotheses"));
                     let sub = match_context_instance addrs stack all_bvars concl hyps i hyps'' k bvars ts SymbolSet.empty [] count in
                        match_redex_sequent_hyps addrs stack concl' (apply_subst sub concl) (**)
                           all_bvars hyps' (SeqHyp.lazy_apply (hyp_apply_subst sub) hyps) (i+count) len
                | _ ->
                     raise (Invalid_argument "Rewrite_match_redex.match_redex_sequent_hyps: RWSeqContextInstance: invalid stack entry")
            end

       | RWSeqHyp (name, term') :: hyps' ->
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "RWSeqHyp (%i out of %i)%t" i len eflush
            ENDIF;
            if i = len then
               REF_RAISE(RefineError ("Rewrite_match_redex.match_redex_sequent_hyps", StringIntError ("not enough hypotheses when matching hypothesis number", succ i)))
            else
               begin
                  match SeqHyp.get hyps i with
                     Hypothesis (v, term) ->
                        set_bvar stack v name;
                        let new_bvars = SymbolSet.add all_bvars v in
                           (*
                            * Strictly speaking, the match_redex_term below should
                            * use the all_bvars, not the new_bvars. But since the
                            * the bvars arg only used to cause alpha-renaming of
                            * bterms, it does not hurt to pass the var in there
                            * as well and, possibly, avoid some potential name
                            * clashes.
                            *)
                           match_redex_term addrs stack new_bvars term' term;
                           match_redex_sequent_hyps addrs stack concl' concl new_bvars hyps' hyps (succ i) len
                   | Context _ ->
                        REF_RAISE(RefineError ("Rewrite_match_redex.match_redex_sequent_hyps", StringIntError ("hypothesis index refers to a context", i)))
               end

   (*
    * This function checks whether a given instance of a sequent contex matches the first instance
    * of that context, as recorded in the stack. It will also attempt to alpha-rename the current
    * sequent so that the current context exactly matches tha one recorder in the stack. This takes
    * care of "context substitution" (e.g. in "<H> >- (<H> >- t) <--> <H> >- t" the inner sequent of
    * the redex needs to be alpha-renamed, so that the free variables of t get matched up with the
    * variables of the outer H (which is the one that will get recorded on the stack), so that when
    * H and t from the stack are put back together in the contractum, the binding structure is still
    * correct.
    *
    * When doing the alpha-renaming we also check (check_instance_hyps) that the remainder of the
    * sequent (e.g. t in the example above) does not have any variables bound by the _outer_ instance
    * (since that would not be a legal match).
    *
    * XXX BUG: When the current instance is _not_ in scope of the original one, any errors reported by
    * check_instance_hyps would be a result of an accidental variable name clash and thus a bug. This
    * is _not_ an easy to fix bug - in case such a clash does happen we are pretty much stuck and the
    * only reasonable solution is trying to prevent such a clash in the first place, but that can not
    * be done in any straightforward way in the current rewriter framework.
    *)
   and match_context_instance addrs stack all_bvars concl hyps i hyps' k bvars ts vars sub count =
      if count = 0 then
         begin
            if not (SymbolSet.is_empty vars) then
               check_instance_hyps concl vars hyps i (SeqHyp.length hyps);
            sub
         end
      else
         match SeqHyp.get hyps i, SeqHyp.get hyps' k with
            Hypothesis (v, t1), Hypothesis(v', t2) ->
               check_instance_term vars t1;
               let t1 = apply_subst sub t1 in
                  check_match addrs stack all_bvars t2 bvars t1 ts;
                  let all_bvars = SymbolSet.add all_bvars v' in
                     if v = v' then
                        match_context_instance addrs stack all_bvars concl hyps (i+1) hyps' (k+1) bvars ts vars sub (count-1)
                     else
                        match_context_instance addrs stack all_bvars concl hyps (i+1) hyps' (k+1) bvars ts (SymbolSet.add (SymbolSet.remove vars v) v') ((v, mk_var_term v') :: sub) (count-1)
          | Context (v, conts, ts1), Context(v', conts', ts2)
            when v=v' && conts=conts' && (List.length ts1 = List.length ts2) ->
               List.iter (check_instance_term vars) ts1;
               let all_bvars = SymbolSet.add all_bvars v' in
               let ts1 = Lm_list_util.smap (apply_subst sub) ts1 in
                  List.iter2 (fun t1 t2 -> check_match addrs stack all_bvars t2 bvars t1 ts) ts1 ts2;
                  match_context_instance addrs stack all_bvars concl hyps (i+1) hyps' (k+1) bvars ts (SymbolSet.remove vars v) sub (count-1)
          | _ ->
               REF_RAISE(RefineError ("Rewrite_match_redex.match_context_instance", StringError ("hypothesis/context mismatch")))
   let match_redex addrs stack vars t tl = function
      [] ->
         REF_RAISE(RefineError ("match_redex", StringError "progs list is empty"))
    | prog::progs ->
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               begin
                  eprintf "Rewrite.match_redex: %d%t" (List.length progs) eflush;
                  List.iter (print_prog stderr) (prog::progs)
               end
         ENDIF;
         iter2_3 match_redex_term addrs stack vars progs tl;
         match_redex_term addrs stack vars prog t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
