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

let debug_rewrite = load_debug "rewrite"

module MakeRewriteMatchRedex
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
    with type bound_term = TermType.bound_term
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals)
   (RewriteTypes : RewriteTypesSig
    with type level_exp = TermType.level_exp
    with type term = TermType.term
    with type address = TermAddr.address
    with type seq_hyps = TermType.seq_hyps)
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = RewriteTypes.rstack)
   (RewriteDebug : RewriteDebugSig
    with type rwterm = RewriteTypes.rwterm
    with type varname = RewriteTypes.varname)
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
   open RewriteDebug

   type term = TermType.term
   type address = TermAddr.address
   type stack = RewriteTypes.stack
   type rwterm = RewriteTypes.rwterm

   (*
    * Get the vars from their indices.
    *)
   let extract_bvar stack v =
      match stack.(v) with
         StackString s ->
            s
       | _ ->
            ref_raise(RefineError ("extract_bvar", RewriteStringError "stack entry is not a string"))

   let extract_bvars stack l = List.map (extract_bvar stack) l

   (*
    * Assign the bvars.
    *)
   let set_bvar stack v = function
      StackName i ->
#ifdef VERBOSE_EXN
         if !debug_rewrite then
            eprintf "Rewrite.set_bvars: stack(%d)/%d%t" i (Array.length stack) eflush;
#endif
         stack.(i) <- StackString v
    | _ ->
         ()

   let set_bvars stack names vars =
#ifdef VERBOSE_EXN
      if !debug_rewrite then
         eprintf "Rewrite.set_bvars %d/%d%t" (List.length vars) (List.length names) eflush;
#endif
      iter2 (set_bvar stack) vars names

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
               ref_raise(RefineError ("Rewrite_match_redex.match_redex_level", RewriteBadMatch (ParamMatch p)))
      in
      let c =
         if c <= c' then
            0
         else if vars' = [] then
            ref_raise(RefineError ("Rewrite_match_redex.match_redex_level", RewriteBadMatch (ParamMatch p)))
         else
            c
      in
         collect c (List.map dest_level_var vars) vars'

#ifdef VERBOSE_EXN
#  define PARAM_REASON p
#else
#  define PARAM_REASON
#endif

   let update_redex_param stack i sp PARAM_REASON =
      match stack.(i) with
         StackVoid ->
            stack.(i) <- sp
       | sp' ->
            if (sp <> sp') then
               ref_raise(RefineError ("update_redex_params", RewriteBadMatch (ParamMatch PARAM_REASON)));
            ()
           

   let match_redex_params stack p' p =
      match p', dest_param p with
           (* Literal matches *)
         RWNumber i, Number j ->
            if (i <> j) then
               ref_raise(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
       | RWString s', String s ->
            if (s' <> s) then
               ref_raise(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
       | RWToken t', Token t ->
            if (t' <> t) then
               ref_raise(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))
       | RWVar v', Var v ->
            if (v' <> v) then
               ref_raise(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))

           (* Variable matches *)
       | RWMNumber i, Number j ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMNumber: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) (Mp_num.string_of_num j) eflush;
#endif
            update_redex_param stack i (StackNumber j) PARAM_REASON
       | RWMString i, String s ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMString: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) s eflush;
#endif
            update_redex_param stack i (StackString s) PARAM_REASON
       | RWMToken i, Token t ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMToken: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) t eflush;
#endif
            update_redex_param stack i (StackString t) PARAM_REASON
       | RWMVar i, Var v ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMVar: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) v eflush;
#endif
            update_redex_param stack i (StackString v) PARAM_REASON
       | RWMLevel1 i, MLevel l ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMLevel1: stack(%d)/%d%t" (**)
                  i (Array.length stack) eflush;
#endif
            update_redex_param stack i (StackLevel l) PARAM_REASON
       | RWMLevel2 l', MLevel l ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMLevel2: stack/%d%t" (**)
                  (Array.length stack) eflush;
#endif
            match_redex_level stack l' l p
       | RWMNumber i, MNumber s ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMNumber: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) s eflush;
#endif
            update_redex_param stack i (StackMString s) PARAM_REASON
       | RWMString i, MString s ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMString: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) s eflush;
#endif
            update_redex_param stack i (StackMString s) PARAM_REASON
       | RWMToken i, MToken s ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMToken: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) s eflush;
#endif
            update_redex_param stack i (StackMString s) PARAM_REASON
       | RWMVar i, MVar v ->
#ifdef VERBOSE_EXN
            if !debug_rewrite then
               eprintf "Rewrite.match_redex_params.RWMVar: stack(%d)/%d <- %s%t" (**)
                  i (Array.length stack) v eflush;
#endif
            update_redex_param stack i (StackMString v) PARAM_REASON

       | _ -> ref_raise(RefineError ("match_redex_params", RewriteBadMatch (ParamMatch p)))

   let rec match_redex_params_iter stack pl1 pl2 =
      match (pl1, pl2) with
         p1::pl1, p2::pl2 ->
            match_redex_params stack p1 p2;
            match_redex_params_iter stack pl1 pl2
       | [], [] -> ()
       | _ -> ref_raise(redex_params_iter_exn)

   (*
    * Check that two terms are equal under the given var equivalence
    *)
   let check_simple_match t vs t' vs' =
      if not (alpha_equal_vars t vs t' vs') then
         ref_raise(RefineError ("check_simple_match", RewriteBadMatch (TermMatch t)))

   (*
    * Check that the terms are all equivalent under the given instantiations
    *)
   let rec check_match addrs stack t' vs = function
      (t, subterms)::tl ->
         if alpha_equal_fun (match_redex_term_pred addrs stack) t' vs t subterms then
            check_match addrs stack t' vs tl
         else
            ref_raise(RefineError ("check_match", RewriteBadMatch (TermMatch t)))
    | [] ->
         ()

   and match_redex_term_pred addrs stack t t' =
      match_redex_term addrs stack t' t;
      true

    (*
     * Match a term against the redex.
     *)
   and match_redex_term addrs stack t' t =
      match t' with
         RWFreeVars (t'',vars) ->
            check_term_free_vars (extract_bvars stack vars) t;
            match_redex_term addrs stack t'' t
       | RWComposite { rw_op = op'; rw_bterms = bterms' } ->
            let term = dest_term t in
            let op = dest_op term.term_op in
#ifdef VERBOSE_EXN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex.RWComposite: %s[%d](%d)/%a[%d](%d)%t" (**)
                     (string_of_opname op'.rw_name) (List.length op'.rw_params) (List.length bterms')
                     debug_print t (List.length op.op_params) (List.length term.term_terms)
                     eflush;
#endif
               if Opname.eq op.op_name op'.rw_name then
                  begin
                     match_redex_params_iter stack op'.rw_params op.op_params;
                     rev_iter2 (match_redex_bterms addrs stack) bterms' term.term_terms;
#ifdef VERBOSE_EXN
                     if !debug_rewrite then
                        eprintf "Rewrite.match_redex.RWComposite done%t" eflush;
#endif
                  end
               else
                  ref_raise(RefineError ("match_redex_term", RewriteBadMatch (TermMatch t)))

       | RWSequent (arg', hyps', goals') ->
            let { sequent_args = arg;
                  sequent_hyps = hyps;
                  sequent_goals = goals
                } = explode_sequent t
            in
               match_redex_term addrs stack arg' arg;
               match_redex_sequent_hyps addrs stack hyps' hyps 0 (SeqHyp.length hyps);
               match_redex_sequent_goals addrs stack goals' goals 0 (SeqGoal.length goals)

       | RWCheckVar i ->
            begin
               let v = dest_var t in
#ifdef VERBOSE_EXN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWCheckVar: %d/%s%t" i v eflush;
#endif
                  match stack.(i) with
                     StackString v' ->
#ifdef VERBOSE_EXN
                        if !debug_rewrite then
                           eprintf "Rewrite.match_redex.RWCheckVar: %s/%s%t" v' v eflush;
#endif
                        if v' <> v then
                           ref_raise(RefineError ("match_redex_term", RewriteBadMatch (VarMatch v)))
                   | x ->
                        ref_raise(RefineError ("match_redex_term", RewriteStringError "stack entry is not a string"))
            end

       | RWSOVar (i, l) ->
              (* Save the term at i *)
            begin
               let vars = extract_bvars stack l in
#ifdef VERBOSE_EXN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOVar: stack(%d)/%d with %a%t" i (Array.length stack) print_term t eflush;
#endif
                  match stack.(i) with
                     StackVoid ->
#ifdef VERBOSE_EXN
                        if !debug_rewrite then
                           eprintf "\tRWSoVar: Void%t" eflush;
#endif
                        stack.(i) <- StackBTerm (t, vars)
                   | StackBTerm (t', vars') ->
#ifdef VERBOSE_EXN
                        if !debug_rewrite then
                           eprintf "\tRWSOVar: Bterm: check_simple_match%t" eflush;
#endif
                        check_simple_match t vars t' vars';
#ifdef VERBOSE_EXN
                        if !debug_rewrite then
                           eprintf "\tRWSOVar: Bterm: check_simple_match: ok%t" eflush;
#endif
                   | StackITerm l ->
#ifdef VERBOSE_EXN
                        if !debug_rewrite then
                           eprintf "\tRWSOVar: ITerm: check_match%t" eflush;
#endif
                        check_match addrs stack t vars l;
#ifdef VERBOSE_EXN
                        if !debug_rewrite then
                           eprintf "\tRWSOVar: ITerm: check_match: ok%t" eflush;
#endif
                        stack.(i) <- StackBTerm (t, vars)
                   | _ ->
                        ref_raise(RefineError ("match_redex_term", RewriteStringError "stack entry is not valid"))
            end

       | RWSOMatch (i, subterms) ->
              (* See if the term matches *)
            begin
#ifdef VERBOSE_EXN
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOMatch: stack(%d)/%d with %a%t" i (Array.length stack) print_term t eflush;
#endif
                  match stack.(i) with
                     StackVoid ->
                        stack.(i) <- StackITerm [t, subterms]
                   | StackBTerm (t'', vars'') ->
                        check_match addrs stack t'' vars'' [t, subterms]
                   | StackITerm l ->
                        stack.(i) <- StackITerm ((t, subterms)::l)
                   | _ ->
                        raise(Invalid_argument "match_redex_term: stack entry is not valid")
            end

       | RWSOContext (addr, i, term', l) ->
              (* Pull an address out of the addr argument *)
            let addr' = addrs.(addr) in
#ifdef VERBOSE_EXN
               if !debug_rewrite then
                  eprintf "Rewrite.match_redex.RWSOContext: %s%t" (string_of_address addr') eflush;
#endif
               let term = term_subterm t addr' in
                  if !debug_rewrite then
                     eprintf "Rewrite.match_redex.RWSOContext: stack(%d)/%d%t" i (Array.length stack) eflush;
                  stack.(i) <- StackContext (extract_bvars stack l, t, addr');
                  match_redex_term addrs stack term' term

       | _ ->
            ref_raise(RefineError ("match_redex_term", RewriteBadMatch (TermMatch t)))

   and match_redex_bterms addrs stack bt' bt =
      match dest_bterm bt with
         { bvars = vars; bterm = bterm } ->
            if bt'.rw_bvars = List.length vars then begin
               if vars!=[] then set_bvars stack bt'.rw_bnames vars;
               match_redex_term addrs stack bt'.rw_bterm bterm
            end else
               ref_raise(RefineError ("match_redex_bterms", RewriteBadMatch (BTermMatch bt)))

   and match_redex_sequent_hyps addrs stack hyps' hyps i len =
      match hyps' with
         [] ->
            if i <> len then
               ref_raise(RefineError ("match_redex_sequent_hyps", RewriteBadMatch (HypMatch hyps)))
       | hyp' :: hyps' ->
            match hyp' with
               RWSeqContext (addr, j, l) ->
                  let count = depth_of_address addrs.(addr) in
#ifdef VERBOSE_EXN
                  let _ =
                     if !debug_rewrite then
                        eprintf "RWSeqContext (%d, %d, [%a])%t" count j print_int_list l eflush
                  in
#endif
                  if count + i > len then
                     ref_raise(RefineError ("match_redex_sequent_hyps", StringError "not enough hyps"))
                  else
                     let bvars = extract_bvars stack l in
                        stack.(j) <- StackSeqContext (bvars, (i, count, hyps));
                        match_redex_sequent_hyps addrs stack hyps' hyps (i+count) len

             | RWSeqFreeVarsContext (non_free, addr, j, l) ->
                  let count = depth_of_address addrs.(addr) in
                  if count + i > len then
                     ref_raise(RefineError ("match_redex_sequent_hyps", StringError "not enough hyps"))
                  else
                     check_hyp_free_vars (extract_bvars stack non_free) hyps i (i+count);
                     let bvars = extract_bvars stack l in
                        stack.(j) <- StackSeqContext (bvars, (i, count, hyps));
                        match_redex_sequent_hyps addrs stack hyps' hyps (i+count) len

             | RWSeqHyp (name, term') ->
#ifdef VERBOSE_EXN
                  let _ =
                     if !debug_rewrite then
                        eprintf "RWSeqHyp (%a, )%t" print_varname name eflush
                  in
#endif
                  if i = len then
                     ref_raise(RefineError ("get_sequent_hyp", StringIntError ("hyp index is out of range", i)))
                  else begin match SeqHyp.get hyps i with
                     Hypothesis (var, term) ->
                        set_bvar stack var name;
                        match_redex_term addrs stack term' term;
                        match_redex_sequent_hyps addrs stack hyps' hyps (succ i) len
                   | Context _ ->
                        ref_raise(RefineError ("get_sequent_hyp", StringIntError ("hyp index refers to a context", i)))
                  end
             | RWSeqContextSubst _ ->
                  ref_raise(RefineError ("match_redex_seq_hyps", RewriteBadMatch (HypMatch hyps)))

   and match_redex_sequent_goals addrs stack goals' goals i len =
      match goals' with
         [] ->
            if i <> len then
               ref_raise(RefineError ("match_redex_sequent_goals", RewriteBadMatch (GoalMatch goals)))
       | goal' :: goals' ->
            if i = len then
               ref_raise(RefineError ("match_redex_sequent_goals", RewriteBadMatch (GoalMatch goals)));
            match_redex_term addrs stack goal' (SeqGoal.get goals i);
            match_redex_sequent_goals addrs stack goals' goals (succ i) len

   and check_hyp_free_vars vars hyps i len =
      if (i=len) then () else
      match SeqHyp.get hyps i with
         Hypothesis (var, term) ->
            let vars = List_util.remove var vars in
            check_term_free_vars vars term;
            check_hyp_free_vars vars hyps (succ i) len
       | Context (var, terms) ->
            List.iter (check_term_free_vars vars) terms;
            check_hyp_free_vars vars hyps (succ i) len
            
   and check_term_free_vars vars t =
      if List_util.intersects vars (free_vars t) then
         ref_raise(RefineError ("match_redex_term", RewriteBadMatch (TermMatch t)))

   let match_redex addrs stack t tl = function
      [] ->
         ref_raise(RefineError ("match_redex", StringError "progs list is empty"))
    | prog::progs ->
#ifdef VERBOSE_EXN
         if !debug_rewrite then
            begin
               eprintf "match_redex: %d%t" (List.length progs) eflush;
               List.iter (print_prog stderr) (prog::progs)
            end;
#endif
         match_redex_term addrs stack prog t;
         iter2 (match_redex_term addrs stack) progs tl
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
