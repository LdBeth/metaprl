(*
 * Build the contractum, given a matched redex.
 * The parts of the redex are sorted in the stack.
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
   show_loading "Loading Rewrite_build_contractum%t"

let debug_rewrite = load_debug "rewrite"
let debug_subst = load_debug "subst"

module MakeRewriteBuildContractum
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
    with type level_exp = TermType.level_exp
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
    with type object_id = TermType.object_id
    with type term = TermType.term
    with type operator = TermType.operator
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
   type stack = RewriteTypes.stack
   type rwterm = RewriteTypes.rwterm

   let rec print_term_list out = function
      term :: terms ->
         output_string out ", ";
         debug_print out term;
         print_term_list out terms
    | [] ->
         ()

   (*
    * The contractum is built as a second order substitution.
    * For variable renaming, we keep track of the variable
    * and the name it has been renamed to.  Whenever a second
    * order term is instantiated, we do a calculation of
    * the variables to be renamed, and send it back up.
    *
    * In this function, we map variable names.
    * bnames is the name of the current bound variables.
    * names is the list of argument names.
    *)
   let build_bname names bnames stack = function
      ArgName i ->
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "ArgName %d%t" i eflush
         ENDIF;
         let v = names.(i) in
         let rec check v = function
            vars :: tl ->
               if List.mem v vars then
                  REF_RAISE(RefineError ("build_bname", RewriteBoundSOVar v))
               else
                  check v tl
          | [] ->
               ()
         in
            check v bnames;
            v
    | StackName i ->
         begin
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "StackName %d%t" i eflush
            ENDIF;
            match stack.(i) with
               StackString s ->
                  s
             | x ->
                  REF_RAISE(RefineError ("build_bname", RewriteStringError "stack entry is not a string"))
         end
    | SaveName i ->
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "SaveName %d%t" i eflush
         ENDIF;
         names.(i)

   (*
    * Append the var array.
    *)
   let append_vars bvars vars =
      Array.append bvars (Array.of_list vars)

   (*
    * Perform a substitution on a sequence of hyps.
    * We explicitely avoid capture.
    *)
   let rec collect_hyp_vars vars hyps i len =
      if len = 0 then
         vars
      else
         match SeqHyp.get hyps i with
            Hypothesis (v, _) ->
               collect_hyp_vars (v :: vars) hyps (i + 1) (len - 1)
          | Context _ ->
               collect_hyp_vars vars hyps (i + 1) (len - 1)

   let avoid_capture hyps i len terms =
      let hyp_vars = collect_hyp_vars [] hyps i len in
         if is_some_var_free_list hyp_vars terms then
            begin
               IFDEF VERBOSE_EXN THEN
                  if !debug_rewrite then
                     eprintf "avoid_capture: capture occurred: %a/%a%t" (**)
                        print_string_list hyp_vars
                        print_string_list (free_vars_terms terms)
                        eflush
               ENDIF;
               REF_RAISE(RefineError ("avoid_capture", StringError "invalid substituion"))
            end

   let subst_hyp terms vars = function
      Hypothesis (v, term) ->
         Hypothesis (v, subst term vars terms)
    | Context (v, subterms) ->
         Context (v, List.map (fun t -> subst t vars terms) subterms)

   let hyp_subst arg terms vars =
      match vars, terms with
         [], [] ->
            arg
       | [v], [t] when is_var_term t && dest_var t = v ->
            arg
       | _ ->
            let i, len, hyps = arg in
               avoid_capture hyps i len terms;
               0, len, SeqHyp.lazy_sub_map (subst_hyp terms vars) hyps i len

   (*
    * Build the terms.
    *    names: variable names provided by argument
    *    bnames: string list list of all the bound variables in the term
    *    stack: stack compiled from match_redex
    *    bvars: array of bound vars explicit in the contractum.
    *    con: the contractum being constructed.
    *)
   let rec build_contractum_term names bnames stack bvars = function
      RWComposite { rw_op = { rw_name = name; rw_params = params }; rw_bterms = bterms } ->
         (* Build a regular term from the parts *)
         mk_term (mk_op name (build_contractum_params stack params)) (**)
            (build_contractum_bterms names bnames stack bvars bterms)
    | RWCompositeSimple { rws_op = op; rws_bterms = bterms } ->
         (* Build a regular term from the parts *)
         mk_term op (build_contractum_bterms names bnames stack bvars bterms)
    | RWSequent (arg, hyps, goals) ->
         let arg = build_contractum_term names bnames stack bvars arg in
         let bnames, bvars, hyps = build_contractum_sequent_hyps names bnames stack bvars [] hyps in
         let goals = List.map (build_contractum_term names bnames stack bvars) goals in
         let seq =
            { sequent_args = arg;
              sequent_hyps = hyps;
              sequent_goals = SeqGoal.of_list goals
            }
         in
            mk_sequent_term seq

    | RWSOSubst(i, []) ->
         begin
            match stack.(i) with
               StackBTerm(term, []) -> term
             | _ -> REF_RAISE(RefineError ("build_contractum_term", RewriteStringError "stack entry is not valid"))
         end
    | RWSOSubst(i, terms) ->
         begin
             (*
              * Instantiate a second order term.
              * Find its free variables, and rename the binding stack
              * if necessary.
              *)
            let subst term vars =
               let terms = List.map (build_contractum_term names bnames stack bvars) terms in
               IFDEF VERBOSE_EXN THEN
                  if !debug_subst then
                      begin
                         eprintf "RWSOSubst2: %a%t" debug_print term eflush;
                         List.iter2 (fun name term ->
                               eprintf "\t%s: %a%t" name debug_print term eflush) (**)
                            vars terms
                      end
               ENDIF;
               let term = subst term vars terms in
                  IFDEF VERBOSE_EXN THEN
                     if !debug_subst then
                        eprintf "\t%a%t" debug_print term eflush
                  ENDIF;
                  term
            in
               match stack.(i) with
                  StackBTerm(term, vars) ->
                     IFDEF VERBOSE_EXN THEN
                        if !debug_subst then
                           eprintf "RWSOSubst: BTerm: %a: %a%t" debug_print term print_string_list vars eflush
                     ENDIF;
                     subst term vars
                | _ ->
                     REF_RAISE(RefineError ("build_contractum_term", RewriteStringError "stack entry is not valid"))
         end

    | RWSOContextSubst(i, t, terms) ->
         begin
             (*
              * Instantiate a context.
              *)
             match stack.(i) with
                StackContext(vars, term, addr) ->
                   let term =
                      replace_subterm term addr (**)
                         (build_contractum_term names bnames stack bvars t)
                   in
                   let terms = List.map (build_contractum_term names bnames stack bvars) terms in
                      IFDEF VERBOSE_EXN THEN
                         if !debug_subst then
                            begin
                               eprintf "RWSOContextSubst: %a%t" debug_print term eflush;
                               List.iter2 (fun name term ->
                                     eprintf "\t%s: %a%t" name debug_print term eflush) (**)
                                  vars terms
                            end
                      ENDIF;
                   subst term vars terms
              | _ ->
                   REF_RAISE(RefineError ("build_contractum_term", RewriteStringError "stack entry is not valid"))
         end

    | RWCheckVar i ->
         (*
          * This is a bound occurrence.
          *)
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "RWCheckVar: %d/%d%t" i (Array.length bvars) eflush
         ENDIF;
         mk_var_term bvars.(i)

    | RWStackVar i ->
         (*
          * This is a bound occurrence.
          *)
         begin
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "RWStackVar: %d%t" i eflush
            ENDIF;
            match stack.(i) with
               StackString s
             | StackMString s ->
                  mk_var_term s
             | _ ->
                  REF_RAISE(RefineError ("build_contractum_term", RewriteStringError "stack entry is not valid"))
         end

    | t ->
         REF_RAISE(RefineError ("build_contractum_term", RewriteStringError "bad contractum"))

   and build_con_exn = RefineError ("build_contractum_param", RewriteStringError "stack entry is not valid")

   and raise_param p =
      REF_RAISE(RefineError ("build_contractum_param", RewriteBadMatch (ParamMatch (make_param p))))

   and build_contractum_level stack l = function
      { rw_le_var = v; rw_le_offset = o } :: t ->
         let l' =
            match stack.(v) with
               StackLevel l' -> l'
             | StackMString s -> mk_var_level_exp s
             | _ -> REF_RAISE(build_con_exn)
         in
            build_contractum_level stack (max_level_exp l l' o) t
    | [] ->
         l

   and build_contractum_param stack = function
      RWNumber i ->
         Number i
    | RWString s ->
         String s
    | RWToken s ->
         Token s
    | RWVar v ->
         Var v
    | RWMNumber i ->
         begin
             match stack.(i) with
                StackNumber j -> Number j
              | StackMString s -> MNumber s
              | t -> REF_RAISE(build_con_exn)
         end
    | RWMString i ->
         begin
             match stack.(i) with
                StackString s -> String s
              | StackMString s -> MString s
              | t -> REF_RAISE(build_con_exn)
         end
    | RWMToken i ->
         begin
             match stack.(i) with
                StackString s -> Token s
              | StackMString s -> MToken s
              | t -> REF_RAISE(build_con_exn)
         end
    | RWMLevel1 i ->
         begin
             match stack.(i) with
                StackLevel l -> MLevel l
              | StackMString s -> MLevel (mk_var_level_exp s)
              | t -> REF_RAISE(build_con_exn)
         end
    | RWMLevel2 { rw_le_const = c; rw_le_vars = vars } ->
         MLevel (build_contractum_level stack (mk_const_level_exp c) vars)
    | RWMVar i ->
         begin
             match stack.(i) with
                StackString v -> Var v
              | StackMString s -> MVar s
              | t -> REF_RAISE(build_con_exn)
         end
    | RWObId id ->
         ObId id
    | RWParamList l ->
         ParamList (build_contractum_params stack l)

   and build_contractum_params stack params =
      let build_contractum_param' p =
         make_param (build_contractum_param stack p)
      in
         List.map build_contractum_param' params

   and build_contractum_bterm names bnames stack bvars = function
      { rw_bvars = vcount; rw_bnames = vars; rw_bterm = term } ->
         let vars' = List.map (build_bname names bnames stack) vars in
            mk_bterm vars' (build_contractum_term names (vars' :: bnames) stack (append_vars bvars vars') term)

   and build_contractum_bterms names bnames stack bvars =
      List.map (build_contractum_bterm names bnames stack bvars)

   and build_contractum_sequent_hyps names bnames stack bvars parts hyps =
      match hyps with
         [] ->
            bnames, bvars, SeqHyp.collect (List.rev parts)
       | hyp :: hyps ->
            match hyp with
               RWSeqContextSubst (j, terms) ->
                  begin
                     IFDEF VERBOSE_EXN THEN
                        if !debug_rewrite then
                           eprintf "-RWSeqContextSubst (%d)%t" j eflush
                     ENDIF;
                     match stack.(j) with
                        StackSeqContext(vars, hyps') ->
                           let terms =
                              List.map (build_contractum_term names bnames stack bvars) terms
                           in
                              IFDEF VERBOSE_EXN THEN
                                 if !debug_rewrite then
                                    eprintf "+RWSeqContextSubst (%d%a)%t" j print_term_list terms eflush
                              ENDIF;
                              let i, len, hyps' = hyp_subst hyps' terms vars in
                              let part = Array_util.ArrayArray (hyps', i, len) in
                                 build_contractum_sequent_hyps names bnames stack bvars (part :: parts) hyps
                      | _ ->
                           REF_RAISE(RefineError
                                     ("build_contractum_sequent_hyps",
                                      RewriteStringError "stack entry is not valid"))
                  end
             | RWSeqHyp (v, hyp) ->
                  IFDEF VERBOSE_EXN THEN
                     if !debug_rewrite then
                        eprintf "RWSeqHyp: (%a)%t" print_varname v eflush
                  ENDIF;
                  let hyp = build_contractum_term names bnames stack bvars hyp in
                  let v = build_bname names bnames stack v in
                  let bnames = [v] :: bnames in
                  let bvars = append_vars bvars [v] in
                  let part = Array_util.ArrayElement (Hypothesis (v, hyp)) in
                     build_contractum_sequent_hyps names bnames stack bvars (part :: parts) hyps
             | RWSeqContext _ | RWSeqFreeVarsContext _ ->
                  raise(Invalid_argument "build_contractum_sequent_hyps: found an invalid context")

   let build_contractum names bnames stack prog =
      build_contractum_term names bnames stack [||] prog

   let rec check_bnames v = function
      bnames :: tl ->
         if List.mem v bnames then
            true
         else
            check_bnames v tl
    | [] ->
         false

   let check vars bnames v =
      List.mem v vars or check_bnames v bnames

   let var_name vars bnames v =
      if check vars bnames v then
         String_util.vnewname v (check vars bnames)
      else
         v

   let contracta_enames vars bnames enames =
      Array.map (var_name vars bnames) enames

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
