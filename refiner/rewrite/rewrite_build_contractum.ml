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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
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
open Refine_error_sig

open Rewrite_util_sig
open Rewrite_debug_sig
open Rewrite_types

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
    with module TermTypes = TermType)
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
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack)
   (RewriteDebug : RewriteDebugSig
    with type rwterm = MakeRewriteTypes(TermType)(TermAddr).rwterm
    with type stack = MakeRewriteTypes(TermType)(TermAddr).stack
    with type varname = MakeRewriteTypes(TermType)(TermAddr).varname)
=
struct
   module RewriteTypes = MakeRewriteTypes(TermType)(TermAddr)
   open TermType
   open Term
   open TermMan
   open TermAddr
   open TermSubst
   open RewriteTypes
   open RewriteDebug

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
   let build_bname =
      let null_var = Lm_symbol.add "v" in
      fun names bnames stack -> function
      StackName i ->
         begin
            IFDEF VERBOSE_EXN THEN
               if !debug_rewrite then
                  eprintf "StackName %d (avoid [%a])%t" i output_symbol_set bnames eflush
            ENDIF;
            let v =
               match stack.(i) with
                  StackString s -> Lm_symbol.add s
                | StackVar v -> v
                | StackVoid -> null_var
                | _ ->
                     raise(Invalid_argument("Rewrite_build_contractum.build_bname: stack entry is not a string"))
            in
               if SymbolSet.mem bnames v then
                  let v = new_name v (SymbolSet.mem bnames) in
                     stack.(i) <- StackVar v;
                     v
               else
                  v
         end
    | SaveName i ->
         IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then
               eprintf "SaveName %d (%a, avoid [%a])%t" i output_symbol names.(i) output_symbol_set bnames eflush
         ENDIF;
         let v = names.(i) in
            if SymbolSet.mem bnames v then
               let v = new_name v (SymbolSet.mem bnames) in
                  names.(i) <- v;
                  v
            else
               v

   (*
    * Append the var array.
    *)
   let append_vars bvars vars =
      Array.append bvars (Array.of_list vars)

   (*
    * Perform a substitution on a sequence of hyps.
    *)
   let subst_hyp terms vars = function
      Hypothesis (v, term) ->
         Hypothesis (v, subst term vars terms)
    | Context (v, conts, subterms) ->
         Context (v, conts, List.map (fun t -> subst t vars terms) subterms)

   let hyp_subst arg terms vars =
      match vars, terms with
         [], [] ->
            arg
       | [v], [t] when is_var_term t && dest_var t = v ->
            arg
       | _ ->
            let i, len, hyps = arg in
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

    | RWSOInstance(i, []) ->
         begin
            match stack.(i) with
               StackBTerm(term, []) -> term
             | _ -> raise(Invalid_argument("Rewrite_build_contractum.build_contractum_term: stack entry is not valid"))
         end
    | RWSOInstance(i, terms) ->
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
                         eprintf "RWSOInstance 2: %a%t" debug_print term eflush;
                         List.iter2 (fun name term ->
                               eprintf "\t%a: %a%t" output_symbol name debug_print term eflush) (**)
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
                           eprintf "RWSOInstance: BTerm: %a: [%a]%t" debug_print term output_symbol_list vars eflush
                     ENDIF;
                     subst term vars
                | _ ->
                     raise(Invalid_argument("Rewrite_build_contractum.build_contractum_term: stack entry is not valid"))
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
                                     eprintf "\t%a: %a%t" output_symbol name debug_print term eflush) (**)
                                  vars terms
                            end
                      ENDIF;
                   subst term vars terms
              | _ ->
                   raise(Invalid_argument("Rewrite_build_contractum.build_contractum_term: stack entry is not valid"))
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
               StackString s ->
                  mk_var_term (Lm_symbol.add s)
             | StackVar v ->
                  mk_var_term v
             | _ ->
                  raise(Invalid_argument("Rewrite_build_contractum.build_contractum_term: stack entry is not valid"))
         end

    | t ->
         raise(Invalid_argument("Rewrite_build_contractum.build_contractum_term: stack entry is not valid"))

   and build_con_exn = Invalid_argument("Rewrite_build_contractum.build_contractum_param: stack entry is not valid")

   and raise_param _ =
      raise(Invalid_argument("Rewrite_build_contractum.build_contractum_param: parameter mismatch"))

   and build_contractum_level stack l = function
      { rw_le_var = v; rw_le_offset = o } :: t ->
         let l' =
            match stack.(v) with
               StackLevel l' -> l'
             | StackVar v -> mk_var_level_exp v
             | _ -> raise(build_con_exn)
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
    | RWQuote ->
         Quote
    | RWMNumber i ->
         begin
             match stack.(i) with
                StackNumber j -> Number j
              | StackString s -> Number (Lm_num.num_of_string s)
              | StackVar v -> MNumber v
              | t -> raise(build_con_exn)
         end
    | RWMString i ->
         begin
             match stack.(i) with
                StackString s -> String s
              | StackVar v -> MString v
              | StackNumber j -> String (Lm_num.string_of_num j)
              | t -> raise(build_con_exn)
         end
    | RWMToken i ->
         begin
             match stack.(i) with
                StackString s -> Token s
              | StackVar v -> MToken v
              | t -> raise(build_con_exn)
         end
    | RWMLevel1 i ->
         MLevel begin
             match stack.(i) with
                StackLevel l -> l
              | StackVar v -> mk_var_level_exp v
              | t -> raise(build_con_exn)
         end
    | RWMLevel2 { rw_le_const = c; rw_le_vars = vars } ->
         MLevel (build_contractum_level stack (mk_const_level_exp c) vars)
    | RWMVar i ->
         Var begin
             match stack.(i) with
                StackVar v -> v
              | StackString s -> Lm_symbol.add s
              | t -> raise(build_con_exn)
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
      { rw_bvars = vcount; rw_bnames = []; rw_bterm = term } ->
            mk_bterm [] (build_contractum_term names bnames stack bvars term)
    | { rw_bvars = vcount; rw_bnames = vars; rw_bterm = term } ->
         let vars' = List.map (build_bname names bnames stack) vars in
         let bnames' = SymbolSet.add_list bnames vars' in
         let bvars' = append_vars bvars vars' in
            mk_bterm vars' (build_contractum_term names bnames' stack bvars' term)

   and build_contractum_bterms names bnames stack bvars =
      List.map (build_contractum_bterm names bnames stack bvars)

   and build_contractum_sequent_hyps names bnames stack bvars parts hyps =
      match hyps with
         [] ->
            bnames, bvars, SeqHyp.collect (List.rev parts)
       | hyp :: hyps ->
            match hyp with
               RWSeqContextInstance (j, terms) ->
                  begin
                     IFDEF VERBOSE_EXN THEN
                        if !debug_rewrite then
                           eprintf "-RWSeqContextInstance (%d)%t" j eflush
                     ENDIF;
                     match stack.(j) with
                        StackSeqContext(vars, hyps') ->
                           let terms =
                              List.map (build_contractum_term names bnames stack bvars) terms
                           in
                              IFDEF VERBOSE_EXN THEN
                                 if !debug_rewrite then
                                    eprintf "+RWSeqContextInstance (%d%a)%t" j print_term_list terms eflush
                              ENDIF;
                              let i, len, hyps' = hyp_subst hyps' terms vars in
                              let part = Lm_array_util.ArrayArray (hyps', i, len) in
                                 build_contractum_sequent_hyps names bnames stack bvars (part :: parts) hyps
                      | _ ->
                           raise(Invalid_argument("Rewrite_build_contractum.build_contractum_sequent_hyps: stack entry is not valid"))
                  end
             | RWSeqHyp (v, hyp) ->
                  IFDEF VERBOSE_EXN THEN
                     if !debug_rewrite then
                        eprintf "RWSeqHyp: (%a)%t" print_varname v eflush
                  ENDIF;
                  let v = build_bname names bnames stack v in
                  let bnames = SymbolSet.add bnames v in
                  (*
                   * Strictly speaking, the build_contractum_term below should
                   * use the old bvars, not the new bvars. But since the
                   * the bvars arg only used to cause alpha-renaming of
                   * bterms, it does not hurt to pass the v in there
                   * as well and, possibly, avoid some potential name
                   * clashes.
                   *)
                  let hyp = build_contractum_term names bnames stack bvars hyp in
                  let bvars = append_vars bvars [v] in
                  let part = Lm_array_util.ArrayElement (Hypothesis (v, hyp)) in
                     build_contractum_sequent_hyps names bnames stack bvars (part :: parts) hyps
             | RWSeqContext _ | RWSeqFreeVarsContext _ ->
                  raise(Invalid_argument "Rewrite_build_contractum.build_contractum_sequent_hyps: found an invalid context")

   let build_contractum names bnames stack prog =
      IFDEF VERBOSE_EXN THEN
            if !debug_rewrite then begin
                  eprintf "Rewrite.build_contractum:\n%a%a%t" print_prog prog print_stack stack eflush;
               end
         ENDIF;
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
         new_name v (check vars bnames)
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
