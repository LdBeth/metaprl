(*
 * Compile a contractum, given a stack from a redex that has
 * already been compiled.
 *
 * When the contractum is compiled, the redex has already been
 * compiled, and the stack contains a list of the special variables.
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
   show_loading "Loading Rewrite_compile_contractum%t"

module MakeRewriteCompileContractum
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
    with type object_id = TermType.object_id
    with type term = TermType.term
    with type operator = TermType.operator
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
   type strict = RewriteTypes.strict = Strict | Relaxed

   let compile_bname names enames stack n =
      if Array_util.mem n names then
         enames, ArgName (Array_util.index n names)
      else if array_rstack_fo_mem n stack then
         enames, StackName (array_rstack_fo_index n stack)
      else
         (n :: enames), SaveName (List.length enames + Array.length names)

   let rec compile_bnames names enames stack = function
      bname :: bnames ->
         let enames, bname = compile_bname names enames stack bname in
         let enames, bnames = compile_bnames names enames stack bnames in
            enames, bname :: bnames
    | [] ->
         enames, []

   let param_error = "Rewrite_compile_contractum.compile_so_contractum_param"

   let rec compile_so_contractum_term
          (strict : strict)
          (names : string array)
          (enames : string list)
          (stack : rstack array)
          (bvars : string list)
          (term : term) =
      if is_so_var_term term then
         let v, subterms = dest_so_var term in
            (* This is a first or second order variable *)
            if List.mem v bvars then
               (* This is a first order variable instance *)
               if subterms <> [] then
                  REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_term", RewriteBoundSOVar v))
               else
                  enames, RWCheckVar(List_util.find_index v bvars)

            else if array_rstack_so_mem v stack then
               (*
                * This is a second order variable.
                * The variable v should be bound, and we generate a
                * a substitution instance.  Check that the subterm counts
                * match.
                *)
               let index = array_rstack_so_index v stack in
               let v' = stack.(index) in
               let enames, subterms = compile_so_contractum_terms strict names enames stack bvars subterms in
                  enames, RWSOSubst(array_rstack_so_index v stack, subterms)

            else if array_rstack_fo_mem v stack & subterms = [] then
               (* This variable represents a binding occurrence *)
               enames, RWStackVar (array_rstack_fo_index v stack)

            else
               (* This is a second order variable that is free *)
               REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_term", RewriteFreeSOVar v))

      else if is_context_term term then
         (* This is a second order context *)
         let v, term', subterms = dest_context term in
            if array_rstack_c_mem v stack then
               (*
                * Second order context and the v is bound.
                * We generate a substitution instance.
                *)
               let enames, term' = compile_so_contractum_term strict names enames stack bvars term' in
               let enames, subterms = compile_so_contractum_terms strict names enames stack bvars subterms in
                 enames, RWSOContextSubst(array_rstack_c_index v stack, term', subterms)

            else
               (* Free second order context *)
               REF_RAISE(RefineError ("Rewrite_compile_contractum.is_context_term", RewriteFreeSOVar v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         compile_so_contractum_sequent strict names enames stack bvars term

      else
         (* This is a normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let enames, bterms' = compile_so_contractum_bterms strict names enames stack bvars bterms in
         if are_sparams params then
               enames, RWCompositeSimple { rws_op = op; rws_bterms = bterms' }
         else
            let params' = List.map (compile_so_contractum_param stack) params in
               enames, RWComposite { rw_op = { rw_name = name; rw_params = params' };
                                     rw_bterms = bterms'
                                   }

   and compile_so_contractum_terms strict names enames stack bvars = function
      term :: terms ->
         let enames, term = compile_so_contractum_term strict names enames stack bvars term in
         let enames, terms = compile_so_contractum_terms strict names enames stack bvars terms in
            enames, term :: terms
    | [] ->
         enames, []

   (*
    * We also compile parameters, and bind meta-variables.
    *)
   and compile_so_contractum_param stack param =
      match dest_param param with
         MNumber v ->
            if array_rstack_p_mem v stack then
               (* New param *)
               RWMNumber (array_rstack_p_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MString v ->
            if array_rstack_p_mem v stack then
               (* New param *)
               RWMString (array_rstack_p_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MToken v ->
            if array_rstack_p_mem v stack then
               (* New param *)
               RWMToken (array_rstack_p_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MLevel l ->
            let { le_const = c; le_vars = vars } = dest_level l in
            let vars = List.map dest_level_var vars in
               (match c, vars with
                  0, [{ le_var = v; le_offset = 0 }] ->
                      if array_rstack_p_mem v stack then
                         RWMLevel1 (array_rstack_p_index v stack)
                      else
                         REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))
                 | _ ->
                      let collect { le_var = v; le_offset = off } =
                         if array_rstack_p_mem v stack then
                            { rw_le_var = array_rstack_p_index v stack; rw_le_offset = off }
                         else
                            REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))
                      in
                         RWMLevel2 { rw_le_const = c;
                                     rw_le_vars = List.map collect vars
                         })

       | MVar v ->
            if array_rstack_p_mem v stack then
               (* New param *)
               RWMVar(array_rstack_p_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | Number i -> RWNumber i
       | String s -> RWString s
       | Token t -> RWToken t
       | Var v -> RWVar v

       | ObId id ->
            RWObId id

       | ParamList l ->
            RWParamList (List.map (compile_so_contractum_param stack) l)

       | BackwardsCompatibleLevel _ ->
            REF_RAISE(RefineError (param_error, StringError "BackwardsCompatibleLevel"))

   (*
    * Tests whether params can be left as is.
    *)

   and is_sparam = function
      Number _ | String _ | Token _ | Var _ ->
         true
    | ParamList l ->
         are_sparams l
    | _ ->
         false

   and are_sparams = function
      [] ->
         true
    | p :: pl ->
         is_sparam (dest_param p) && are_sparams pl

   (*
    * In bterms, have to add these vars to the binding stack.
    *)
   and compile_so_contractum_bterm strict names enames stack bvars bterm =
      let { bvars = vars; bterm = term } = dest_bterm bterm in
      let enames, term' = compile_so_contractum_term strict names enames stack (bvars @ vars) term in
      let enames, vars' = compile_bnames names enames stack vars in
         enames, { rw_bvars = List.length vars; rw_bnames = vars'; rw_bterm = term' }

   and compile_so_contractum_bterms strict names enames stack bvars = function
      bterm :: bterms ->
         let enames, bterm = compile_so_contractum_bterm strict names enames stack bvars bterm in
         let enames, bterms = compile_so_contractum_bterms strict names enames stack bvars bterms in
            enames, bterm :: bterms
    | [] ->
         enames, []

   (*
    * Contexts are handled specially inside sequents.
    *)
   and compile_so_contractum_sequent strict names enames (stack : rstack array) bvars term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
      let enames, arg = compile_so_contractum_term strict names enames stack bvars arg in
      let enames, hyps, goals =
         compile_so_contractum_sequent_inner strict names enames stack bvars 0 (SeqHyp.length hyps) hyps goals
      in
         enames, RWSequent (arg, hyps, goals)

   and compile_so_contractum_sequent_inner strict names enames stack bvars i len hyps goals =
      if i = len then
         let enames, goals =
            compile_so_contractum_goals strict names enames stack bvars 0 (SeqGoal.length goals) goals
         in
            enames, [], goals
      else
         match SeqHyp.get hyps i with
            Context (v, subterms) ->
               (* This is a second order context *)
               if array_rstack_c_mem v stack then
                  (*
                   * Second order context and the v is bound.
                   * We generate a substitution instance.
                   *)
                  let enames, subterms =
                     compile_so_contractum_terms strict names enames stack bvars subterms
                  in
                  let term = RWSeqContextSubst (array_rstack_c_index v stack, subterms) in
                  let enames, hyps, goals =
                     compile_so_contractum_sequent_inner strict names enames stack bvars (i + 1) len hyps goals
                  in
                     enames, term :: hyps, goals
               else
                  (* Free second order context *)
                  REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_hyp", RewriteFreeSOVar v))

          | Hypothesis (v, term) ->
               let enames, term = compile_so_contractum_term strict names enames stack bvars term in
               let enames, v' = compile_bname names enames stack v in
               let enames, hyps, goals =
                  compile_so_contractum_sequent_inner strict names enames stack (bvars @ [v]) (i + 1) len hyps goals
               in
               let hyp = RWSeqHyp (v', term) in
                  enames, hyp :: hyps, goals

   and compile_so_contractum_goals strict names enames stack bvars i len goals =
      if i = len then
         enames, []
      else
         let enames, goal = compile_so_contractum_term strict names enames stack bvars (SeqGoal.get goals i) in
         let enames, goals = compile_so_contractum_goals strict names enames stack bvars (i + 1) len goals in
            enames, goal :: goals

   (*
    * Toplevel compilation.
    *)
   let compile_so_contractum strict names stack term =
      let enames, term = compile_so_contractum_term strict names [] stack [] term in
         Array.of_list (List.rev enames), term

   let compile_so_contracta strict names stack terms =
      let rec compile names enames stack = function
         term :: terms ->
            let enames, term = compile_so_contractum_term strict names enames stack [] term in
            let enames, terms = compile names enames stack terms in
               enames, term :: terms
       | [] ->
            enames, []
      in
      let enames, terms = compile names [] stack terms in
         Array.of_list (List.rev enames), terms
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)

