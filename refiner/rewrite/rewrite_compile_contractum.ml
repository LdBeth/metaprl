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
   show_loading "Loading Rewrite_compile_contractum%t"

let debug_rewrite = load_debug "rewrite"

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
   (RewriteUtil : RewriteUtilSig
    with type term = TermType.term
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack)
   (RewriteDebug : RewriteDebugSig
    with type rstack = MakeRewriteTypes(TermType)(TermAddr).rstack)
   =
struct
   module RewriteTypes = MakeRewriteTypes(TermType)(TermAddr)
   open TermType
   open Term
   open TermMan
   open RefineError
   open RewriteTypes
   open RewriteUtil
   open RewriteDebug

   type strict = RewriteTypes.strict
   type rwterm = RewriteTypes.rwterm

   let compile_bname strict enames stack n =
      if array_rstack_fo_mem n stack then
         enames, StackName (array_rstack_fo_index n stack)
      else if strict = Relaxed && array_rstack_mem n stack then
         enames, StackName (array_rstack_index n stack)
      else
         (n :: enames), SaveName (List.length enames)

   let rec compile_bnames strict enames stack = function
      bname :: bnames ->
         let enames, bname = compile_bname strict enames stack bname in
         let enames, bnames = compile_bnames strict enames stack bnames in
            enames, bname :: bnames
    | [] ->
         enames, []

   let check_cont c bconts v =
      if not (List.mem v bconts) then
         REF_RAISE(RefineError("Rewrite_compile_contractum",RewriteFreeContextVar(v,c)))

   let param_error = "Rewrite_compile_contractum.compile_so_contractum_param"

   let rec compile_so_contractum_term
          (strict : strict)
          (enames : var list)
          (stack : rstack array)
          (bconts : var list)
          (bvars : var list)
          (term : term) =
      if is_var_term term then
         let v = dest_var term in
            if List.mem v bvars then
               enames, RWCheckVar(Lm_list_util.find_rindex v bvars)
            else if array_rstack_freefo_mem v stack then
               enames, RWStackVar(array_rstack_freefo_index v stack)
            else (* XXX: Abusing RewriteFreeSOVar here *)
               REF_RAISE(RefineError ("compile_so_redex_term", RewriteFreeSOVar v))
      else if is_so_var_term term then
         let v, conts, subterms = dest_so_var term in
            if List.mem v bvars then
               REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_term", RewriteBoundSOVar v))
            else if array_rstack_so_mem v stack then begin
               (*
                * This is a second order variable.
                * The variable v should be bound, and we generate a
                * a substitution instance.  Check that the subterm counts
                * match.
                *)
               List.iter (check_cont v bconts) conts;
               let index = array_rstack_so_index v stack in
               check_arity v conts (List.length subterms) stack.(index);
               let enames, subterms = compile_so_contractum_terms strict enames stack bconts bvars subterms in
                  enames, RWSOInstance(index, subterms)

            end else if (strict = Relaxed) &&
                    (array_rstack_mem v stack) &&
                    (conts = [] && subterms = []) then
               (*
                * Display forms:
                * convert a stack element into a variable representation
                *)
               enames, RWStackVar (array_rstack_index v stack)

            else
               (* This is a second order variable that is free *)
               REF_RAISE(
                  RefineError ("Rewrite_compile_contractum.compile_so_contractum_term",
                     if (!debug_rewrite) && (array_rstack_mem v stack) then
                        StringVarError("SO Var has stack item " ^ rstack_item_str stack.(array_rstack_index v stack) ^ " and bvars are [" ^ (String.concat "; " (List.map string_of_symbol bvars)) ^ "]", v)
                     else
                        RewriteFreeSOVar v
                  )
               )

      else if is_context_term term then
         (* This is a second order context *)
         let v, term', conts, subterms = dest_context term in
            if array_rstack_c_mem v stack then begin
               (*
                * Second order context and the v is bound.
                * We generate a substitution instance.
                *)
               List.iter (check_cont v bconts) conts;
               let index = array_rstack_c_index v stack in
               check_arity v conts (List.length subterms) stack.(index); (* TODO: check_arity does not support contexts yet *)
               let enames, term' = compile_so_contractum_term strict enames stack bconts bvars term' in
               let enames, subterms = compile_so_contractum_terms strict enames stack (v::bconts) bvars subterms in
                 enames, RWSOContextSubst(index, term', subterms)

            end else
               (* Free second order context *)
               REF_RAISE(RefineError ("Rewrite_compile_contractum.is_context_term", RewriteFreeSOVar v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         compile_so_contractum_sequent strict enames stack bconts bvars term

      else
         (* This is a normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let enames, bterms' = compile_so_contractum_bterms strict enames stack bconts bvars bterms in
         if are_sparams params then
               enames, RWCompositeSimple { rws_op = op; rws_bterms = bterms' }
         else
            let params' = List.map (compile_so_contractum_param strict stack) params in
               enames, RWComposite { rw_op = { rw_name = name; rw_params = params' };
                                     rw_bterms = bterms'
                                   }

   and compile_so_contractum_terms strict enames stack bconts bvars = function
      term :: terms ->
         let enames, term = compile_so_contractum_term strict enames stack bconts bvars term in
         let enames, terms = compile_so_contractum_terms strict enames stack bconts bvars terms in
            enames, term :: terms
    | [] ->
         enames, []

   (*
    * We also compile parameters, and bind meta-variables.
    *)
   and compile_so_contractum_param strict stack param =
      match dest_param param with
         MNumber v ->
            if array_rstack_p_mem ShapeNumber v stack then
               (* New param *)
               RWMNumber (array_rstack_p_index ShapeNumber v stack)
            else if strict = Relaxed && array_rstack_mem v stack then
               RWMNumber (array_rstack_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MString v ->
            if array_rstack_p_mem ShapeString v stack then
               (* New param *)
               RWMString (array_rstack_p_index ShapeString v stack)
            else if strict = Relaxed && array_rstack_mem v stack then
               RWMString (array_rstack_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MToken v ->
            if array_rstack_p_mem ShapeToken v stack then
               (* New param *)
               RWMToken (array_rstack_p_index ShapeToken v stack)
            else if strict = Relaxed && array_rstack_mem v stack then
               RWMToken (array_rstack_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MLevel l ->
            let { le_const = c; le_vars = vars } = dest_level l in
            let vars = List.map dest_level_var vars in
               (match c, vars with
                  0, [{ le_var = v; le_offset = 0 }] ->
                      if array_rstack_p_mem ShapeLevel v stack then
                         RWMLevel1 (array_rstack_p_index ShapeLevel v stack)
                      else
                         REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))
                 | _ ->
                      let collect { le_var = v; le_offset = off } =
                         if array_rstack_p_mem ShapeLevel v stack then
                            { rw_le_var = array_rstack_p_index ShapeLevel v stack;
                              rw_le_offset = off
                            }
                         else
                            REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))
                      in
                         RWMLevel2 { rw_le_const = c;
                                     rw_le_vars = List.map collect vars
                         })

       | Var v ->
            if strict == Relaxed && array_rstack_mem v stack then
               if array_rstack_p_mem ShapeVar v stack then
                  RWMVar(array_rstack_p_index ShapeVar v stack)
               else
                  RWMVar(array_rstack_index v stack)
            else
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | Number i -> RWNumber i
       | String s -> RWString s
       | Token t -> RWToken t
       | Quote -> RWQuote

       | ObId id ->
            RWObId id

       | ParamList l ->
            RWParamList (List.map (compile_so_contractum_param strict stack) l)

   (*
    * Tests whether params can be left as is.
    *)

   and is_sparam = function
      Number _ | String _ | Token _ | Quote ->
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
   and compile_so_contractum_bterm strict enames stack bconts bvars bterm =
      let { bvars = vars; bterm = term } = dest_bterm bterm in
      let enames, term' = compile_so_contractum_term strict enames stack bconts (bvars @ vars) term in
      let enames, vars' = compile_bnames strict enames stack vars in
         enames, { rw_bvars = List.length vars; rw_bnames = vars'; rw_bterm = term' }

   and compile_so_contractum_bterms strict enames stack bconts bvars = function
      bterm :: bterms ->
         let enames, bterm = compile_so_contractum_bterm strict enames stack bconts bvars bterm in
         let enames, bterms = compile_so_contractum_bterms strict enames stack bconts bvars bterms in
            enames, bterm :: bterms
    | [] ->
         enames, []

   (*
    * Contexts are handled specially inside sequents.
    *)
   and compile_so_contractum_sequent strict enames (stack : rstack array) bconts bvars term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
      let enames, arg = compile_so_contractum_term strict enames stack bconts bvars arg in
      let enames, hyps, goals =
         compile_so_contractum_sequent_inner strict enames stack bconts bvars 0 (SeqHyp.length hyps) hyps goals
      in
         enames, RWSequent (arg, hyps, goals)

   and compile_so_contractum_sequent_inner strict enames stack bconts bvars i len hyps goals =
      if i = len then
         let enames, goals =
            compile_so_contractum_goals strict enames stack bconts bvars 0 (SeqGoal.length goals) goals
         in
            enames, [], goals
      else
         match SeqHyp.get hyps i with
            Context (v, conts, subterms) ->
               (* This is a second order context *)
               if array_rstack_c_mem v stack then begin
                  (*
                   * Second order context and the v is bound.
                   * We generate a substitution instance.
                   *)
                  List.iter (check_cont v bconts) conts;
                  let enames, subterms =
                     compile_so_contractum_terms strict enames stack bconts bvars subterms
                  in
                  let term = RWSeqContextInstance (array_rstack_c_index v stack, subterms) in
                  let enames, hyps, goals =
                     compile_so_contractum_sequent_inner strict enames stack (v::bconts) bvars (i + 1) len hyps goals
                  in
                     enames, term :: hyps, goals
               end else
                  (* Free second order context *)
                  REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_hyp", RewriteFreeSOVar v))

          | Hypothesis (v, term) ->
               if List.mem v bvars then
                  REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_hyp", StringVarError("double binding", v)));
               let enames, term = compile_so_contractum_term strict enames stack bconts bvars term in
               let enames, v' = compile_bname strict enames stack v in
               let enames, hyps, goals =
                  compile_so_contractum_sequent_inner strict enames stack bconts (bvars @ [v]) (i + 1) len hyps goals
               in
               let hyp = RWSeqHyp (v', term) in
                  enames, hyp :: hyps, goals

   and compile_so_contractum_goals strict enames stack bconts bvars i len goals =
      if i = len then
         enames, []
      else
         let enames, goal = compile_so_contractum_term strict enames stack bconts bvars (SeqGoal.get goals i) in
         let enames, goals = compile_so_contractum_goals strict enames stack bconts bvars (i + 1) len goals in
            enames, goal :: goals

   (*
    * Toplevel compilation.
    *)
   let compile_so_contractum strict stack term =
      let enames, term = compile_so_contractum_term strict [] stack [] [] term in
         Array.of_list (List.rev enames), term

   let compile_so_contracta strict stack terms =
      let rec compile enames stack = function
         term :: terms ->
            let enames, term = compile_so_contractum_term strict enames stack [] [] term in
            let enames, terms = compile enames stack terms in
               enames, term :: terms
       | [] ->
            enames, []
      in
      let enames, terms = compile [] stack terms in
         Array.of_list (List.rev enames), terms
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)

