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
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and
 * California Institute of Technology
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
   show_loading "Loading Rewrite_compile_contractum%t"

let debug_rewrite = load_debug "rewrite"

module MakeRewriteCompileContractum
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (RefineError : RefineErrorSig with module Types = TermType)
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

   type rwterm = RewriteTypes.rwterm

   let compile_bname strict stack n =
      if array_rstack_mem n stack then
         StackName (array_rstack_index n stack, n)
      else
         OldName n

   let rec compile_bnames strict stack = List.map (compile_bname strict stack)

   let check_cont c bconts v =
      if not (List.mem v bconts) then
         REF_RAISE(RefineError("Rewrite_compile_contractum",RewriteFreeContextVar(v,c)))

   let param_error = "Rewrite_compile_contractum.compile_so_contractum_param"

   let rec compile_so_contractum_term
          (strict : strict)
          (stack : rstack array)
          (bconts : var list)
          (bvars : var list)
          (term : term) =
      if is_var_term term then
         let v = dest_var term in
            if List.mem v bvars then
               RWCheckVar(Lm_list_util.find_rindex v bvars)
            else if array_rstack_freefo_mem v stack then
               RWStackVar(array_rstack_freefo_index v stack)
            else (* XXX: Abusing RewriteFreeSOVar here *)
               REF_RAISE(RefineError ("compile_so_redex_term", RewriteFreeSOVar v))
      else if is_so_var_term term then
         let v, conts, subterms = dest_so_var term in
            if array_rstack_so_pattern_mem v stack then begin
               (*
                * This is a second order variable.
                * The variable v should be bound, and we generate a
                * a substitution instance.  Check that the subterm counts
                * match.
                *)
               if strict = Strict then List.iter (check_cont v bconts) conts;
               let index = array_rstack_so_index v stack in
               check_arity v conts (List.length subterms) stack.(index);
               let subterms = compile_so_contractum_terms strict stack bconts bvars subterms in
                  RWSOInstance(index, subterms)

            end else if (strict = Relaxed) &&
                    (array_rstack_mem v stack) &&
                    (conts = [] && subterms = []) then
               (*
                * Display forms:
                * convert a stack element into a variable representation
                *)
               RWStackVar (array_rstack_index v stack)

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
               if strict = Strict then List.iter (check_cont v bconts) conts;
               let index = array_rstack_c_index v stack in
               check_arity v conts (List.length subterms) stack.(index);
               let term' = compile_so_contractum_term strict stack (v::bconts) bvars term' in
               let subterms = compile_so_contractum_terms strict stack bconts bvars subterms in
                 RWSOContextSubst(index, term', subterms)

            end else
               (* Free second order context *)
               REF_RAISE(RefineError ("Rewrite_compile_contractum (is_context_term)", RewriteFreeSOVar v))

      else if is_sequent_term term then
         (* Sequents are handled specially *)
         compile_so_contractum_sequent strict stack bconts bvars term

      else
         (* This is a normal term--not a var *)
         let { term_op = op; term_terms = bterms } = dest_term term in
         let { op_name = name; op_params = params } = dest_op op in
         let bterms' = compile_so_contractum_bterms strict stack bconts bvars bterms in
         if are_sparams params then
               RWCompositeSimple { rws_op = op; rws_bterms = bterms' }
         else
            let params' = List.map (compile_so_contractum_param strict stack) params in
               RWComposite { rw_op = { rw_name = name; rw_params = params' };
                                     rw_bterms = bterms'
                                   }

   and compile_so_contractum_terms strict stack bconts bvars = 
      List.map (compile_so_contractum_term strict stack bconts bvars)

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

       | MShape v ->
            if array_rstack_p_mem ShapeShape v stack then
               (* New param *)
               RWMShape (array_rstack_p_index ShapeShape v stack)
            else if strict = Relaxed && array_rstack_mem v stack then
               RWMShape (array_rstack_index v stack)
            else
               (* Free param *)
               REF_RAISE(RefineError (param_error, RewriteFreeParamVar v))

       | MOperator v ->
            if array_rstack_p_mem ShapeOperator v stack then
               (* New param *)
               RWMOperator (array_rstack_p_index ShapeOperator v stack)
            else if strict = Relaxed && array_rstack_mem v stack then
               RWMOperator (array_rstack_index v stack)
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
       | Shape s -> RWShape s
       | Operator o -> RWOperator o
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
   and compile_so_contractum_bterm strict stack bconts bvars bterm =
      let { bvars = vars; bterm = term } = dest_bterm bterm in
      let term' = compile_so_contractum_term strict stack bconts (bvars @ vars) term in
      let vars' = compile_bnames strict stack vars in
         { rw_bvars = List.length vars; rw_bnames = vars'; rw_bterm = term' }

   and compile_so_contractum_bterms strict stack bconts bvars = 
      List.map (compile_so_contractum_bterm strict stack bconts bvars)

   (*
    * Contexts are handled specially inside sequents.
    *)
   and compile_so_contractum_sequent strict (stack : rstack array) bconts bvars term =
      let { sequent_args = arg;
            sequent_hyps = hyps;
            sequent_concl = concl
          } = explode_sequent term
      in
      let arg = compile_so_contractum_term strict stack bconts bvars arg in
      let hyps, concl =
         compile_so_contractum_sequent_inner strict stack bconts bvars 0 (SeqHyp.length hyps) hyps concl
      in
         RWSequent (arg, hyps, concl)

   and compile_so_contractum_sequent_inner strict stack bconts bvars i len hyps concl =
      if i = len then
         let concl =
            compile_so_contractum_term strict stack bconts bvars concl
         in
            [], concl
      else
         match SeqHyp.get hyps i with
            Context (v, conts, subterms) ->
               (* This is a second order context *)
               if array_rstack_c_mem v stack then begin
                  (*
                   * Second order context and the v is bound.
                   * We generate a substitution instance.
                   *)
                  let index = array_rstack_c_index v stack in
                  check_arity v conts (List.length subterms) stack.(index);
                  if strict = Strict then List.iter (check_cont v bconts) conts;
                  let subterms = compile_so_contractum_terms strict stack bconts bvars subterms in
                  let term = RWSeqContextInstance (index, subterms) in
                  let hyps, concl =
                     compile_so_contractum_sequent_inner strict stack (v::bconts) bvars (i + 1) len hyps concl
                  in
                     term :: hyps, concl
               end else
                  (* Free second order context *)
                  REF_RAISE(RefineError ("Rewrite_compile_contractum.compile_so_contractum_hyp", RewriteFreeSOVar v))

          | Hypothesis (v, term) ->
               let term = compile_so_contractum_term strict stack bconts bvars term in
               let v' = compile_bname strict stack v in
               let hyps, concl =
                  compile_so_contractum_sequent_inner strict stack bconts (bvars @ [v]) (i + 1) len hyps concl
               in
               let hyp = RWSeqHyp (v', term) in
                  hyp :: hyps, concl

   (*
    * Toplevel compilation.
    *)
   let compile_so_contractum strict stack term =
      compile_so_contractum_term strict stack [] [] term

   let compile_so_contracta strict stack terms =
      List.map (compile_so_contractum_term strict stack [] []) terms
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)

