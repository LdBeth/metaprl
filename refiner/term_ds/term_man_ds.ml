(*
 * Manifest terms.
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Authors: Alexey Nogin
 *)
INCLUDE "refine_error.mlh"

open Lm_debug
open Lm_symbol

open Opname
open Refine_error_sig
open Term_sig
open Term_ds_sig
open Term_ds
open Term_op_sig
open Term_subst_sig

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Term_man_ds%t"

let debug_address =
   create_debug (**)
      { debug_name = "address";
        debug_description = "show term addressing operations";
        debug_value = false
      }

(*
 * Module builds on term implementation.
 *)
module TermMan (**)
   (Term : TermDsSig with module TermTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (RefineError : RefineErrorSig
    with type Types.term = TermType.term
    with type Types.level_exp = TermType.level_exp) =
struct
   open Term
   open TermType
   open TermOp
   open TermSubst
   open RefineError

   module ManTypes = TermType

   (************************************************************************
    * Level expressions                                                    *
    ************************************************************************)

   (* Simplified level expression constructors *)
   let mk_const_level_exp i =
      { le_const = i; le_vars = [] }

   let mk_var_level_exp v =
      { le_const = 0; le_vars = [{ le_var = v; le_offset = 0 }] }

   (*
    * Increment a level exp
    *)
   let incr_level_exp = function
      ({ le_const = c; le_vars = vars } : level_exp) ->
         let add1 = function
            { le_var = v; le_offset = o } ->
               { le_var = v; le_offset = o + 1 }
         in
            { le_const = c + 1; le_vars = List.map add1 vars }

   (*
    * Build a level expression out of the max of two level
    * expressions.
    *)
   let max_level_exp
      ({ le_const = c1; le_vars = l1 } : level_exp)
      ({ le_const = c2; le_vars = l2 } : level_exp)
      o3 =
         (* Max of two expressions; sort the variables *)
         let rec join = function
            ({ le_var = v1; le_offset = o1 } as h1::t1 as l1),
            ({ le_var = v2; le_offset = o2 } as h2::t2 as l2) ->
               if Lm_symbol.eq v1 v2 then
                  { le_var = v1; le_offset = max o1 (o2 + o3) } :: join (t1, t2)
               else if v1 < v2 then
                  h1 :: join (t1, l2)
               else if o3 = 0 then
                  h2 :: join (l1, t2)
               else
                  { le_var = v2; le_offset = o2 + o3 } :: join (l1, t2)
          | [], l2 ->
               if o3 = 0 then
                  l2
               else
                  let add_off { le_var = v2; le_offset = o2 } =
                     { le_var = v2; le_offset = o2 + o3 }
                  in
                     List.map add_off l2
          | l1, [] ->
               l1
         in
            { le_const = max c1 (c2 + o3); le_vars = join (l1, l2) }

   (*
    * See if the first level is contained in the second.
    *)
   let level_le = fun
      { le_const = const1; le_vars = vars1 }
      { le_const = const2; le_vars = vars2 } ->
         let rec caux = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            { le_var = v2; le_offset = o2 }::t2 ->
               if Lm_symbol.eq v1 v2 then
                  if o1 <= o2 then
                     caux (t1, t2)
                  else
                     false
               else if v2 < v1 then
                  caux (l1, t2)
               else
                  false
          | [], _ -> true
          | _, [] -> false
         in
            if const1 <= const2 then
               caux (vars1, vars2)
            else
               false

   let level_lt = fun
      { le_const = const1; le_vars = vars1 }
      { le_const = const2; le_vars = vars2 } ->
         let rec caux = function
            ({ le_var = v1; le_offset = o1 }::t1 as l1),
            { le_var = v2; le_offset = o2 }::t2 ->
               if Lm_symbol.eq v1 v2 then
                  if o1 < o2 then
                     caux (t1, t2)
                  else
                     false
               else if v2 < v1 then
                  caux (l1, t2)
               else
                  false
          | [], _ ->
               true
          | _, [] ->
               false
         in
            if const1 < const2 then
               caux (vars1, vars2)
            else
               false

   (************************************************************************
    * PRIMITIVE FORMS                                                      *
    ************************************************************************)

   (*
    * Lists.
    *)
   let xnil_term = xnil_term
   let is_xlist_term = is_xlist_term
   let dest_xlist = dest_xlist
   let mk_xlist_term = mk_xlist_term

   let is_xnil_term = is_no_subterms_term xnil_opname
   let is_xcons_term = is_dep0_dep0_term xcons_opname
   let mk_xcons_term = mk_dep0_dep0_term xcons_opname
   let dest_xcons = dest_dep0_dep0_term xcons_opname

   let xconcl_term = xconcl_term

   (*
    * Strings.
    *)
   let string_opname = mk_opname "string" xperv

   let is_xstring_term t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [String _] };
                term_terms = []
         } when Opname.eq opname string_opname ->
            true
       | _ ->
            false

   let dest_xstring t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = []
         } when Opname.eq opname string_opname ->
            s
       | _ ->
            REF_RAISE(RefineError ("dest_xstring", TermMatchError (t, "not a string")))

   let mk_xstring_term s =
      let op = { op_name = string_opname; op_params = [String s] } in
         mk_term op []

   (*
    * String with one subterm.
    *)
   let is_xstring_dep0_term t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [String _] };
                term_terms = [{ bvars = [] }]
         } when Opname.eq opname string_opname ->
            true
       | _ ->
            false

   let dest_xstring_dep0_term t =
      match dest_term t with
         { term_op = { op_name = opname; op_params = [String s] };
           term_terms = [{ bvars = []; bterm = t }]
         } when Opname.eq opname string_opname ->
            s, t
       | _ ->
            REF_RAISE(RefineError ("dest_xstring_dep0_term", TermMatchError (t, "not a string")))

   let mk_xstring_dep0_term s t =
      let op = { op_name = string_opname; op_params = [String s] } in
         mk_term op [mk_simple_bterm t]

   (****************************************
    * Binding                              *
    ****************************************)

   let xbind_opname =
      mk_opname "bind" xperv

   let is_xbind_term =
      is_dep1_term xbind_opname

   let mk_xbind_term =
      mk_dep1_term xbind_opname

   (*************************
    * Sequents              *
    *************************)

   let is_sequent_term t =
      match get_core t with
         Sequent _ -> true
       | _ -> false

   let mk_sequent_term = Term.mk_sequent_term

   let explode_sequent t =
      match get_core t with
         Sequent s ->
            s
       | _ ->
            REF_RAISE(RefineError ("Term_man_ds.explode_sequent", TermMatchError (t, "not a sequent")))

   let rec need_renaming hyps len vars i =
      (i < len) &&
      (match SeqHyp.get hyps i with
         Context _ ->
            need_renaming hyps len vars (succ i)
       | Hypothesis (v, _) ->
            SymbolSet.mem vars v || need_renaming hyps len (SymbolSet.add vars v) (succ i))

   let rec rename_hyps vars sub = function
      [] -> [], sub
    | Context(c, cts, ts) :: hyps ->
         let hyps', sub' = rename_hyps (SymbolSet.add vars c) sub hyps in
            Context(c, cts, List.map (apply_subst sub) ts) :: hyps', sub'
    | Hypothesis (v,t) :: hyps when SymbolSet.mem vars v ->
         let v' = new_name v (SymbolSet.mem vars) in
         let sub' = (v, mk_var_term v') :: sub in
         let hyps', sub' = rename_hyps (SymbolSet.add vars v') sub' hyps in
            Hypothesis(v', apply_subst sub t) :: hyps', sub'
    | Hypothesis (v,t) :: hyps ->
         let hyps', sub' = rename_hyps (SymbolSet.add vars v) sub hyps in
            Hypothesis(v, apply_subst sub t) :: hyps', sub'

   let explode_sequent_and_rename t vars =
      let s = explode_sequent t in
         if need_renaming s.sequent_hyps (SeqHyp.length s.sequent_hyps) vars 0 then
            let hyps, subst = rename_hyps vars [] (SeqHyp.to_list s.sequent_hyps) in
               {
                  sequent_args = s.sequent_args;
                  sequent_hyps = SeqHyp.of_list hyps;
                  sequent_concl = apply_subst subst s.sequent_concl
               }
         else
            s

   (*
    * Get the arguments.
    *)
   let args_name = "Term_man_ds.args"
   let rec args t =
      match t.core with
         Sequent s ->
            s.sequent_args
       | Subst _ ->
            let _ = get_core t in
               args t
       | _ ->
            REF_RAISE(RefineError (args_name, TermMatchError (t, "not a sequent")))

   (*
    * Count the hyps.
    *)
   let num_hyps_name = "Term_man_ds.num_hyps"
   let rec num_hyps t =
      match t.core with
         Sequent s ->
            SeqHyp.length s.sequent_hyps
       | Subst _ ->
            let _ = get_core t in
            num_hyps t
       | _ ->
            REF_RAISE(RefineError (num_hyps_name, TermMatchError (t, "not a sequent")))

   (*
    * Fast access to hyp and concl. Counting starts from 1.
    *)
   let nth_hyp_name = "Term_man_ds.nth_hyp"
   let rec nth_hyp t i =
      match t.core with
         Sequent s ->
            if i <= 0 then
               REF_RAISE(RefineError (nth_hyp_name, StringError "negative address"))
            else
               let i = pred i in
                  if i < SeqHyp.length s.sequent_hyps then
                     match SeqHyp.get s.sequent_hyps i with
                        Hypothesis (_, t) -> t
                      | Context _ ->
                           REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "it's a context")))
                  else
                     REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "not enough hyps")))
       | Subst _ ->
            let _ = get_core t in
            nth_hyp t i
       | _ ->
            REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "not a sequent")))

   let rec nth_binding t i =
      match t.core with
         Sequent s ->
            if i <= 0 then
               REF_RAISE(RefineError (nth_hyp_name, StringError "negative address"))
            else
               let i = pred i in
                  if i < SeqHyp.length s.sequent_hyps then
                     match SeqHyp.get s.sequent_hyps i with
                        Hypothesis (v, _) -> v
                      | Context _ ->
                           REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "it's a context")))
                  else
                     REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "not enough hyps")))
       | Subst _ ->
            let _ = get_core t in
            nth_binding t i
       | _ ->
            REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "not a sequent")))

   (*
    * Get all the hyps.
    * Contexts are not allowed.
    *)
   let hyps_name = "Term_man_ds.hyps"
   let rec hyps_aux hyps i t =
      if i < 0 then
         []
      else
         let rem = hyps_aux hyps (pred i) t in
            match SeqHyp.get hyps i with
               Hypothesis (_, t) -> t :: rem
             | Context _ -> REF_RAISE(RefineError (hyps_name, TermMatchError (t, "illegal context")))

   let hyps t =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
               hyps_aux hyps (SeqHyp.length s.sequent_hyps - 1) t
       | _ ->
            REF_RAISE(RefineError (hyps_name, TermMatchError (t, "not a sequent")))

   (*
    * Get the conclusion.
    *)
   let concl_name = "Term_man_ds.concl"
   let concl t =
      match get_core t with
         Sequent s ->
            s.sequent_concl
       | _ ->
            REF_RAISE(RefineError (concl_name, TermMatchError (t, "not a sequent")))

   (*
    * Collect the vars.
    *)
   let rec declared_vars_aux hyps i =
      if i < 0 then
         []
      else
         let rem = declared_vars_aux hyps (pred i) in
            match SeqHyp.get hyps i with
               Hypothesis (v,_) -> v::rem
             | Context _ -> rem

   let declared_vars_name = "Term_man_ds.declared_vars"
   let declared_vars t =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
               declared_vars_aux hyps (SeqHyp.length s.sequent_hyps - 1)
       | _ ->
            REF_RAISE(RefineError (declared_vars_name, TermMatchError (t, "not a sequent")))

   (*
    * Get the number of the hyp with the given var.
    *)
   let get_decl_number_name = "Term_man_ds.get_decl_number"
   let get_decl_number t v =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
            let rec aux i =
               if i = 0 then
                  REF_RAISE(RefineError (get_decl_number_name, TermMatchError (t, "declaration not found")))
               else
                  match SeqHyp.get hyps (i - 1) with
                     Hypothesis (v',_) when Lm_symbol.eq v' v ->
                        i
                  | _ ->
                        aux (i - 1)
            in
               aux (SeqHyp.length hyps)
       | _ ->
            REF_RAISE(RefineError (get_decl_number_name, TermMatchError (t, "not a sequent")))

   let get_hyp_number_name = "Term_man_ds.get_hyp_number"
   let get_hyp_number t hyp =
      match get_core t with
         Sequent s ->
            let hyps = s.sequent_hyps in
            let rec aux i =
               if i = 0 then
                  REF_RAISE(RefineError (get_hyp_number_name, TermMatchError (t, "declaration not found")))
               else
                  match SeqHyp.get hyps (i - 1) with
                     Hypothesis (_, hyp') when alpha_equal hyp hyp' ->
                        i
                  | _ ->
                        aux (i - 1)
            in
               aux (SeqHyp.length hyps)
       | _ ->
            REF_RAISE(RefineError (get_hyp_number_name, TermMatchError (t, "not a sequent")))

   let replace_concl_name = "Term_man_ds.replace_concl"
   let replace_concl t concl =
      match get_core t with
         Sequent s ->
            mk_sequent_term {sequent_args = s.sequent_args; sequent_hyps = s.sequent_hyps; sequent_concl = concl}
       | _ ->
            REF_RAISE(RefineError (replace_concl_name, TermMatchError (t, "not a sequent")))

   (*
    * Rewrite
    *)
   let xrewrite_op = mk_opname "rewrite" xperv

   let is_xrewrite_term = is_dep0_dep0_term xrewrite_op
   let mk_xrewrite_term = mk_dep0_dep0_term xrewrite_op
   let dest_xrewrite = dest_dep0_dep0_term xrewrite_op

   (************************************************
    * General term destruction.
    *)
   let dest_match_param param =
      match param with
         Number n ->
            if Lm_num.is_integer_num n then
               MatchNumber (n, Some (Lm_num.int_of_num n))
            else
               MatchNumber (n, None)
       | String s ->
            MatchString s
       | Token opname ->
            MatchToken (opname, Opname.dest_opname opname)
       | Shape s ->
            MatchShape s
       | Operator s ->
            MatchOperator s
       | Var v ->
            MatchVar v
       | MLevel l ->
            MatchLevel l
       | Quote
       | MNumber _
       | MString _
       | MToken _
       | MShape _
       | MOperator _
       | ObId _
       | ParamList _ ->
            MatchUnsupported

   let rec explode_term t =
      match get_core t with
         Term t ->
            let op = dest_opname t.term_op.op_name in
            let params = List.map dest_match_param t.term_op.op_params in
               MatchTerm (op, params, t.term_terms)
       | FOVar v ->
            MatchTerm (["var"], [MatchVar v], [])
       | SOVar(v,conts,terms) ->
            MatchTerm (["var"], [MatchVar v], List.map mk_simple_bterm terms)
       | SOContext _ ->
            explode_term (core_term (Term (dest_term t))) (* XXX HACK! *)
       | Sequent { sequent_args = args;
                   sequent_hyps = hyps;
                   sequent_concl = concl
         } ->
            let op = dest_opname (opname_of_term args) in
            let args = explode_term args in
               MatchSequent (op, args, SeqHyp.to_list hyps, concl)
       | Subst _
       | Hashed _ ->
            fail_core "explode_term"

   (*****************************************
    * SO variables and contexts             *
    *****************************************)

   let is_so_var_term t = match get_core t with
      SOVar _ -> true
    | _ -> false

   let dest_so_var t = match get_core t with
      SOVar(v, cs, ts) -> v, cs, ts
    | _ -> REF_RAISE(RefineError ("Term_man_ds.dest_so_var", TermMatchError (t, "not a so_var")))

   let mk_so_var_term v cs ts = core_term (SOVar (v,cs,ts))

   let is_fso_var_term t = match get_core t with
      FOVar _
    | SOVar (_, _, []) -> true
    | _ -> false

   let dest_fso_var t = match get_core t with
      FOVar v
    | SOVar(v, _, []) -> v
    | _ -> REF_RAISE(RefineError ("Term_man_ds.dest_fso_var", TermMatchError (t, "not a FO or 0-ary SO variable")))

   (*
    * A first-order term has no bvars.
    *)
   let is_fo_term t =
      match get_core t with
         Term t ->
            List.for_all (fun bterm -> bterm.bvars = []) t.term_terms
       | FOVar _ ->
            true
       | SOVar _
       | SOContext _
       | Sequent _ ->
            false
       | Subst _
       | Hashed _ ->
            fail_core "is_fo_term"

   (*
    * Second order context, contains a context term, plus
    * binding variables like so vars.
    *)
   let is_context_term t =
      match get_core t with
         SOContext _ -> true
       | _ -> false

   let dest_context term =
      match get_core term with
         SOContext (v, t, conts, ts) -> v, t, conts, ts
       | _ -> REF_RAISE(RefineError ("dest_context", TermMatchError (term, "not a context")))

   let mk_context_term v t conts terms =
      core_term (SOContext(v, t, conts, terms))

   let rec context_vars vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec hyp_context_vars vars i =
               if i = len then
                   context_vars (context_vars vars seq.sequent_args) seq.sequent_concl
               else
                  match SeqHyp.get hyps i with
                     Hypothesis (_, h) ->
                        context_vars (hyp_context_vars vars (succ i)) h
                   | Context (v,_,ts) ->
                        let ints, addrs = vars in
                        let vars = SymbolSet.add ints v, addrs in
                           List.fold_left context_vars (hyp_context_vars vars (succ i)) ts
            in
               hyp_context_vars vars 0
       | SOContext(v, t, _, ts) ->
            let ints, addrs = vars in
               List.fold_left context_vars (ints, SymbolSet.add addrs v) (t::ts)
       | Term { term_terms = bts } ->
            List.fold_left bterm_context_vars vars bts
       | SOVar(_, _, ts) -> List.fold_left context_vars vars ts
       | FOVar _ -> vars
       | Hashed _| Subst _ -> fail_core "context_vars"

   and bterm_context_vars vars bt =
      context_vars vars bt.bterm

   let context_vars = context_vars (SymbolSet.empty, SymbolSet.empty)

   (*
    * This is a variant of the above,
    * but instead of just collecting the vars,
    * we collect a table of their arities.
    *
    * The table is:
    *     var -> (sequent_flag, context_arities, term_arities)
    *        sequent_flag : true iff this is a sequent context
    *        context_arity : number of context arguments
    *        term_arity : number of subterms
    *)
   let rec context_vars_term vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec hyp_context_vars vars i =
               if i = len then
                  vars
               else
                  match SeqHyp.get hyps i with
                     Hypothesis (_, h) ->
                        hyp_context_vars (context_vars_term vars h) (succ i)
                   | Context (v, cs, ts) ->
                        let vars = SymbolTable.add vars v (true, List.length cs, List.length ts) in
                           hyp_context_vars (context_vars_term_list vars ts) (succ i)
            in
               context_vars_term (hyp_context_vars (context_vars_term vars seq.sequent_args) 0) seq.sequent_concl
       | SOContext(v, t, conts, ts) ->
            let vars = SymbolTable.add vars v (false, List.length conts, List.length ts) in
               context_vars_term (context_vars_term_list vars ts) t
       | Term { term_terms = bts } ->
            context_vars_bterm_list vars bts
       | FOVar _ ->
            vars
       | SOVar(v, _, ts) ->
            context_vars_term_list vars ts
       | Hashed _
       | Subst _ ->
            fail_core "context_vars"

   and context_vars_term_list vars tl =
      List.fold_left context_vars_term vars tl

   and context_vars_bterm vars bt =
      context_vars_term vars bt.bterm

   and context_vars_bterm_list vars l =
      List.fold_left context_vars_bterm vars l

   let context_vars_info = context_vars_term
   let context_vars_info_list = context_vars_term_list

   (*
    * We do similarly for so_vars.
    *)
   let rec so_vars_term vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec hyp_so_vars vars i =
               if i = len then
                  vars
               else
                  match SeqHyp.get hyps i with
                     Hypothesis (v, h) ->
                        let vars = SymbolTable.remove (so_vars_term vars h) v in
                           hyp_so_vars vars (succ i)
                   | Context (_, _, ts) ->
                        hyp_so_vars (so_vars_term_list vars ts) (succ i)
            in
               so_vars_term (hyp_so_vars (so_vars_term vars seq.sequent_args) 0) seq.sequent_concl
       | Term { term_terms = bts } ->
            so_vars_bterm_list vars bts
       | SOContext (_, t, _, ts) ->
            so_vars_term_list vars (t :: ts)
       | FOVar _ ->
            vars
       | SOVar (v, cs, ts) ->
            let vars = SymbolTable.add vars v (List.length cs, List.length ts) in
               so_vars_term_list vars ts
       | Hashed _
       | Subst _ ->
            fail_core "context_vars"

   and so_vars_term_list vars tl =
      List.fold_left so_vars_term vars tl

   and so_vars_bterm vars bt =
      so_vars_term vars bt.bterm

   and so_vars_bterm_list vars l =
      List.fold_left so_vars_bterm vars l

   let so_vars_info = so_vars_term
   let so_vars_info_list = so_vars_term_list

   (*
    * Collect all the parameter vars.
    *)
   let rec param_vars_term vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let vars =
               SeqHyp.fold (fun vars _ hyp ->
                     match hyp with
                        Hypothesis (_, h) ->
                           param_vars_term vars h
                      | Context (_, _, ts) ->
                           param_vars_term_list vars ts) vars seq.sequent_hyps
            in
               param_vars_term (param_vars_term vars seq.sequent_args) seq.sequent_concl
       | Term { term_op = { op_params = params }; term_terms = bts } ->
            param_vars_bterm_list (param_vars_param_list vars params) bts
       | FOVar _ ->
            vars
       | SOVar(_, _, ts) ->
            param_vars_term_list vars ts
       | SOContext (_, t, _, ts) ->
            param_vars_term_list vars (t :: ts)
       | Hashed _
       | Subst _ ->
            fail_core "param_vars"

   and param_vars_term_list vars tl =
      List.fold_left param_vars_term vars tl

   and param_vars_param vars param =
      match param with
         Number _
       | String _
       | Token _
       | MLevel _
       | Shape _
       | Operator _
       | Quote ->
            vars
       | Var v
       | MNumber v
       | MString v
       | MShape v
       | MOperator v
       | MToken v ->
            SymbolSet.add vars v
       | ObId params
       | ParamList params ->
            param_vars_param_list vars params

   and param_vars_param_list vars params =
      List.fold_left param_vars_param vars params

   and param_vars_bterm vars bt =
      param_vars_term vars bt.bterm

   and param_vars_bterm_list vars l =
      List.fold_left param_vars_bterm vars l

   let param_vars_info = param_vars_term
   let param_vars_info_list = param_vars_term_list

   (*
    * Meta term.
    *)
   DEFINE FREE_VARS(sovar_case) =
      let rec aux vars t = match get_core t with
         FOVar _ -> vars
       | SOVar(v, conts, ts) -> sovar_case
       | Term{term_terms = bts} -> List.fold_left aux_bterm vars bts
       | SOContext(v, t, conts, ts) ->
            List.fold_left aux (SymbolSet.add_list (SymbolSet.add vars v) conts) (t::ts)
       | Sequent eseq ->
            aux (
               aux_hyps (
                  aux vars eseq.sequent_concl
               ) eseq.sequent_hyps (SeqHyp.length eseq.sequent_hyps)
            ) eseq.sequent_args
       | Hashed _ | Subst _ -> fail_core "free_meta_variables"
      and aux_bterm vars bt = aux vars bt.bterm
      and aux_hyps vars hyps i =
         if i = 0 then
            vars
         else
            let i = pred i in
            let vars =
               match SeqHyp.get hyps i with
                  Hypothesis(_, t) ->
                     aux vars t
                | Context(v, conts, ts) ->
                     List.fold_left aux (SymbolSet.add_list (SymbolSet.add vars v) conts) ts
            in
               aux_hyps vars hyps i
      in
         aux SymbolSet.empty

   let all_meta_variables = FREE_VARS(SymbolSet.add (List.fold_left aux (SymbolSet.add_list vars conts) ts) v)
   let all_contexts = FREE_VARS(List.fold_left aux (SymbolSet.add_list vars conts) ts)

   (************************************************************************
    * Rewrite rules                                                        *
    ************************************************************************)

   (*
    * Build a redex.
    *)
   let construct_redex vars params terms =
      let t = mk_xlist_term (params @ terms) in
      let l = Array.length vars in
      let rec aux i =
         if i < l then
            mk_xbind_term vars.(i) (aux (i + 1))
         else
            t
      in
         aux 0
end
