(*
 * Manifest terms.
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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
    with type ErrTypes.Types.term = TermType.term
    with type ErrTypes.Types.level_exp = TermType.level_exp) =
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
               if v1 = v2 then
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
               if v1 = v2 then
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
               if v1 = v2 then
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

   let rec rename_hyps vars sub = function
      [] -> [], sub
    | Context(c, _,_) :: _ when SymbolSet.mem vars c ->
         invalid_arg "Term_man_ds.explode_sequent_and_rename: bound context encountered"
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
         let hyps, subst = rename_hyps vars [] (SeqHyp.to_list s.sequent_hyps) in
            if subst == [] then s else {
               s with
               sequent_hyps = SeqHyp.of_list hyps;
               sequent_concl = apply_subst subst s.sequent_concl
            }

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
      if i < 0 then [] else
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
                     Hypothesis (v',_) when v' = v ->
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
       | Token s ->
            MatchToken s
       | Var v ->
            MatchVar v
       | MLevel l ->
            MatchLevel l
       | Quote
       | MNumber _
       | MString _
       | MToken _
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
       | Sequent { sequent_args = args;
                   sequent_hyps = hyps;
                   sequent_concl = concl
         } ->
            let op = dest_opname (opname_of_term args) in
            let args = List.map explode_term (subterms_of_term args) in
               MatchSequent (op, args, SeqHyp.to_list hyps, concl)
       | Subst _ | Hashed _ ->
            fail_core "explode_term"

   (*****************************************
    * SO variables and contexts             *
    *****************************************)
   let is_so_var_term t = match get_core t with
    | SOVar _ -> true
    | _ -> false

   let dest_so_var t = match get_core t with
      SOVar(v,cs,ts) -> v,cs,ts
    | _ -> REF_RAISE(RefineError ("Term_man_ds.dest_so_var", TermMatchError (t, "not a so_var")))

   let mk_so_var_term v cs ts = core_term (SOVar (v,cs,ts))

   let is_fso_var_term t = match get_core t with
      FOVar _ | SOVar (_, _, []) -> true
    | _ -> false

   let dest_fso_var t = match get_core t with
      FOVar v | SOVar(v,_,[]) -> v
    | _ -> REF_RAISE(RefineError ("Term_man_ds.dest_fso_var", TermMatchError (t, "not a FO or 0-ary SO variable")))

   (*
    * Second order context, contains a context term, plus
    * binding variables like so vars.
    *
    * XXX TODO: Eventually contexts should probably have their own
    * choice in the core type.
    *)
   let is_context_term =
      let rec is_context_bterms = function
         []|[_] -> false
       | [ { bvars = [_] }; { bvars = []; bterm = conts }] -> is_xlist_term conts
       | { bvars = [] } :: tl -> is_context_bterms tl
       | _ -> false
      in fun t -> match get_core t with
         Term { term_op = { op_name = opname; op_params = [Var _] };
                term_terms = bterms
              } -> Opname.eq opname context_opname && is_context_bterms bterms
       | _ ->
            false

   let dest_context =
      let rec collect v term = function
         [ { bvars = [v']; bterm = t }; { bvars = []; bterm = conts }] ->
            [],
            (if v=v' then
               t
            else if is_var_free v t then
               raise (Invalid_argument "Term_man_ds.dest_context: variable clash")
            else
               subst1 t v' (mk_var_term v)),
            (List.map dest_var (dest_xlist conts))
       | { bvars = []; bterm = hd } :: tl ->
            let args, t, conts = collect v term tl in hd :: args, t, conts
       | _ ->
            REF_RAISE(RefineError ("dest_context", TermMatchError (term, "not a context")))
      in fun term -> match dest_term term with
         { term_op = { op_name = opname; op_params = [Var v] };
           term_terms = bterms
         } when Opname.eq opname context_opname ->
            let args, term, conts = collect v term bterms in v, term, conts, args
       | _ ->
            REF_RAISE(RefineError ("dest_context", TermMatchError (term, "not a context")))

   let mk_context_term v term conts terms =
      let rec collect = function
         [] ->
            [mk_bterm [v] term; mk_simple_bterm (mk_xlist_term (List.map mk_var_term conts))]
       | h::t ->
            mk_simple_bterm h :: collect t
      in
         mk_term (mk_op context_opname [Var v]) (collect terms)

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
       | Term { term_op = { op_name = opname; op_params = [Var v] }; term_terms = bts }
            when Opname.eq opname Opname.context_opname ->
            let ints, addrs = vars in
               List.fold_left bterm_context_vars (ints, SymbolSet.add addrs v) bts
       | Term { term_terms = bts } ->
            List.fold_left bterm_context_vars vars bts
       | SOVar(_, _, ts) -> List.fold_left context_vars vars ts
       | FOVar _ -> vars
       | Hashed _| Subst _ -> fail_core "context_vars"

   and bterm_context_vars vars bt =
      context_vars vars bt.bterm

   let context_vars = context_vars (SymbolSet.empty, SymbolSet.empty)

   let rec free_meta_variables vars t = match get_core t with
      FOVar _ -> vars
    | SOVar(v, conts, ts) -> SymbolSet.add (List.fold_left free_meta_variables (SymbolSet.add_list vars conts) ts) v
    | Term{term_terms = bts} -> List.fold_left free_meta_variables_bterm vars bts
    | Sequent eseq ->
         free_meta_variables (
            free_meta_variables_hyps (
               free_meta_variables vars eseq.sequent_concl
            ) eseq.sequent_hyps (SeqHyp.length eseq.sequent_hyps)
         ) eseq.sequent_args
    | Hashed _ | Subst _ -> fail_core "free_meta_variables"

   and free_meta_variables_bterm vars bt = free_meta_variables vars bt.bterm

   and free_meta_variables_hyps vars hyps i =
      if i = 0 then
         vars
      else
         let i = pred i in
         let vars =
            match SeqHyp.get hyps i with
               Hypothesis(_, t) ->
                  free_meta_variables vars t
             | Context(v, conts, ts) ->
                  List.fold_left free_meta_variables (SymbolSet.add_list (SymbolSet.remove vars v) conts) ts
         in
            free_meta_variables_hyps vars hyps i

   let free_meta_variables = free_meta_variables SymbolSet.empty

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
