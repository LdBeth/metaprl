(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998-2003 Aleksey Nogin, Cornell University
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
 * Authors: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

INCLUDE "refine_error.mlh"

open Lm_symbol

open Printf
open Lm_debug

open Refine_error_sig
open Term_ds_sig
open Term_ds
open Term_subst_sig
open Term_op_sig
open Term_man_sig

(*
 * Address of a subterm.
 *)
type addr =
   Path of int list
 | ArgAddr
 | HypAddr of int
 | GoalAddr of int
 | Compose of addr * addr

module TermAddr (**)
   (Term : TermDsSig
    with type level_exp_var = TermType.level_exp_var
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type operator = TermType.operator
    with type term = TermType.term
    with type term_core = TermType.term_core
    with type bound_term = TermType.bound_term
    with type esequent = TermType.esequent
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals

    with type hypothesis = TermType.hypothesis
    with type level_exp_var' = TermType.level_exp_var'
    with type level_exp' = TermType.level_exp'
    with type object_id = TermType.object_id
    with type param' = TermType.param'
    with type operator' = TermType.operator'
    with type term' = TermType.term'
    with type bound_term' = TermType.bound_term')
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (TermOp : TermOpSig
    with type term = TermType.term)
   (TermMan : TermManSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr) =
struct
   open TermMan
   open TermType
   open Term
   open TermSubst
   open TermOp
   open RefineError

   type term = TermType.term
   type address = addr

   (*
    * Constructor.
    *)
   let make_address l =
      Path l

   let is_null_address = function
      Path [] ->
         true
    | _ ->
         false

   let compose_address path1 path2 =
      match path1 with
         Path [] ->
            path2
       | _ ->
            Compose (path1, path2)

   let rec clause_address_of_address = function
      (HypAddr _ | GoalAddr _) as addr ->
         addr
    | Compose (addr, _) ->
         clause_address_of_address addr
    | _ ->
         REF_RAISE (RefineError ("Term_addr_ds.clause_address_of_address", StringError "address is not a sequent address"))

   IFDEF VERBOSE_EXN THEN
      DEFMACRO ATERM = (a, term)
   ELSE
      DEFMACRO ATERM = NOTHING
   ENDIF

   let rec find_subterm_term addr arg t =
      if alpha_equal t arg then
         Some (Path (List.rev addr))
      else match get_core t with
         FOVar _ -> None
       | SOVar(_,_,terms) -> find_subterm_terms addr arg 0 terms
       | Term t -> find_subterm_bterms addr arg 0 t.term_terms
       | Sequent s ->
            begin match
               (if alpha_equal s.sequent_args arg then Some ArgAddr
                else find_subterm_hyps arg s 0 (SeqHyp.length s.sequent_hyps)),
               addr
            with
               None, _ -> None
             | res, [] -> res
             | Some res, _ -> Some(Compose(Path (List.rev addr), res))
            end
       | Hashed _ | Subst _ -> fail_core "find_subterm_term"

   and find_subterm_bterms addr arg index = function
      [] -> None
    | bterm :: bterms ->
         begin match find_subterm_term (index :: addr) arg bterm.bterm with
            Some _ as result -> result
          | None -> find_subterm_bterms addr arg (succ index) bterms
         end

   and find_subterm_terms addr arg index = function
      [] -> None
    | t :: ts ->
         begin match find_subterm_term (index :: addr) arg t with
            Some _ as result -> result
          | None -> find_subterm_terms addr arg (succ index) ts
         end

   and find_subterm_hyps arg s i len =
      if i = len then find_subterm_goals arg s 0 (SeqGoal.length s.sequent_goals)
      else match
         match SeqHyp.get s.sequent_hyps i with
            Hypothesis t | HypBinding (_, t) -> find_subterm_term [] arg t
          | Context(_, _, ts) -> find_subterm_terms [] arg 0 ts
      with
         Some(Path[]) -> Some(HypAddr i)
       | Some addr -> Some(Compose(HypAddr i, addr))
       | None -> find_subterm_hyps arg s (succ i) len

   and find_subterm_goals arg s i len =
      if i = len then None else
      match find_subterm_term [] arg (SeqGoal.get s.sequent_goals i) with
         Some(Path[]) -> Some(GoalAddr i)
       | Some addr -> Some(Compose(GoalAddr i, addr))
       | None -> find_subterm_goals arg s (succ i) len

   let find_subterm t arg =
      match find_subterm_term [] arg t with
         Some addr -> addr
       | None ->
            REF_RAISE(RefineError ("Term_addr_gen.find_subterm", StringTermError ("subterm can't be found",arg)))

   (*
    * Get a subterm.
    *)
   let rec getnth ATERM terms i =
      match (terms, i) with
         (hd::_, 0) ->
            hd
       | (hd::tl, _) ->
            getnth ATERM tl (pred i)
       | ([], _) ->
            REF_RAISE(RefineError ("getnth", AddressError (a, term)))

   (*
    * Follow an explicit path.
    *)
   let term_subterm_name = "Term_addr_ds.term_subterm"
   let rec term_subterm_path ATERM t = function
      [] ->
         t
    | i::tl ->
         begin match get_core t with
            Term t ->
               term_subterm_path ATERM (getnth ATERM t.term_terms i).bterm tl
          | SOVar(_, _, ts) ->
               term_subterm_path ATERM (getnth ATERM ts i) tl
          | FOVar _ | Sequent _ ->
               REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
          | Subst _ | Hashed _ ->
               fail_core term_subterm_name
         end

   (*
    * Get the subterm for any type of path.
    *)
   let rec term_subterm term a =
      match (get_core term), a with
         _, Path path ->
            term_subterm_path ATERM term path
       | Sequent s, ArgAddr ->
            s.sequent_args
       | Sequent s, HypAddr i ->
            if i >= 0 && i < SeqHyp.length s.sequent_hyps then
               match SeqHyp.get s.sequent_hyps i with
                  HypBinding (_, t) | Hypothesis t -> t
                | Context _ -> REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
            else REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
       | Sequent s, GoalAddr i ->
            if i >= 0 && i < SeqGoal.length s.sequent_goals then SeqGoal.get s.sequent_goals i
            else REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
       | (Sequent s, Compose (HypAddr i, ((Path (j :: path)) as addr2))) ->
            (*
             * Special case to address through contexts.
             *)
            if i >= 0 && i < SeqHyp.length s.sequent_hyps then
               match SeqHyp.get s.sequent_hyps i with
                  HypBinding (_, t) | Hypothesis t ->
                     term_subterm t addr2
                | Context (_, _, subterms) ->
                     term_subterm_path ATERM (getnth ATERM subterms j) path
            else
               REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
       | _, Compose (addr1, addr2) ->
            term_subterm (term_subterm term addr1) addr2
       | _ -> REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))

   (*
    * Just get the subterm addresses.
    *)
   let subterm_addresses =
      let rec make_path_list i =
         if i = 0 then [] else let i = pred i in (Path [i]) :: (make_path_list i)
      in let rec make_goal_list i goals addrs =
         if i = 0 then addrs else let i = pred i in make_goal_list i goals (GoalAddr i :: addrs)
      in let rec make_hyppath_list i addr addrs =
         if i = 0 then addrs else let i = pred i in make_hyppath_list i addr (Compose(addr, Path [i]) :: addrs)
      in let rec make_hyp_list i hyps addrs =
         if i = 0 then addrs else let i = pred i in
            make_hyp_list i hyps (
               match SeqHyp.get hyps i with 
                  Hypothesis _ | HypBinding _ -> HypAddr i :: addrs
                | Context(_,_,ts) -> make_hyppath_list (List.length ts) (HypAddr i) addrs
            )
      in fun t -> match get_core t with
         Term t ->
            make_path_list (List.length t.term_terms)
       | FOVar _ -> []
       | SOVar (_, _, ts) -> make_path_list (List.length ts)
       | Sequent s ->
            let goal_addrs = make_goal_list (SeqGoal.length s.sequent_goals) s.sequent_goals [ArgAddr] in
               make_hyp_list (SeqHyp.length s.sequent_hyps) s.sequent_hyps goal_addrs
       | Hashed _ | Subst _ -> fail_core "subterm_addresses"

   (*
    * Replace a subterm at the specified address.
    * Capture is not taken into account.  This function
    * allows the replacement to compute an extra value.
    *)

   IFDEF VERBOSE_EXN THEN
      DEFMACRO FAIL = fail
      DEFMACRO DO_FAIL = fail_addr fail
   ELSE
      DEFMACRO FAIL = NOTHING
      DEFMACRO DO_FAIL = RAISE_GENERIC_EXN
   ENDIF

   let rec collect_hyp_bvars i hyps bvars =
      if i < 0 then bvars
      else match SeqHyp.get hyps i with
         HypBinding (v,_) -> collect_hyp_bvars (pred i) hyps (SymbolSet.add bvars v)
       | Hypothesis _ | Context _ -> collect_hyp_bvars (pred i) hyps bvars

   let collect_goal_bvars hyps = collect_hyp_bvars (SeqHyp.length hyps - 1) hyps

   let fail_addr (addr, term) =
      REF_RAISE(RefineError ("Term_addr_ds.apply_*_fun_*", AddressError (addr, term)))

   DEFMACRO MAKE_PATH_REPLACE_TERM =
      fun FAIL f BVARS t -> function
         i::tl -> begin
            match get_core t with
               Term t ->
                  let bterms, arg = PATH_REPLACE_BTERM FAIL f tl i BVARS t.term_terms in
                     mk_term t.term_op bterms, arg
             | SOVar(v, conts, ts) ->
                  let ts, arg = PATH_REPLACE_TERMS FAIL f tl i BVARS ts in
                     core_term (SOVar(v, conts, ts)), arg
             | Sequent _ | FOVar _ -> DO_FAIL
             | Hashed _ | Subst _ -> fail_core "path_replace_term"
            end
       | [] ->
            f BVARS t

   DEFMACRO MAKE_PATH_REPLACE_TERMS =
      fun FAIL f tl i BVARS ts ->
         match i, ts with
            0, (t::ts) ->
               let t, arg = PATH_REPLACE_TERM FAIL f BVARS t tl
                  in (t::ts), arg
          | _, (t::ts) ->
               let ts, arg = PATH_REPLACE_TERMS FAIL f tl (pred i) BVARS ts in
                  (t::ts), arg
          | _, [] ->
               DO_FAIL

   DEFMACRO MAKE_PATH_REPLACE_BTERM =
      fun FAIL f tl i BVARS bterms ->
         match i, bterms with
            (0, { bvars = vars; bterm = term } :: bterms) ->
               let term, arg = PATH_REPLACE_TERM FAIL f VARS_BVARS term tl in
                  mk_bterm vars term :: bterms, arg
          | (_, bterm :: bterms) ->
               let bterms, arg = PATH_REPLACE_BTERM FAIL f tl (pred i) BVARS bterms in
                  bterm :: bterms, arg
          | _, [] ->
               DO_FAIL

   DEFMACRO APPLY_FUN_AUX MY_NAME =
      fun FAIL f addr BVARS term ->
         match (get_core term, addr) with
            (Sequent s, ArgAddr) ->
               let term, arg = f BVARS s.sequent_args in
                  mk_sequent_term (**)
                     { sequent_args = term;
                       sequent_hyps = s.sequent_hyps;
                       sequent_goals = s.sequent_goals
                     }, arg
          | (Sequent s, HypAddr i) ->
               if i>=0 && i < SeqHyp.length s.sequent_hyps then
                  let hyp, arg = match SeqHyp.get s.sequent_hyps i with
                     HypBinding (v,t) as hyp ->
                        let term, arg = f HYP_BVARS t in
                           HypBinding (v,term), arg
                   | Hypothesis t as hyp ->
                        let term, arg = f HYP_BVARS t in
                           Hypothesis term, arg
                   | Context (v, conts, subterms) ->
                        let slot = mk_var_term v in
                        let t = mk_context_term v slot conts subterms in
                        let t, arg = f BVARS t in
                        let v1, term1, conts, subterms = dest_context t in
                           if v1 = v && is_var_term term1 && dest_var term1 = v then
                              Context (v, conts, subterms), arg
                           else DO_FAIL
                  in
                  let aux i' hyp' =
                     if i' = i then hyp else hyp'
                  in
                     mk_sequent_term (**)
                        { sequent_args = s.sequent_args;
                          sequent_hyps = SeqHyp.mapi aux s.sequent_hyps;
                          sequent_goals = s.sequent_goals
                        }, arg
               else DO_FAIL
          | (Sequent s, GoalAddr i) ->
               if i>=0 && i < SeqGoal.length s.sequent_goals then
                  let term, arg = f GOAL_BVARS (SeqGoal.get s.sequent_goals i) in
                  let aux i' t' =
                    if i' = i then term else t'
                  in mk_sequent_term (**)
                        { sequent_args = s.sequent_args;
                          sequent_hyps = s.sequent_hyps;
                          sequent_goals = SeqGoal.mapi aux s.sequent_goals
                        }, arg
               else DO_FAIL
          | (_, Path addr) ->
               PATH_REPLACE_TERM FAIL f BVARS term addr
          | (_, Compose (addr1, addr2)) ->
               MY_NAME FAIL (MY_NAME FAIL f addr2) addr1 BVARS term
          | _ -> DO_FAIL


   DEFMACRO BVARS = NOTHING
   DEFMACRO VARS_BVARS = NOTHING
   DEFMACRO HYP_BVARS = NOTHING
   DEFMACRO GOAL_BVARS = NOTHING
   DEFMACRO PATH_REPLACE_TERM = path_replace_term
   DEFMACRO PATH_REPLACE_BTERM = path_replace_bterm
   DEFMACRO PATH_REPLACE_TERMS = path_replace_terms

   let rec path_replace_term = MAKE_PATH_REPLACE_TERM
   and path_replace_terms = MAKE_PATH_REPLACE_TERMS
   and path_replace_bterm = MAKE_PATH_REPLACE_BTERM

   IFDEF VERBOSE_EXN THEN
      let rec apply_fun_arg_at_addr_aux =
         APPLY_FUN_AUX apply_fun_arg_at_addr_aux

      let apply_fun_arg_at_addr =
         fun f addr BVARS term ->
            apply_fun_arg_at_addr_aux (addr, term) f addr BVARS term
   ELSE
      let rec apply_fun_arg_at_addr =
         APPLY_FUN_AUX apply_fun_arg_at_addr
   ENDIF

   let add_unit_arg f BVARS t =
      f BVARS t, ()

   let apply_fun_at_addr f addr BVARS term =
      fst (apply_fun_arg_at_addr (add_unit_arg f) addr BVARS term)

   UNDEF BVARS
   UNDEF VARS_BVARS

   DEFMACRO BVARS = bvars
   DEFMACRO VARS_BVARS = SymbolSet.add_list bvars vars
   DEFMACRO HYP_BVARS = (collect_hyp_bvars (pred i) s.sequent_hyps BVARS)
   DEFMACRO GOAL_BVARS = (collect_goal_bvars s.sequent_hyps BVARS)
   DEFMACRO PATH_REPLACE_TERM = path_var_replace_term
   DEFMACRO PATH_REPLACE_BTERM = path_var_replace_bterm
   DEFMACRO PATH_REPLACE_TERMS = path_var_replace_terms

   let rec path_var_replace_term = MAKE_PATH_REPLACE_TERM
   and path_var_replace_terms = MAKE_PATH_REPLACE_TERMS
   and path_var_replace_bterm = MAKE_PATH_REPLACE_BTERM

   IFDEF VERBOSE_EXN THEN
      let rec apply_var_fun_at_addr_aux =
         APPLY_FUN_AUX apply_var_fun_at_addr_aux

      let apply_var_fun_arg_at_addr =
         fun f addr BVARS term ->
            apply_var_fun_at_addr_aux (addr, term) f addr BVARS term
   ELSE
      let rec apply_var_fun_arg_at_addr =
         APPLY_FUN_AUX apply_var_fun_arg_at_addr
   ENDIF

   let add_var_unit_arg f BVARS t =
      f BVARS t, ()

   let apply_var_fun_at_addr f addr BVARS term =
      fst (apply_var_fun_arg_at_addr (add_var_unit_arg f) addr BVARS term)

   let replace_subterm_aux subterm term =
      subterm

   let replace_subterm term addr subterm =
      apply_fun_at_addr (replace_subterm_aux subterm) addr term

   let replace_bound_subterm_aux f bvars term =
      f bvars

   let replace_bound_subterm term addr bvars f =
      apply_var_fun_at_addr (replace_bound_subterm_aux f) addr bvars term

   (*
    * Print address as a string.
    *)
   let rec collect_string_of_path_address = function
      [] ->
         ""
    | [h] ->
         string_of_int h
    | h::t ->
         (string_of_int h) ^ "; " ^ (collect_string_of_path_address t)

   let rec collect_string_of_address = function
      Path addr ->
         collect_string_of_path_address addr
    | ArgAddr -> "Arg"
    | HypAddr i -> "Hyp(" ^ string_of_int (i+1) ^ ")"
    | GoalAddr i -> "Goal(" ^ string_of_int (i+1) ^ ")"
    | Compose (addr1, addr2) ->
         let addr1 = collect_string_of_address addr1 in
         let addr2 = collect_string_of_address addr2 in
            if addr1 = "" then
               addr2
            else if addr2 = "" then
               addr1
            else
               addr1 ^ "; " ^ addr2

   let string_of_address addr =
      "[" ^ collect_string_of_address addr ^ "]"

   (*
    * Apply the function to the outermost terms where it does not fail.
    *)
   let rec apply_fun_higher_term f coll term =
      try let t, arg = f term in
             t, (arg::coll)
      with RefineError _ -> begin
         match get_core term with
            FOVar _ | Term { term_terms = [] } -> (term,coll)
          | Term bt ->
               let btrms, args = apply_fun_higher_bterms f coll bt.term_terms in
                  if args == coll then (term,coll) else (mk_term bt.term_op btrms, args)
          | SOVar(v, conts, ts) ->
               let ts, args = apply_fun_higher_terms f coll ts in
                  if args == coll then (term,coll) else core_term (SOVar(v, conts, ts)), args
          | Sequent _ -> raise(Invalid_argument "Term_addr_ds.apply_fun_higher called on a sequent")
          | Hashed _ | Subst _ -> fail_core "apply_fun_higher_term"
         end

   and apply_fun_higher_terms f coll = function
      [] -> [], coll
    | (t :: ts) as all_ts ->
         let ts_new, args = apply_fun_higher_terms f coll ts in
         let t_new, args2 = apply_fun_higher_term f args t in
            if args2 == coll then (all_ts, coll) else
            let t_new = if args2 == args then t else t_new in
               (t_new::ts_new, args2)

   and apply_fun_higher_bterms f coll = function
      [] ->
         ([],coll)
    | [bt] as bterms ->
         let (bt_new, args) = apply_fun_higher_term f coll bt.bterm in
            if args == coll then (bterms, coll)
            else
               [mk_bterm bt.bvars bt_new],args
    | (bt::btrms) as bterms ->
         let btrms_new, args = apply_fun_higher_bterms f coll btrms in
         let bt_new, args2 = apply_fun_higher_term f args bt.bterm in
            if args2 == coll then (bterms, coll)
            else
               let bt_new = if args2 == args then bt else mk_bterm bt.bvars bt_new in
                  (bt_new::btrms_new, args2)

   let apply_fun_higher f term = apply_fun_higher_term f [] term

   (*
    * Apply the function at the outermost terms where it does not fail,
    * and also pass in binding variables.
    *)
   let rec apply_var_fun_higher_term f bvars coll term =
      try
         let t, arg = f bvars term in
            t, arg :: coll
      with RefineError _ -> begin
         match get_core term with
            FOVar _ | Term { term_terms = [] } -> (term,coll)
          | Term bt ->
               let btrms, args = apply_var_fun_higher_bterms f bvars coll bt.term_terms in
                  if args == coll then (term,coll) else (mk_term bt.term_op btrms, args)
          | SOVar(v, conts, ts) ->
               let ts, args = apply_var_fun_higher_terms f bvars coll ts in
                  if args == coll then (term,coll) else core_term(SOVar(v, conts, ts)), args
          | Sequent _ -> raise(Invalid_argument "Term_addr_ds.apply_var_fun_higher called on a sequent")
          | Hashed _ | Subst _ -> fail_core "apply_var_fun_higher_term"
         end

   and apply_var_fun_higher_terms f bvars coll = function
      [] -> [], coll
    | (t :: ts) as all_ts ->
         let ts_new, args = apply_var_fun_higher_terms f bvars coll ts in
         let t_new, args2 = apply_var_fun_higher_term f bvars args t in
            if args2 == coll then (all_ts, coll) else
            let t_new = if args2 == args then t else t_new in
               (t_new::ts_new, args2)

   and apply_var_fun_higher_bterms f bvars coll = function
      [] ->
         [], coll
    | ({ bvars = bvars'; bterm = term } :: bterms) as bterms' ->
         let bterms_new, args = apply_var_fun_higher_bterms f bvars coll bterms in
         let bterm_new, args = apply_var_fun_higher_term f (SymbolSet.add_list bvars bvars') args term in
            if args == coll then
               bterms', coll
            else
               (mk_bterm bvars' bterm_new) :: bterms_new, args

   let apply_var_fun_higher f bvars term =
      apply_var_fun_higher_term f bvars [] term

   (*
    * Find the address of the hyp. Numbers start with 1
    * We just check to make sure the address is valid.
    *)
   let nth_hyp_addr_name = "Term_addr_ds.nth_hyp_addr"
   let nth_hyp_addr t n =
      if n <= 0 then
         REF_RAISE(RefineError (nth_hyp_addr_name, StringError "negative address"))
      else
         match get_core t with
            Sequent s ->
               if n <= SeqHyp.length s.sequent_hyps then
                  HypAddr (pred n)
               else
                  REF_RAISE(RefineError (nth_hyp_addr_name, TermMatchError (t, "not enough hyps")))
          | _ ->
               REF_RAISE(RefineError (nth_hyp_addr_name, TermMatchError (t, "not a sequent")))

   (*
    * Find the address of the conclusion. Numbers start with 1
    *)
   let nth_concl_addr_name = "Term_addr_ds.nth_concl_addr"
   let nth_concl_addr t n =
      if n <= 0 then
         REF_RAISE(RefineError (nth_concl_addr_name, StringError "negative address"))
      else
         match get_core t with
            Sequent s ->
               if n <= SeqGoal.length s.sequent_goals then
                  GoalAddr (pred n)
               else
                  REF_RAISE(RefineError (nth_concl_addr_name, TermMatchError (t, "not enough hyps")))
          | _ ->
               REF_RAISE(RefineError (nth_concl_addr_name, TermMatchError (t, "not a sequent")))

   (*
    * Conclusion is number 0,
    * negative numbers index from last hyp towards first.
    *)
   let nth_clause_addr_name = "Term_man_ds.nth_clause_addr"
   let nth_clause_addr t i =
      match get_core t with
         Sequent s ->
            let hlen = SeqHyp.length s.sequent_hyps in
               if (i = 0) then
                  GoalAddr 0
               else if (i > 0) then
                  if i <= hlen then
                     HypAddr (pred i)
                  else
                     REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "not enough hyps")))
               else if (-i) <= hlen then
                  HypAddr (hlen + i)
               else
                  REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "not enough hyps for a negative addressing")))
       | _ ->
            REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "not a sequent")))
end

