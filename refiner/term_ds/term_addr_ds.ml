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

open Refine_error_sig
open Term_sig
open Term_ds_sig
open Term_ds
open Term_subst_sig
open Term_op_sig
open Term_man_sig

(*
 * Address of a subterm.
 *)
type addr_item =
   Subterm of int
 | ArgAddr
 | HypAddr of int
 | GoalAddr of int

type addr = addr_item list

module TermAddr (**)
   (Term : TermDsSig with module TermTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (RefineError : RefineErrorSig
    with type ErrTypes.Types.term = TermType.term
    with type ErrTypes.address = addr) =
struct
   open TermMan
   open TermType
   open Term
   open TermSubst
   open RefineError

   module AddrTypes = TermType

   type address = addr

   (*
    * Constructor.
    *)
   let make_address =
      List.map (fun i -> Subterm i)

   let is_null_address = function
      [] ->
         true
    | _ ->
         false

   let compose_address path1 path2 =
      path1 @ path2

   let rec split_clause_address = function
      (HypAddr _ | GoalAddr _) as addr :: rest ->
         [addr], rest
    | _ ->
         REF_RAISE (RefineError ("Term_addr_ds.split_clause_address", StringError "address is not a sequent address"))

   IFDEF VERBOSE_EXN THEN
      DEFINE ATERM = (a, term)
   ELSE
      DEFINE ATERM = NOTHING
   ENDIF

   let rec find_subterm_term addr arg t =
      if alpha_equal t arg then
         Some (List.rev addr)
      else match get_core t with
         FOVar _ -> None
       | SOVar(_,_,terms) -> find_subterm_terms addr arg 0 terms
       | Term t -> find_subterm_bterms addr arg 0 t.term_terms
       | Sequent s ->
            begin match
               find_subterm_term (ArgAddr :: addr) arg s.sequent_args with
                  None ->
                     find_subterm_hyps addr arg s 0 (SeqHyp.length s.sequent_hyps)
                | found -> found
            end
       | Hashed _ | Subst _ -> fail_core "find_subterm_term"

   and find_subterm_bterms addr arg index = function
      [] -> None
    | bterm :: bterms ->
         begin match find_subterm_term (Subterm index :: addr) arg bterm.bterm with
            Some _ as result -> result
          | None -> find_subterm_bterms addr arg (succ index) bterms
         end

   and find_subterm_terms addr arg index = function
      [] -> None
    | t :: ts ->
         begin match find_subterm_term (Subterm index :: addr) arg t with
            Some _ as result -> result
          | None -> find_subterm_terms addr arg (succ index) ts
         end

   and find_subterm_hyps addr arg s i len =
      if i = len then find_subterm_goals addr arg s 0 (SeqGoal.length s.sequent_goals)
      else match
         let addr = HypAddr i :: addr in
         match SeqHyp.get s.sequent_hyps i with
            Hypothesis (_, t) -> find_subterm_term addr arg t
          | Context(_, _, ts) -> find_subterm_terms addr arg 0 ts
      with
         None -> find_subterm_hyps addr arg s (succ i) len
       | found -> found

   and find_subterm_goals addr arg s i len =
      if i = len then None else
      match find_subterm_term (GoalAddr i :: addr) arg (SeqGoal.get s.sequent_goals i) with
         None -> find_subterm_goals addr arg s (succ i) len
       | found -> found

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
   let rec term_subterm term = function
      [] -> term
    | (hd :: rest) as a ->
         let t =
            match (get_core term), hd with
               Term t, Subterm i ->
                  (getnth ATERM t.term_terms i).bterm
             | SOVar(_, _, ts), Subterm i ->
                  getnth ATERM ts i
             | Sequent s, ArgAddr ->
                  s.sequent_args
             | Sequent s, HypAddr i ->
                  if i >= 0 && i < SeqHyp.length s.sequent_hyps then
                     match SeqHyp.get s.sequent_hyps i with
                        Hypothesis (_, t) -> t
                      | Context (v, conts, ts) -> core_term (SOVar(v, conts, ts))
                  else REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
             | Sequent s, GoalAddr i ->
                  if i >= 0 && i < SeqGoal.length s.sequent_goals then SeqGoal.get s.sequent_goals i
                  else REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
             | _ -> REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
         in
            term_subterm t rest

   (*
    * Just get the subterm addresses.
    *)
   let subterm_addresses =
      let rec make_path_list i =
         if i = 0 then [] else let i = pred i in [Subterm i] :: (make_path_list i)
      in let rec make_goal_list i goals addrs =
         if i = 0 then addrs else let i = pred i in make_goal_list i goals ([GoalAddr i] :: addrs)
      in let rec make_hyppath_list i addr addrs =
         if i = 0 then addrs else let i = pred i in make_hyppath_list i addr ([addr; Subterm i] :: addrs)
      in let rec make_hyp_list i hyps addrs =
         if i = 0 then addrs else let i = pred i in
            make_hyp_list i hyps (
               match SeqHyp.get hyps i with
                  Hypothesis _ -> [HypAddr i] :: addrs
                | Context(_,_,ts) -> make_hyppath_list (List.length ts) (HypAddr i) addrs
            )
      in fun t -> match get_core t with
         Term t ->
            make_path_list (List.length t.term_terms)
       | FOVar _ -> []
       | SOVar (_, _, ts) -> make_path_list (List.length ts)
       | Sequent s ->
            let goal_addrs = make_goal_list (SeqGoal.length s.sequent_goals) s.sequent_goals [[ArgAddr]] in
               make_hyp_list (SeqHyp.length s.sequent_hyps) s.sequent_hyps goal_addrs
       | Hashed _ | Subst _ -> fail_core "subterm_addresses"

   (*
    * Replace a subterm at the specified address.
    * Capture is not taken into account.  This function
    * allows the replacement to compute an extra value.
    *)

   IFDEF VERBOSE_EXN THEN
      DEFINE FAIL = fail
      DEFINE DO_FAIL = fail_addr fail
   ELSE
      DEFINE FAIL = NOTHING
      DEFINE DO_FAIL = RAISE_GENERIC_EXN
   ENDIF

   let rec collect_hyp_bvars i hyps bvars =
      if i < 0 then bvars
      else match SeqHyp.get hyps i with
         Hypothesis (v,_) -> collect_hyp_bvars (pred i) hyps (SymbolSet.add bvars v)
       | Context _ -> collect_hyp_bvars (pred i) hyps bvars

   let collect_goal_bvars hyps = collect_hyp_bvars (SeqHyp.length hyps - 1) hyps

   let fail_addr (addr, term) =
      REF_RAISE(RefineError ("Term_addr_ds.apply_*_fun_*", AddressError (addr, term)))

   DEFINE MAKE_PATH_REPLACE_TERMS(bvars, path_replace_terms) =
      fun FAIL f i bvars ts ->
         match i, ts with
            0, (t::ts) ->
               let t, arg = f bvars t
                  in (t::ts), arg
          | _, (t::ts) ->
               let ts, arg = path_replace_terms FAIL f (pred i) bvars ts in
                  (t::ts), arg
          | _, [] ->
               DO_FAIL

   DEFINE MAKE_PATH_REPLACE_BTERM(bvars, vars_bvars, path_replace_bterm) =
      fun FAIL f i bvars bterms ->
         match i, bterms with
            (0, { bvars = vars; bterm = term } :: bterms) ->
               let term, arg = f vars_bvars term in
                  mk_bterm vars term :: bterms, arg
          | (_, bterm :: bterms) ->
               let bterms, arg = path_replace_bterm FAIL f (pred i) bvars bterms in
                  bterm :: bterms, arg
          | _, [] ->
               DO_FAIL

   DEFINE APPLY_FUN_AUX(my_name, path_replace_terms, path_replace_bterm, bvars, hyp_bvars, goal_bvars) =
      fun FAIL f addr bvars term ->
         match get_core term, addr with
            _, [] ->
               f bvars term
          | Sequent s, [ArgAddr] ->
               let term, arg = f bvars s.sequent_args in
                  mk_sequent_term (**)
                     { sequent_args = term;
                       sequent_hyps = s.sequent_hyps;
                       sequent_goals = s.sequent_goals
                     }, arg
          | Sequent s, [HypAddr i] ->
               if i>=0 && i < SeqHyp.length s.sequent_hyps then
                  let hyp, arg = match SeqHyp.get s.sequent_hyps i with
                     Hypothesis (v,t) as hyp ->
                        let term, arg = f hyp_bvars t in
                           Hypothesis (v,term), arg
                   | Context (v, conts, ts) ->
                        let t, arg = f bvars (core_term (SOVar(v, conts, ts))) in
                        let v1, conts, subterms = dest_so_var t in
                           if v1 = v then
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
          | (Sequent s, [GoalAddr i]) ->
               if i>=0 && i < SeqGoal.length s.sequent_goals then
                  let term, arg = f goal_bvars (SeqGoal.get s.sequent_goals i) in
                  let aux i' t' =
                    if i' = i then term else t'
                  in mk_sequent_term (**)
                        { sequent_args = s.sequent_args;
                          sequent_hyps = s.sequent_hyps;
                          sequent_goals = SeqGoal.mapi aux s.sequent_goals
                        }, arg
               else DO_FAIL
          | Term t, [Subterm i] ->
               let bterms, arg = path_replace_bterm FAIL f i bvars t.term_terms in
                  mk_term t.term_op bterms, arg
          | SOVar(v, conts, ts), [Subterm i] ->
               let ts, arg = path_replace_terms FAIL f i bvars ts in
                  core_term (SOVar(v, conts, ts)), arg
          | (_, addr1 :: ( (_::_) as addr2)) ->
               my_name FAIL (my_name FAIL f addr2) [addr1] bvars term
          | _ -> DO_FAIL

   let rec path_replace_terms = MAKE_PATH_REPLACE_TERMS(NOTHING, path_replace_terms)
   let rec path_replace_bterm = MAKE_PATH_REPLACE_BTERM(NOTHING, NOTHING, path_replace_bterm)

   IFDEF VERBOSE_EXN THEN
      let rec apply_fun_arg_at_addr_aux =
         APPLY_FUN_AUX(apply_fun_arg_at_addr_aux, path_replace_terms, path_replace_bterm, NOTHING, NOTHING, NOTHING)

      let apply_fun_arg_at_addr =
         fun f addr term ->
            apply_fun_arg_at_addr_aux (addr, term) f addr term
   ELSE
      let rec apply_fun_arg_at_addr =
         APPLY_FUN_AUX(apply_fun_arg_at_addr, path_replace_terms, path_replace_bterm, NOTHING, NOTHING, NOTHING)
   ENDIF

   let add_unit_arg f t =
      f t, ()

   let apply_fun_at_addr f addr term =
      fst (apply_fun_arg_at_addr (add_unit_arg f) addr term)

   let rec path_var_replace_terms = MAKE_PATH_REPLACE_TERMS(bvars, path_var_replace_terms)
   let rec path_var_replace_bterm = MAKE_PATH_REPLACE_BTERM(bvars, SymbolSet.add_list bvars vars, path_var_replace_bterm)

   DEFINE HYP_BVARS = (collect_hyp_bvars (pred i) s.sequent_hyps bvars)
   DEFINE GOAL_BVARS = (collect_goal_bvars s.sequent_hyps bvars)
   IFDEF VERBOSE_EXN THEN
      let rec apply_var_fun_at_addr_aux =
         APPLY_FUN_AUX(apply_var_fun_at_addr_aux, path_var_replace_terms, path_var_replace_bterm, bvars, HYP_BVARS, GOAL_BVARS)

      let apply_var_fun_arg_at_addr =
         fun f addr bvars term ->
            apply_var_fun_at_addr_aux (addr, term) f addr bvars term
   ELSE
      let rec apply_var_fun_arg_at_addr =
         APPLY_FUN_AUX(apply_var_fun_arg_at_addr, path_var_replace_terms, path_var_replace_bterm, bvars, HYP_BVARS, GOAL_BVARS)
   ENDIF

   let add_var_unit_arg f bvars t =
      f bvars t, ()

   let apply_var_fun_at_addr f addr bvars term =
      fst (apply_var_fun_arg_at_addr (add_var_unit_arg f) addr bvars term)

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
   let rec collect_string_of_address = function
      [] -> ""
    | [Subterm i] -> string_of_int i
    | [ArgAddr] -> "Arg"
    | [HypAddr i] -> "Hyp(" ^ string_of_int (i+1) ^ ")"
    | [GoalAddr i] -> "Goal(" ^ string_of_int (i+1) ^ ")"
    | addr1 :: ( (_ :: _) as addr2) ->
         (collect_string_of_address [addr1]) ^ "; " ^ (collect_string_of_address addr2)

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
          | Sequent s ->
               let arg, args = apply_fun_higher_term f coll s.sequent_args in
               let hyps, args = apply_fun_higher_hyps f args (SeqHyp.to_list s.sequent_hyps) in
               let goals, args = apply_fun_higher_terms f args (SeqGoal.to_list s.sequent_goals) in
                  if args == coll then (term, coll) else
                  core_term (Sequent {
                     sequent_args = arg;
                     sequent_hyps = SeqHyp.of_list hyps;
                     sequent_goals = SeqGoal.of_list goals;
                  }), args
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

   and apply_fun_higher_hyps f coll = function
      [] -> [], coll
    | Hypothesis (v, t) :: hyps ->
         let t, args = apply_fun_higher_term f coll t in
         let hyps, args = apply_fun_higher_hyps f args hyps in
            (Hypothesis (v, t) :: hyps, args)
    | Context (c, conts, ts) :: hyps ->
         let ts, args = apply_fun_higher_terms f coll ts in
         let hyps, args = apply_fun_higher_hyps f args hyps in
            (Context (c, conts, ts) :: hyps, args)

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
          | Sequent s -> (*raise(Invalid_argument "Term_addr_ds.apply_var_fun_higher called on a sequent")*)
               let arg, args = apply_var_fun_higher_term f bvars coll s.sequent_args in
               let bvars', hyps, args = apply_var_fun_higher_hyps f bvars args (SeqHyp.to_list s.sequent_hyps) in
               let goals, args = apply_var_fun_higher_terms f bvars' args (SeqGoal.to_list s.sequent_goals) in
                  if args == coll then (term, coll) else
                  core_term (Sequent {
                     sequent_args = arg;
                     sequent_hyps = SeqHyp.of_list hyps;
                     sequent_goals = SeqGoal.of_list goals;
                  }), args
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

   and apply_var_fun_higher_hyps f bvars coll = function
      [] -> bvars, [], coll
    | Hypothesis (v, t) :: hyps ->
         let t, args = apply_var_fun_higher_term f bvars coll t in
         let bvars', hyps, args = apply_var_fun_higher_hyps f (SymbolSet.add bvars v) args hyps in
            (bvars', Hypothesis (v, t) :: hyps, args)
    | Context (c, conts, ts) :: hyps ->
         let ts, args = apply_var_fun_higher_terms f bvars coll ts in
         let bvars', hyps, args = apply_var_fun_higher_hyps f (SymbolSet.add bvars c) args hyps in
            (bvars', Context (c, conts, ts) :: hyps, args)

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
                  [HypAddr (pred n)]
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
                  [GoalAddr (pred n)]
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
                  [GoalAddr 0]
               else if (i > 0) then
                  if i <= hlen then
                     [HypAddr (pred i)]
                  else
                     REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "not enough hyps")))
               else if (-i) <= hlen then
                  [HypAddr (hlen + i)]
               else
                  REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "not enough hyps for a negative addressing")))
       | _ ->
            REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "not a sequent")))
end
