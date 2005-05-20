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
 * Subterm' addressing                                                   *
 ************************************************************************)

INCLUDE "refine_error.mlh"

open Lm_symbol

open Term_addr_sig
open Refine_error_sig
open Term_sig
open Term_ds_sig
open Term_ds
open Term_subst_sig
open Term_op_sig
open Term_man_sig

module TermAddr (**)
   (Term : TermDsSig with module TermTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (RefineError : RefineErrorSig
    with type Types.term = TermType.term
    with type Params.address = addr_item list) =
struct
   open TermMan
   open TermType
   open Term
   open TermSubst
   open RefineError

   module AddrTypes = TermType

   type address = addr_item list

   let compose_address = compose_addr
   let string_of_address = string_of_addr

   external make_address : address -> address = "%identity"
   external dest_address : address -> address = "%identity"

   let rec split_clause_address = function
      ClauseAddr _ as addr :: rest ->
         [addr], rest
    | _ ->
         REF_RAISE (RefineError ("Term_addr_ds.split_clause_address", StringError "address is not a sequent address"))

   IFDEF VERBOSE_EXN THEN
      DEFINE ATERM = (a, term)
   ELSE
      DEFINE ATERM = NOTHING
   ENDIF

   let rec subterm_exists t addr =
      (addr = []) ||
      match get_core t, addr with
         SOVar(_, _, terms), (Subterm i) :: addr ->
            begin match make_index_opt i (List.length terms) with
               None -> false
             | Some i  -> subterm_exists (List.nth terms i) addr
            end
       | SOContext (_, t, _, terms), (Subterm i) :: addr ->
            begin match make_index_opt i (List.length terms + 1) with
               None -> false
             | Some i  -> subterm_exists (List.nth (terms @ [t]) i) addr
            end
       | Term t, (Subterm i) :: addr ->
            begin match make_index_opt i (List.length t.term_terms) with
               None -> false
             | Some i  -> subterm_exists (List.nth t.term_terms i).bterm addr
            end
       | Sequent s, ArgAddr :: addr ->
            subterm_exists s.sequent_args addr
       | Sequent s, ClauseAddr 0 :: addr ->
            subterm_exists s.sequent_concl addr
       | Sequent s, ClauseAddr i :: addr ->
            begin match make_index_opt i (SeqHyp.length s.sequent_hyps) with
               None -> false
             | Some i -> subterm_exists_hyp addr (SeqHyp.get s.sequent_hyps i)
            end
       | _ ->
            false

   and subterm_exists_hyp addr = function
      Hypothesis (_, t) -> subterm_exists t addr
    | Context (_, _, ts) ->
         begin match addr with
            [] -> true
          | Subterm i :: addr ->
               begin match make_index_opt i (List.length ts) with
                  None -> false
                | Some i ->
                     subterm_exists (List.nth ts i) addr
               end
          | _ -> false
         end

   let rec find_subterm_term vars addrs addr arg t =
      let addrs = if arg t vars then (List.rev addr) :: addrs else addrs in
         match get_core t with
            FOVar _ -> addrs
          | SOVar(_,_,terms) -> find_subterm_terms vars addrs addr arg 1 terms
          | SOContext(v,t,_,terms) ->
               let addrs = find_subterm_terms vars addrs addr arg 1 terms in
               let addr = Subterm ((List.length terms) + 1) :: addr in
                  find_subterm_term (SymbolSet.add vars v) addrs addr arg t
          | Term t -> find_subterm_bterms vars addrs addr arg 1 t.term_terms
          | Sequent s ->
               let addrs = find_subterm_term vars addrs (ArgAddr :: addr) arg s.sequent_args in
                  find_subterm_hyps vars addrs addr arg s 0 (SeqHyp.length s.sequent_hyps)
          | Hashed _ | Subst _ -> fail_core "find_subterm_term"

   and find_subterm_bterms vars addrs addr arg index = function
      [] -> addrs
    | bterm :: bterms ->
         let addrs = find_subterm_term (SymbolSet.add_list vars bterm.bvars) addrs (Subterm index :: addr) arg bterm.bterm in
            find_subterm_bterms vars addrs addr arg (succ index) bterms

   and find_subterm_terms vars addrs addr arg index = function
      [] -> addrs
    | t :: ts ->
         let addrs = find_subterm_term vars addrs (Subterm index :: addr) arg t in
            find_subterm_terms vars addrs addr arg (succ index) ts

   and find_subterm_hyps vars addrs addr arg s i len =
      if i = len then
         find_subterm_term vars addrs (ClauseAddr 0 :: addr) arg s.sequent_concl
      else
         let addr' = ClauseAddr (i + 1) :: addr in
            match SeqHyp.get s.sequent_hyps i with
              Hypothesis (v, t) ->
                 let addrs = find_subterm_term vars addrs addr' arg t in
                    find_subterm_hyps (SymbolSet.add vars v) addrs addr arg s (succ i) len
            | Context(v, _, ts) ->
                 let addrs = find_subterm_terms vars addrs addr' arg 1 ts in
                    find_subterm_hyps (SymbolSet.add vars v) addrs addr arg s (succ i) len

   let find_subterm t arg =
      List.rev (find_subterm_term SymbolSet.empty [] [] arg t)

   (*
    * Traslate a [-lenght..-1]U[1..length] index into a [0..length-1] one.
    *)
   let make_index_name = "getnth"
   let make_index ATERM i length =
      if i > 0 then
         if i > length then
            REF_RAISE(RefineError (make_index_name, AddressError (a, term)))
         else
            (i - 1)
      else
         let i = length + i in
            if i < 0 then
               REF_RAISE(RefineError (make_index_name, AddressError (a, term)))
            else
               i
   (*
    * Get a subterm.
    *)
   let getnth ATERM terms i =
      if i = 0 then
         raise (Invalid_argument "Term_addr_ds.getnth: got Subterm 0")
      else
         List.nth terms (make_index ATERM i (List.length terms))

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
             | SOContext(_, t, _, ts), Subterm i ->
                  getnth ATERM (ts @ [t]) i
             | SOVar(_, _, ts), Subterm i ->
                  getnth ATERM ts i
             | Sequent s, ArgAddr ->
                  s.sequent_args
             | Sequent s, ClauseAddr 0 ->
                  s.sequent_concl
             | Sequent s, ClauseAddr i ->
                  let i = make_index ATERM i (SeqHyp.length s.sequent_hyps) in
                     begin match SeqHyp.get s.sequent_hyps i with
                        Hypothesis (_, t) -> t
                      | Context (v, conts, ts) -> core_term (SOVar(v, conts, ts))
                     end
             | _ -> REF_RAISE(RefineError (term_subterm_name, AddressError (a, term)))
         in
            term_subterm t rest

   (*
    * Just get the subterm addresses.
    *)
   let subterm_addresses =
      let rec make_path_list i =
         if i = 0 then [] else [Subterm i] :: (make_path_list (pred i))
      in let rec make_hyppath_list i addr addrs =
         if i = 0 then addrs else make_hyppath_list (pred i) addr ([addr; Subterm i] :: addrs)
      in let rec make_hyp_list i hyps addrs =
         if i = 0 then
            addrs
         else
            let i' = pred i in
            make_hyp_list i' hyps (
               match SeqHyp.get hyps i' with
                  Hypothesis _ -> [ClauseAddr i] :: addrs
                | Context(_,_,ts) -> make_hyppath_list (List.length ts) (ClauseAddr i) addrs
            )
      in fun t -> match get_core t with
         Term t -> make_path_list (List.length t.term_terms)
       | FOVar _ -> []
       | SOVar (_, _, ts) -> make_path_list (List.length ts)
       | SOContext (_, _, _, ts) -> make_path_list (List.length ts + 1)
       | Sequent s ->
            concl_addr :: (make_hyp_list (SeqHyp.length s.sequent_hyps) s.sequent_hyps [[ArgAddr]])
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

   let fail_addr (addr, term) =
      REF_RAISE(RefineError ("Term_addr_ds.apply_*_fun_*", AddressError (addr, term)))

   DEFINE MAKE_PATH_REPLACE_TERMS(bvars, path_replace_terms) =
      fun f i bvars ts ->
         match i, ts with
            0, (t::ts) ->
               let t, arg = f bvars t
                  in (t::ts), arg
          | _, (t::ts) ->
               let ts, arg = path_replace_terms f (pred i) bvars ts in
                  (t::ts), arg
          | _, [] ->
               raise (Invalid_argument "Term_addr_ds.path_replace_terms: internal error")

   DEFINE MAKE_PATH_REPLACE_BTERM(bvars, zero_case, path_replace_bterm) =
      fun f i bvars bterms ->
         match i, bterms with
            (0, bt :: bterms) ->
               zero_case
          | (_, bterm :: bterms) ->
               let bterms, arg = path_replace_bterm f (pred i) bvars bterms in
                  bterm :: bterms, arg
          | _, [] ->
               raise (Invalid_argument "Term_addr_ds.path_replace_bterm: internal error")

   DEFINE APPLY_FUN_AUX(my_name, path_replace_terms, path_replace_bterm, bvars, v_bvars, concl_case, hyp_case) =
      fun FAIL f addr bvars term ->
         match get_core term, addr with
            _, [] ->
               f bvars term
          | Sequent s, [ArgAddr] ->
               let term, arg = f bvars s.sequent_args in
                  mk_sequent_term {s with sequent_args = term}, arg
          | Sequent s, [ClauseAddr 0] ->
               concl_case
          | Sequent s, [ClauseAddr i] ->
               hyp_case
          | Term t, [Subterm i] ->
               let i = make_index FAIL i (List.length t.term_terms) in
               let bterms, arg = path_replace_bterm f i bvars t.term_terms in
                  mk_term t.term_op bterms, arg
          | SOVar(v, conts, ts), [Subterm i] ->
               let i = make_index FAIL i (List.length ts) in
               let ts, arg = path_replace_terms f i bvars ts in
                  core_term (SOVar(v, conts, ts)), arg
          | SOContext(v, t, conts, ts), [Subterm i] ->
               let len = List.length ts in
               let i = make_index FAIL i (len + 1) in
                  if i = len then
                     let t, arg = f v_bvars t in
                        core_term (SOContext(v, t, conts, ts)), arg
                  else
                     let ts, arg = path_replace_terms f i bvars ts in
                        core_term (SOContext(v, t, conts, ts)), arg
          | (_, addr1 :: ( (_::_) as addr2)) ->
               my_name FAIL (my_name FAIL f addr2) [addr1] bvars term
          | _ -> DO_FAIL

   let rec path_replace_terms = MAKE_PATH_REPLACE_TERMS(NOTHING, path_replace_terms)
   let rec path_replace_bterm = MAKE_PATH_REPLACE_BTERM(NOTHING,
      (let t, arg = f bt.bterm in mk_bterm bt.bvars t :: bterms, arg),
      path_replace_bterm)

   DEFINE CONCL_CASE =
      let term, arg = f s.sequent_concl in
         mk_sequent_term {s with sequent_concl = term}, arg
   DEFINE HYP_CASE =
      let i = make_index FAIL i (SeqHyp.length s.sequent_hyps) in
      let hyp, arg =
         match SeqHyp.get s.sequent_hyps i with
            Hypothesis (v,t) as hyp ->
               let term, arg = f t in
                  Hypothesis (v,term), arg
          | Context (v, conts, ts) ->
               let t, arg = f (core_term (SOVar(v, conts, ts))) in
               let v1, conts, subterms = dest_so_var t in
                  if Lm_symbol.eq v1 v then
                     Context (v, conts, subterms), arg
                  else DO_FAIL
         in
         let aux i' hyp' =
            if i' = i then hyp else hyp'
         in
            mk_sequent_term {s with sequent_hyps = SeqHyp.mapi aux s.sequent_hyps}, arg
   IFDEF VERBOSE_EXN THEN
      let rec apply_fun_arg_at_addr_aux =
         APPLY_FUN_AUX(apply_fun_arg_at_addr_aux, path_replace_terms, path_replace_bterm, NOTHING, NOTHING, CONCL_CASE, HYP_CASE)

      let apply_fun_arg_at_addr =
         fun f addr term ->
            apply_fun_arg_at_addr_aux (addr, term) f addr term
   ELSE
      let rec apply_fun_arg_at_addr =
         APPLY_FUN_AUX(apply_fun_arg_at_addr, path_replace_terms, path_replace_bterm, NOTHING, NOTHING, CONCL_CASE, HYP_CASE)
   ENDIF

   let add_unit_arg f t =
      f t, ()

   let apply_fun_at_addr f addr term =
      fst (apply_fun_arg_at_addr (add_unit_arg f) addr term)

   let rec path_var_replace_terms = MAKE_PATH_REPLACE_TERMS(bvars, path_var_replace_terms)
   let rec path_var_replace_bterm = MAKE_PATH_REPLACE_BTERM(bvars,
      (let bt = TermSubst.dest_bterm_and_rename bvars bt in
       let t, arg = f (SymbolSet.add_list bvars bt.bvars) bt.bterm in
          mk_bterm bt.bvars t :: bterms, arg),
      path_var_replace_bterm)

   DEFINE VARS_RUN_HYPS = fun hyps bvars sub ->
      function
         [] ->
            List.rev hyps, bvars, sub
       | Hypothesis(v, t) :: rest ->
            let t = apply_subst sub t in
            let v, sub =
               if SymbolSet.mem bvars v then
                  let v' = new_name v (SymbolSet.mem bvars) in
                     v', (v, mk_var_term v') :: sub
               else
                  v, sub
            in
               var_run_hyps (Hypothesis(v, t) :: hyps) (SymbolSet.add bvars v) sub rest
       | Context(v, cts, ts) :: rest ->
            var_run_hyps (Context(v, cts, List.map (apply_subst sub) ts) :: hyps) (SymbolSet.add bvars v) sub rest

   DEFINE VARS_APPLY_FUN_HYPS = fun FAIL f hyps bvars sub i ->
      function
         [] -> raise (Invalid_argument "Term_addr_ds.VARS_APPLY_FUN_HYPS: internal error")
       | hyp :: rest ->
            let v, hyp, sub =
               match hyp with
                  Hypothesis (v, t) ->
                     let t = apply_subst sub t in
                        if SymbolSet.mem bvars v then
                           let v' = new_name v (SymbolSet.mem bvars) in
                              v', Hypothesis (v', t), (v, mk_var_term v') :: sub
                        else
                           v, Hypothesis (v, t), sub
                | Context(v, cts, ts) ->
                      v, Context(v, cts, List.map (apply_subst sub) ts), sub
            in
               if i = 0 then
                  let hyp, arg =
                     match hyp with
                        Hypothesis (v, t) ->
                           let t, arg = f bvars t in
                              Hypothesis (v, t), arg
                      | Context (v, conts, ts) ->
                           let t, arg = f bvars (core_term (SOVar(v, conts, ts))) in
                           let v1, conts, subterms = dest_so_var t in
                              if Lm_symbol.eq v1 v then
                                 Context (v, conts, subterms), arg
                              else DO_FAIL
                  in
                     (var_run_hyps (hyp::hyps) (SymbolSet.add bvars v) sub rest), arg
               else
                  var_apply_fun_hyps FAIL f (hyp::hyps) (SymbolSet.add bvars v) sub (i-1) rest

   DEFINE VARS_CONCL_CASE =
      let hyps, bvars, sub = var_run_hyps [] bvars [] (SeqHyp.to_list s.sequent_hyps) in
         if sub = [] then
            let term, arg = f bvars s.sequent_concl in
               mk_sequent_term {s with sequent_concl = term}, arg
         else
            let term, arg = f bvars (apply_subst sub s.sequent_concl) in
               mk_sequent_term {s with sequent_concl = term; sequent_hyps = SeqHyp.of_list hyps}, arg

   DEFINE VARS_HYP_CASE =
      let i = make_index FAIL i (SeqHyp.length s.sequent_hyps) in
      let (hyps, _, sub), arg = var_apply_fun_hyps FAIL f [] bvars [] i (SeqHyp.to_list s.sequent_hyps) in
         if sub = [] then
            mk_sequent_term {s with sequent_hyps = SeqHyp.of_list hyps}, arg
         else
            let s =
               { s with
                  sequent_hyps = SeqHyp.of_list hyps;
                  sequent_concl = apply_subst sub s.sequent_concl }
            in
               mk_sequent_term s, arg

   IFDEF VERBOSE_EXN THEN
      let rec apply_var_fun_at_addr_aux =
         APPLY_FUN_AUX(apply_var_fun_at_addr_aux, path_var_replace_terms, path_var_replace_bterm, bvars,
            SymbolSet.add bvars v, VARS_CONCL_CASE, VARS_HYP_CASE)

      and var_apply_fun_hyps = VARS_APPLY_FUN_HYPS
      and var_run_hyps = VARS_RUN_HYPS

      let apply_var_fun_arg_at_addr =
         fun f addr bvars term ->
            apply_var_fun_at_addr_aux (addr, term) f addr bvars term

   ELSE
      let rec apply_var_fun_arg_at_addr =
         APPLY_FUN_AUX(apply_var_fun_arg_at_addr, path_var_replace_terms, path_var_replace_bterm, bvars,
            SymbolSet.add bvars v, VARS_CONCL_CASE, VARS_HYP_CASE)

      and var_apply_fun_hyps = VARS_APPLY_FUN_HYPS
      and var_run_hyps = VARS_RUN_HYPS

   ENDIF

   let add_var_unit_arg f bvars t =
      f bvars t, ()

   let apply_var_fun_at_addr f addr bvars term =
      fst (apply_var_fun_arg_at_addr (add_var_unit_arg f) addr bvars term)

   let replace_subterm_aux subterm term =
      subterm

   let replace_subterm term addr subterm =
      apply_fun_at_addr (replace_subterm_aux subterm) addr term

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
          | SOContext(v, t, conts, ts) ->
               let ts, args = apply_fun_higher_terms f coll ts in
               let t, args = apply_fun_higher_term f args t in
                  if args == coll then (term,coll) else core_term (SOContext(v, t, conts, ts)), args
          | Sequent s ->
               let arg, args = apply_fun_higher_term f coll s.sequent_args in
               let hyps, args = apply_fun_higher_hyps f args (SeqHyp.to_list s.sequent_hyps) in
               let concl, args = apply_fun_higher_term f args s.sequent_concl in
                  if args == coll then (term, coll) else
                  core_term (Sequent {
                     sequent_args = arg;
                     sequent_hyps = SeqHyp.of_list hyps;
                     sequent_concl = concl;
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
          | SOContext(v, t, conts, ts) ->
               let ts, args = apply_var_fun_higher_terms f bvars coll ts in
               let t, args = apply_var_fun_higher_term f (SymbolSet.add bvars v) args t in
                  if args == coll then (term,coll) else core_term (SOContext(v, t, conts, ts)), args
          | Sequent s -> (*raise(Invalid_argument "Term_addr_ds.apply_var_fun_higher called on a sequent")*)
               let arg, args = apply_var_fun_higher_term f bvars coll s.sequent_args in
               let bvars', hyps, args = apply_var_fun_higher_hyps f bvars args (SeqHyp.to_list s.sequent_hyps) in
               let concl, args = apply_var_fun_higher_term f bvars' args s.sequent_concl in
                  if args == coll then (term, coll) else
                  core_term (Sequent {
                     sequent_args = arg;
                     sequent_hyps = SeqHyp.of_list hyps;
                     sequent_concl = concl;
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
    * Strip the initial part of an address.
    *)
   let rec strip_address items addr =
      match items, addr with
         [], _ -> addr
       | item1 :: items, item2 :: addr ->
            if item1 = item2 then
               strip_address items addr
            else
               REF_RAISE(RefineError ("strip_address", StringError "addresses do not match"))
       | _, [] ->
            REF_RAISE(RefineError ("strip_address", StringError "address is too short"))
end
