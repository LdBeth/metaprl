(*
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

INCLUDE "refine_error.mlh"

open Lm_symbol

open Opname
open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_subst_sig
open Term_op_sig
open Term_man_gen_sig
open Term_addr_sig

module TermAddr (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermMan: TermManGenSig with module ManTypes = TermType)
   (RefineError : RefineErrorSig
    with module Types = TermType
    with type Params.address = addr_item list) =
struct
   open TermType
   open Term
   open TermSubst
   open TermMan
   open RefineError

   module AddrTypes = TermType

   type address = addr_item list

   external make_address : address -> address = "%identity"
   external dest_address : address -> address = "%identity"

   let compose_address = compose_addr
   let string_of_address = string_of_addr

   let rec split_clause_address = function
      ClauseAddr _ as addr :: rest ->
         [addr], rest
    | _ ->
         REF_RAISE (RefineError ("Term_addr_gen.split_clause_address", StringError "address is not a sequent address"))

   let find_subterm t arg =
      let rec search vars addrs addr t =
         let addrs = if arg t vars then (List.rev addr) :: addrs else addrs in
            search_bterms vars addrs addr 1 (dest_term t).term_terms
      and search_bterms vars addrs addr index = function
         [] -> addrs
       | bterm :: bterms ->
            let bterm = dest_bterm bterm in
            let addrs = search (SymbolSet.add_list vars bterm.bvars) addrs (Subterm index :: addr) bterm.bterm in
               search_bterms vars addrs addr (succ index) bterms
      in
         List.rev (search SymbolSet.empty [] [] t)

   (*
    * A version of the nth_hyp capable of returning the context term.
    *)
   let nth_hyp_name = "Term_addr_gen.nth_hyp"
   let nth_hyp t i =
      if i <= 0 then REF_RAISE(RefineError (nth_hyp_name, StringError "hyp number is not positive"));
      let rec aux i term =
         let { term_op = op; term_terms = bterms } = dest_term term in
         let opname = (dest_op op).op_name in
            if Opname.eq opname hyp_opname then
               let t, _, term = match_hyp_all nth_hyp_name t bterms in
                  if i = 0 then t else aux (i - 1) term
            else if Opname.eq opname context_opname then
               if i = 0 then term else aux (i - 1) (match_context op nth_hyp_name t bterms)
            else if Opname.eq opname concl_opname then
               REF_RAISE(RefineError (nth_hyp_name, StringError "hyp is out of range"))
            else
               REF_RAISE(RefineError (nth_hyp_name, TermMatchError (t, "malformed sequent")))
      in
         aux (pred i) (body_of_sequent t)

   let rec subterm_exists t = function
      [] -> true
    | Subterm 0 :: _ ->
         raise (Invalid_argument "Term_addr_gen.subterm_exists: got Subterm 0")
    | Subterm i :: addr ->
         let t = dest_term t in
            begin match make_index_opt i (List.length t.term_terms) with
               None -> false
             | Some i -> subterm_exists (dest_bterm (List.nth t.term_terms i)).bterm addr
            end
    | ArgAddr :: addr ->
         is_sequent_term t && subterm_exists (snd (dest_sequent_outer_term t)) addr
    | ClauseAddr 0 :: addr ->
         is_sequent_term t && subterm_exists (concl t) addr
    | ClauseAddr i :: addr ->
         is_sequent_term t &&
         let n = num_hyps t in
            begin match make_index_opt i n with
               None -> false
             | Some i -> subterm_exists (nth_hyp t (i+1)) addr
            end

   IFDEF VERBOSE_EXN THEN
      DEFINE ATERM = (a, term)
   ELSE
      DEFINE ATERM = NOTHING
   ENDIF

   (*
    * Traslate a [-length..-1]U[1..length] index into a [0..length-1] one.
    *)
   let make_index_name = "make_index"
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
    * Get the subterm for any type of path.
    *)
   let rec term_subterm term = function
      [] -> term
    | Subterm 0 :: _ ->
         raise (Invalid_argument "Term_addr_gen.term_subterm: got Subterm 0")
    | (Subterm i :: addr) as a ->
         let term' = dest_term term in
         let i = make_index ATERM i (List.length term'.term_terms) in
            term_subterm (dest_bterm (List.nth term'.term_terms i)).bterm addr
    | ArgAddr :: addr ->
         term_subterm (snd (dest_sequent_outer_term term)) addr
    | ClauseAddr 0 :: addr ->
         term_subterm (concl term) addr
    | ClauseAddr i :: addr when i > 0 ->
         term_subterm (nth_hyp term i) addr
    | ClauseAddr i :: addr ->
         term_subterm (nth_hyp term (num_hyps term + i + 1)) addr

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
      REF_RAISE(RefineError ("apply_*_fun_*", AddressError (addr, term)))

   DEFINE MAKE_REPLACE_BTERM(bvars, dest_bterm, vars_bvars, replace_term, replace_bterm) =
      fun FAIL f addr i bvars bterms ->
         match i, bterms with
            (0, bterm :: bterms) ->
               let { bvars = vars; bterm = term } = dest_bterm in
               let term, arg = replace_term FAIL f addr vars_bvars term in
                  mk_bterm vars term :: bterms, arg
          | (_, bterm :: bterms) ->
               let bterms, arg = replace_bterm FAIL f addr (i - 1) bvars bterms in
                  bterm :: bterms, arg
          | _, [] ->
               raise (Invalid_argument "Term_addr_gen.replace_bterm: internal error")

   DEFINE MAKE_HYP_REPLACE_TERM(bvars, vars_bvars, replace_term, hyp_replace_term) =
      fun FAIL f addr bvars t i ->
         if i = 0 then
            match dest_term t with
               { term_op = op } when Opname.eq (dest_op op).op_name context_opname ->
                  let v, term, conts, subterms = dest_context t in
                  let slot = mk_var_term v in
                  let t = mk_context_term v slot conts subterms in
                  let t, arg = replace_term FAIL f addr bvars t in
                  let v1, term1, conts, subterms = dest_context t in
                     if v1 = v && is_var_term term1 && dest_var term1 = v then
                        mk_context_term v term conts subterms, arg
                     else
                        DO_FAIL
             | { term_op = op; term_terms = bterm :: bterms } ->
                  let { bvars = _vars; bterm = term } = dest_bterm bterm in
                  let term, arg = replace_term FAIL f addr vars_bvars term in
                  let bterm = mk_bterm _vars term in
                     mk_term op (bterm :: bterms), arg
             | _ ->
                  DO_FAIL
         else
            match dest_term t with
               { term_op = op; term_terms = [bterm] } ->
                  (* Always take subterm if there is only one *)
                  let { bvars = _vars; bterm = trm } = dest_bterm bterm in
                  let term, arg = hyp_replace_term FAIL f addr vars_bvars trm (i - 1) in
                  let bterm = mk_bterm _vars term in
                     mk_term op [bterm], arg
             | { term_op = op; term_terms = bterm1 :: bterm2 :: bterms } ->
                  if Opname.eq (dest_op op).op_name context_opname then
                     let v, t, conts, args  = dest_context t in
                     let _vars = [v] in
                     let t, arg = hyp_replace_term FAIL f addr vars_bvars t (i - 1) in
                        mk_context_term v t conts args, arg
                  else
                     let { bvars = _vars; bterm = trm } = dest_bterm bterm2 in
                     let term, arg = hyp_replace_term FAIL f addr vars_bvars trm (i - 1) in
                     let bterm = mk_bterm _vars term in
                        mk_term op (bterm1 :: bterm :: bterms), arg
             | _ ->
                  DO_FAIL

   DEFINE APPLY_FUN_AUX(my_name, bvars, replace_bterm, hyp_replace_term) =
      fun FAIL f addr bvars term ->
         match addr with
            [] -> f bvars term
          | Subterm i :: addr ->
               let { term_op = op; term_terms = bterms } = dest_term term in
               let i = make_index FAIL i (List.length bterms) in
               let bterms, arg = replace_bterm FAIL f addr i bvars bterms in
                  mk_term op bterms, arg
          | ArgAddr :: addr ->
               my_name FAIL f (Subterm 1 :: addr) bvars term
          | ClauseAddr i :: addr when i <= 0 ->
               let i = (num_hyps term) + i + 1 in
                  if i <= 0 then DO_FAIL else my_name FAIL f (ClauseAddr i :: addr) bvars term
          | ClauseAddr i :: addr ->
               hyp_replace_term FAIL f addr bvars term i

   let rec apply_fun_arg_at_addr_aux =
      APPLY_FUN_AUX(apply_fun_arg_at_addr_aux, NOTHING, replace_bterm, hyp_replace_term)

   and replace_bterm =
      MAKE_REPLACE_BTERM(NOTHING, dest_bterm bterm, NOTHING, apply_fun_arg_at_addr_aux, replace_bterm)

   and hyp_replace_term =
      MAKE_HYP_REPLACE_TERM(NOTHING, NOTHING, apply_fun_arg_at_addr_aux, hyp_replace_term)

   IFDEF VERBOSE_EXN THEN
      let apply_fun_arg_at_addr f addr term =
            apply_fun_arg_at_addr_aux (addr, term) f addr term
   ELSE
      let apply_fun_arg_at_addr = apply_fun_arg_at_addr_aux
   ENDIF

   let add_unit_arg f t =
      f t, ()

   let apply_fun_at_addr f addr term =
      fst (apply_fun_arg_at_addr (add_unit_arg f) addr term)

   let rec apply_var_fun_at_addr_aux =
      APPLY_FUN_AUX(apply_var_fun_at_addr_aux, bvars, var_replace_bterm, hyp_var_replace_term)

   and var_replace_bterm =
      MAKE_REPLACE_BTERM(bvars, TermSubst.dest_bterm_and_rename bterm bvars, SymbolSet.add_list bvars vars, apply_var_fun_at_addr_aux, var_replace_bterm)

   and hyp_var_replace_term =
      MAKE_HYP_REPLACE_TERM(bvars, SymbolSet.add_list bvars _vars, apply_var_fun_at_addr_aux, hyp_var_replace_term)

   IFDEF VERBOSE_EXN THEN
      let apply_var_fun_arg_at_addr f addr bvars term =
            apply_var_fun_at_addr_aux (addr, term) f addr bvars term
   ELSE
      let apply_var_fun_arg_at_addr = apply_var_fun_at_addr_aux
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
      try let (t,arg) = f term in
             t, (arg::coll)
      with RefineError _ ->
            let dt = dest_term term in
            let (btrms, args) = apply_fun_higher_bterms f coll dt.term_terms in
               if args == coll
               then (term,coll)
               else (mk_term dt.term_op btrms, args)

   and apply_fun_higher_bterms f coll = function
      [] ->
         ([],coll)
    | (bt::btrms) as bterms ->
         let (btrms_new, args) = apply_fun_higher_bterms f coll btrms in
         let dbt = dest_bterm bt in
         let (bt_new, args2) = apply_fun_higher_term f args dbt.bterm in
            if args2 == coll then (bterms, coll)
            else
               let bt_new =
                  if args2 == args
                  then bt else
                  mk_bterm dbt.bvars bt_new
               in
               (bt_new::btrms_new, args2)

   let apply_fun_higher f term = apply_fun_higher_term f [] term

   (*
    * Apply the function at the outermost terms where it does not fail,
    * and also pass in binding variables.
    *)
   let rec apply_var_fun_higher_term f bvars coll term =
      try
         let t, arg = f bvars term in
            t, arg::coll
      with
         RefineError _ ->
            let dt = dest_term term in
            let bterms, args = apply_var_fun_higher_bterms f bvars coll dt.term_terms in
               if args == coll then
                  term, coll
               else
                  mk_term dt.term_op bterms, args

   and apply_var_fun_higher_bterms f bvars coll = function
      [] ->
         [], coll
    | (bterm :: bterms) as bterms' ->
         let bterms_new, args = apply_var_fun_higher_bterms f bvars coll bterms in
         let { bvars = bvars'; bterm = term } = dest_bterm bterm in
         let bterm_new, args = apply_var_fun_higher_term f (SymbolSet.add_list bvars bvars') args term in
            if args == coll then
               bterms', coll
            else
               (mk_bterm bvars' bterm_new) :: bterms_new, args

   let apply_var_fun_higher f bvars term =
      apply_var_fun_higher_term f bvars [] term

   let rec make_path_list i =
      if i = 0 then [] else [Subterm i] :: (make_path_list (pred i))
   let subterm_addresses_name = "Term_addr_gen.subterm_addresses"
   let subterm_addresses t =
      if is_var_term t then []
      else if is_so_var_term t then let _, _, ts = dest_so_var t in make_path_list (List.length ts)
      else if is_context_term t then let _, _, _, ts = dest_context t in make_path_list (List.length ts + 1)
      else if is_sequent_term t then
         let rec aux i term =
            let { term_op = op; term_terms = bterms } = dest_term term in
            let opname = (dest_op op).op_name in
               if Opname.eq opname hyp_opname then
                  [ClauseAddr i] :: (aux (i + 1) (match_hyp subterm_addresses_name t bterms))
               else if Opname.eq opname context_opname then
                  let _, term, _, ts = dest_context term in
                     let addrs = List.map (fun a -> ClauseAddr i :: a) (make_path_list (List.length ts)) in
                        addrs @ (aux (i + 1) term)
               else if Opname.eq opname concl_opname then
                  match bterms with
                     [bt] when is_simple_bterm bt ->
                        [[ArgAddr]]
                   | _ ->
                        REF_RAISE(RefineError (subterm_addresses_name, TermMatchError (t, "malformed sequent")))
               else
                  REF_RAISE(RefineError (subterm_addresses_name, TermMatchError (t, "malformed sequent")))
         in
            concl_addr :: aux 1 (body_of_sequent t)
      else make_path_list (List.length (dest_term t).term_terms)

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

