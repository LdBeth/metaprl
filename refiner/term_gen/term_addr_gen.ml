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

open Printf
open Lm_debug

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_subst_sig
open Term_op_sig
open Term_man_gen_sig

(*
 * Address of a subterm.
 *)
type addr =
   Path of int list
 | NthClause of int * bool
 | Compose of addr * addr

let nth_hd_address i =
   NthClause (i, true)

let nth_tl_address i =
   NthClause (i, false)

module TermAddr (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term = TermType.term
    with type term' = TermType.term'
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term'
    with type operator = TermType.operator
    with type operator' = TermType.operator')
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (TermOp : TermOpSig
    with type term = TermType.term)
   (TermMan: TermManGenSig
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr) =
struct
   open TermType
   open Term
   open TermSubst
   open TermOp
   open TermMan
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

   let rec clause_address_of_address = function
      (NthClause _) as addr ->
         addr
    | Compose (addr, _) ->
         clause_address_of_address addr
    | Path _ ->
         REF_RAISE(RefineError ("clause_address_of_address", StringError "address is not a sequent address"))

   let compose_address path1 path2 =
      match path1 with
         Path [] ->
            path2
       | _ ->
            Compose (path1, path2)

   let find_subterm t arg =
      let rec search addr t =
         if alpha_equal t arg then
            Some addr
         else
            search_bterms addr 0 (dest_term t).term_terms
      and search_bterms addr index = function
         [] ->
            None
       | bterm :: bterms ->
            begin match search (index :: addr) (dest_bterm bterm).bterm with
               Some _ as result -> result
             | None -> search_bterms addr (succ index) bterms
            end
      in
         match search [] t with
            Some addr -> Path (List.rev addr)
          | None ->
               REF_RAISE(RefineError ("Term_addr_gen.find_subterm", StringTermError ("subterm can't be found", arg)))

   IFDEF VERBOSE_EXN THEN
      DEFMACRO ATERM = (a, term)
   ELSE
      DEFMACRO ATERM = NOTHING
   ENDIF

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
   let rec term_subterm_path ATERM t = function
      [] ->
         t
    | i::tl ->
         term_subterm_path ATERM (dest_bterm (getnth ATERM (dest_term t).term_terms i)).bterm tl

   (*
    * Follow a sequent path to a clause.
    *)
   let rec term_subterm_nthpath ATERM flag t = function
      0 ->
         if flag then
            match dest_term t with
               { term_op = op } when Opname.eq (dest_op op).op_name context_opname ->
                  let v, _, conts, subterms = dest_context t in
                     mk_so_var_term v conts subterms
             | { term_op = op; term_terms = bterm :: _ } ->
                  (dest_bterm bterm).bterm
             | _ ->
                  REF_RAISE(RefineError ("term_subterm_nthpath", AddressError (a, term)))
         else
            t
    | i ->
         let { term_op = op; term_terms = bterms } = dest_term t in
            match (dest_term t).term_terms with
               [bterm] ->
                  term_subterm_nthpath ATERM flag (dest_bterm bterm).bterm (i - 1)
             | ((_ :: bterm2 :: _) as bterms) ->
                  if Opname.eq (dest_op op).op_name context_opname then
                     let _, t, _, _ = dest_context t in term_subterm_nthpath ATERM flag t (i - 1)
                  else
                     term_subterm_nthpath ATERM flag (dest_bterm bterm2).bterm (i - 1)
             | _ ->
                  REF_RAISE(RefineError ("term_subterm_nthpath", AddressError (a, term)))

   (*
    * Get the subterm for any type of path.
    *)
   let rec term_subterm term a =
      match a with
         Path addr ->
            term_subterm_path ATERM term addr
       | NthClause (addr, flag) ->
            term_subterm_nthpath ATERM flag term addr
       | Compose (addr1, addr2) ->
            term_subterm (term_subterm term addr1) addr2

   let rec make_path_list i =
      if i = 0 then [] else let i = pred i in (Path [i]) :: (make_path_list i)

   let subterm_addresses t =
      make_path_list (List.length (dest_term t).term_terms)

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

   let fail_addr (addr, term) =
      REF_RAISE(RefineError ("apply_*_fun_*", AddressError (addr, term)))

   DEFMACRO MAKE_PATH_REPLACE_TERM =
      fun FAIL f BVARS t -> function
         i::tl ->
            let { term_op = op; term_terms = bterms } = dest_term t in
            let bterms, arg = PATH_REPLACE_BTERM FAIL f tl i BVARS bterms in
               mk_term op bterms, arg
       | [] ->
            f BVARS t

   DEFMACRO MAKE_PATH_REPLACE_BTERM =
      fun FAIL f tl i BVARS bterms ->
         match i, bterms with
            (0, bterm :: bterms) ->
               let { bvars = vars; bterm = term } = dest_bterm bterm in
               let term, arg = PATH_REPLACE_TERM FAIL f VARS_BVARS term tl in
                  mk_bterm vars term :: bterms, arg
          | (_, bterm :: bterms) ->
               let bterms, arg = PATH_REPLACE_BTERM FAIL f tl (i - 1) BVARS bterms in
                  bterm :: bterms, arg
          | _, [] ->
               DO_FAIL

   DEFMACRO MAKE_NTHPATH_REPLACE_TERM =
      fun FAIL flag f BVARS t i ->
         if i = 0 then
            if flag then
               match dest_term t with
                  { term_op = op } when Opname.eq (dest_op op).op_name context_opname ->
                     let v, term, conts, subterms = dest_context t in
                     let slot = mk_var_term v in
                     let t = mk_context_term v slot conts subterms in
                     let t, arg = f BVARS t in
                     let v1, term1, conts, subterms = dest_context t in
                        if v1 = v && is_var_term term1 && dest_var term1 = v then
                           mk_context_term v term conts subterms, arg
                        else
                           DO_FAIL
                | { term_op = op; term_terms = bterm :: bterms } ->
                     let { bvars = vars; bterm = term } = dest_bterm bterm in
                     let term, arg = f VARS_BVARS term in
                     let bterm = mk_bterm vars term in
                        mk_term op (bterm :: bterms), arg
                | _ ->
                     DO_FAIL
            else
               f BVARS t
         else
            match dest_term t with
               { term_op = op; term_terms = [bterm] } ->
                  (* Always take subterm if there is only one *)
                  let { bvars = vars; bterm = trm } = dest_bterm bterm in
                  let term, arg = NTHPATH_REPLACE_TERM FAIL flag f VARS_BVARS trm (i - 1) in
                  let bterm = mk_bterm vars term in
                     mk_term op [bterm], arg
             | { term_op = op; term_terms = ((bterm1 :: bterm2 :: bterms) as bterms') } ->
                  if Opname.eq (dest_op op).op_name context_opname then
                     let v, t, conts, args  = dest_context t in
                     let vars = [v] in
                     let t, arg = NTHPATH_REPLACE_TERM FAIL flag f VARS_BVARS t (i - 1) in
                        mk_context_term v t conts args, arg
                  else
                     let { bvars = vars; bterm = trm } = dest_bterm bterm2 in
                     let term, arg = NTHPATH_REPLACE_TERM FAIL flag f VARS_BVARS trm (i - 1) in
                     let bterm = mk_bterm vars term in
                        mk_term op (bterm1 :: bterm :: bterms), arg
             | _ ->
                  DO_FAIL

   DEFMACRO APPLY_FUN_AUX MY_NAME =
      fun FAIL f addr BVARS term ->
         match addr with
            Path addr ->
               PATH_REPLACE_TERM FAIL f BVARS term addr
          | NthClause (addr, flag) ->
               NTHPATH_REPLACE_TERM FAIL flag f BVARS term addr
          | Compose (addr1, addr2) ->
               MY_NAME FAIL (MY_NAME FAIL f addr2) addr1 BVARS term

   DEFMACRO BVARS = NOTHING
   DEFMACRO VARS_BVARS = NOTHING
   DEFMACRO PATH_REPLACE_TERM = path_replace_term
   DEFMACRO PATH_REPLACE_BTERM = path_replace_bterm
   DEFMACRO NTHPATH_REPLACE_TERM = nthpath_replace_term

   let rec path_replace_term = MAKE_PATH_REPLACE_TERM
   and path_replace_bterm = MAKE_PATH_REPLACE_BTERM
   let rec nthpath_replace_term = MAKE_NTHPATH_REPLACE_TERM

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
   DEFMACRO PATH_REPLACE_TERM = path_var_replace_term
   DEFMACRO PATH_REPLACE_BTERM = path_var_replace_bterm
   DEFMACRO NTHPATH_REPLACE_TERM = nthpath_var_replace_term

   let rec path_var_replace_term = MAKE_PATH_REPLACE_TERM
   and path_var_replace_bterm = MAKE_PATH_REPLACE_BTERM
   let rec nthpath_var_replace_term = MAKE_NTHPATH_REPLACE_TERM

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
   let rec collect_string_of_nthpath_address_true = function
      0 ->
         "0"
    | i ->
         "@; " ^ (collect_string_of_nthpath_address_true (i - 1))

   let rec collect_string_of_nthpath_address_false = function
      0 ->
         ""
    | 1 ->
         "@"
    | i ->
         "@; " ^ (collect_string_of_nthpath_address_false (i - 1))

   let rec collect_string_of_address = function
      Path addr ->
         String.concat "; " (List.map string_of_int addr)
    | NthClause (addr, flag) ->
         (if flag then "hd" else "tl") ^ "@" ^ (string_of_int addr)
    | Compose (addr1, Path [] ) ->
         collect_string_of_address addr1
    | Compose (addr1, addr2) ->
         (collect_string_of_address addr1) ^ "; " ^ (collect_string_of_address addr2)

   let string_of_address addr =
      "[" ^ collect_string_of_address addr ^ "]"

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

   (*
    * Find the address of the hyp.
    * We just check to make sure the address is valid.
    * Hyps are numbered from 1.
    *)
   let nth_hyp_addr_name = "nth_hyp_addr"
   let nth_hyp_addr t n =
      let n = pred n in
      let addr = nth_hd_address n in
      let rec skip_hyps i term =
         let { term_op = op; term_terms = bterms } = dest_term term in
         let opname = (dest_op op).op_name in
            if Opname.eq opname hyp_opname then
               let term = match_hyp nth_hyp_addr_name t bterms in
                  if i = 0 then
                     addr
                  else
                     skip_hyps (i - 1) term
            else if Opname.eq opname context_opname then
               let term = match_context nth_hyp_addr_name t bterms in
                  if i = 0 then
                     addr
                  else
                     skip_hyps (i - 1) term
            else
               REF_RAISE(RefineError (nth_hyp_addr_name, TermMatchError (t, "not enough hyps")))
      in
         skip_hyps n (goal_of_sequent t)

   (*
    * Find the address of the conclusion.
    * This is the address of the concl term whose car is the desired conclusion
    * not the conclusion itself.
    *
    * Conclusions are numbered from 1.
    *)
   let nth_concl_addr_name = "nth_concl_addr"
   let nth_concl_addr t n =
      let rec skip_concl i n term =
         if n <= 1 then
            nth_tl_address i
         else
            match dest_term term with
               { term_op = op; term_terms = [bterm1; bterm2] }
                  when Opname.eq (dest_op op).op_name concl_opname ->
                  begin
                     match dest_bterm bterm1, dest_bterm bterm2 with
                        ({ bvars = [] }, { bvars = []; bterm = term }) ->
                           skip_concl (i + 1) (n - 1) term
                      | _ ->
                           REF_RAISE(RefineError (nth_concl_addr_name, TermMatchError (t, "malformed conclusion")))
                  end
             | _ ->
                  REF_RAISE(RefineError (nth_concl_addr_name, (TermMatchError (t, "malformed conclusion"))))
      in
      let rec skip_hyps i term =
         let { term_op = op; term_terms = bterms } = dest_term term in
         let opname = (dest_op op).op_name in
            if Opname.eq opname hyp_opname then
               let term = match_hyp nth_concl_addr_name t bterms in
                  skip_hyps (i + 1) term
            else if Opname.eq opname context_opname then
               let term = match_context nth_concl_addr_name t bterms in
                  skip_hyps (i + 1) term
            else if Opname.eq opname concl_opname then
               let term = match_concl nth_concl_addr_name t bterms in
                  skip_concl i n term
            else
               REF_RAISE(RefineError (nth_concl_addr_name, TermMatchError (t, "malformed sequent")))
      in
         skip_hyps 0 (goal_of_sequent t)

   (*
    * Conclusion is number 0,
    * negative numbers index from last hyp towards first.
    *)
   let nth_clause_addr_name = "nth_clause_addr"
   let nth_clause_addr_aux make_address t =
      let rec aux i term =
         let { term_op = op; term_terms = bterms } = dest_term term in
         let opname = (dest_op op).op_name in
            if Opname.eq opname hyp_opname then
               let term = match_hyp nth_clause_addr_name t bterms in
                  aux (i + 1) term
            else if Opname.eq opname context_opname then
               let term = match_context nth_clause_addr_name t bterms in
                  aux (i + 1) term
            else if Opname.eq opname concl_opname then
               make_address i
            else
               REF_RAISE(RefineError (nth_clause_addr_name, TermMatchError (t, "malformed sequent")))
      in
         aux 1 (goal_of_sequent t)

   let make_nth_clause_addr nth_address count i =
      if i < 0 then
         nth_address (count + i)
      else if i = 0 then
         nth_address count
      else
         nth_address i

   let nth_clause_addr t i =
      nth_clause_addr_aux (fun count -> make_nth_clause_addr nth_hd_address count i) t

   let arg_addr = Path [0]
end

