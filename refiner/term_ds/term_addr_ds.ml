(*
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

(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

#include "refine_error.h"

open Printf
open Mp_debug

open Refine_error_sig
open Term_ds_sig
open Term_ds
open Term_op_sig

(*
 * Address of a subterm.
 *)
type addr =
   Path of int list
 | ArgAddr
 | HypAddr of int
 | GoalAddr of int
 | Compose of addr * addr

module AddressType =
struct
   type t = addr
end

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
   (TermOp : TermOpSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr) =
struct
   open TermType
   open Term
   open TermOp
   open RefineError

   type term = TermType.term
   type address = addr

   (*
    * Constructor.
    *)
   let make_address l =
      Path l

   let nth_hd_address i =
      ref_raise (RefineError ("Term_addr_ds.nth_hd_address", StringError "not implemented and should not be used"))

   let nth_tl_address i =
      ref_raise (RefineError ("Term_addr_ds.nth_tl_address", StringError "not implemented and should not be used"))

   let compose_address path1 path2 =
      match path1 with
         Path [] ->
            path2
       | _ ->
            Compose (path1, path2)

   (*
    * Turn the address back into a term count.
    *)
   let depth_of_address = function
      HypAddr i ->
         i
    | GoalAddr i ->
         i
    | _ ->
         ref_raise (RefineError ("Term_addr_ds.depth_of_address", StringError "address is not a sequent address"))

#ifdef VERBOSE_EXN
#  define ATERM a term
#else
#  define ATERM
#endif

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
            ref_raise(RefineError ("getnth", AddressError (a, term)))

   (*
    * Follow an explicit path.
    *)
   let rec term_subterm_path ATERM t = function
      [] ->
         t
    | i::tl ->
         term_subterm_path ATERM (getnth ATERM (dest_term t).term_terms i).bterm tl

   (*
    * Get the subterm for any type of path.
    *)
   let term_subterm_name = "Term_addr_ds.term_subterm"
   let rec term_subterm term a =
      match (get_core term), a with
         (Term _, Path addr) ->
            term_subterm_path ATERM term addr
       | (Sequent s, ArgAddr) ->
            s.sequent_args
       | (Sequent s, HypAddr i) ->
            if i >= 0 && i < SeqHyp.length s.sequent_hyps then
               match SeqHyp.get s.sequent_hyps i with
                  Hypothesis (_,t) -> t
                | Context _ -> ref_raise(RefineError (term_subterm_name, AddressError (a, term)))
            else ref_raise(RefineError (term_subterm_name, AddressError (a, term)))
       | (Sequent s, GoalAddr i) ->
            if i >= 0 && i < SeqGoal.length s.sequent_goals then SeqGoal.get s.sequent_goals i
            else ref_raise(RefineError (term_subterm_name, AddressError (a, term)))
       | (Sequent s, Compose (HypAddr i, ((Path (j :: path)) as addr2))) ->
            (*
             * Special case to address through contexts.
             *)
            if i >= 0 && i < SeqHyp.length s.sequent_hyps then
               match SeqHyp.get s.sequent_hyps i with
                  Hypothesis (_, t) ->
                     term_subterm t addr2
                | Context (_, subterms) ->
                     if j >= 0 && j < List.length subterms then
                        term_subterm (List.nth subterms j) (Path path)
                     else
                        ref_raise(RefineError (term_subterm_name, AddressError (a, term)))
            else
               ref_raise(RefineError (term_subterm_name, AddressError (a, term)))
       | (FOVar _, Path []) ->
            term
       | (_, Compose (addr1, addr2)) ->
            term_subterm (term_subterm term addr1) addr2
       | _ -> ref_raise(RefineError (term_subterm_name, AddressError (a, term)))

   (*
    * Just get the subterm count.
    *)
   let rec term_subterm_path_count ATERM t = function
      [] ->
         subterm_count t
    | i::tl ->
         term_subterm_path_count ATERM (dest_bterm (getnth ATERM (dest_term t).term_terms i)).bterm tl

   let term_subterm_count_name = "Term_addr_ds.term_subterm_count"
   let rec term_subterm_count term a =
      match (get_core term), a with
         (Term _, Path addr) ->
            term_subterm_path_count ATERM term addr
       | (Sequent s, ArgAddr) ->
            subterm_count s.sequent_args
       | (Sequent s, HypAddr i) ->
            if i >= 0 && i < SeqHyp.length s.sequent_hyps then
               match SeqHyp.get s.sequent_hyps i with
                  Hypothesis (_,t) ->
                     subterm_count t
                | Context (_, subterms) ->
                     List.length subterms
            else
               ref_raise(RefineError (term_subterm_count_name, AddressError (a, term)))
       | (Sequent s, GoalAddr i) ->
            if i >= 0 && i < SeqGoal.length s.sequent_goals then
               subterm_count (SeqGoal.get s.sequent_goals i)
            else
               ref_raise(RefineError (term_subterm_count_name, AddressError (a, term)))
       | (Sequent s, Compose (HypAddr i, ((Path (j :: path)) as addr2))) ->
            (*
             * Special case to address through contexts.
             *)
            if i >= 0 && i < SeqHyp.length s.sequent_hyps then
               match SeqHyp.get s.sequent_hyps i with
                  Hypothesis (_, t) ->
                     term_subterm_count t addr2
                | Context (_, subterms) ->
                     if j >= 0 && j < List.length subterms then
                        term_subterm_count (List.nth subterms j) (Path path)
                     else
                        ref_raise(RefineError (term_subterm_count_name, AddressError (a, term)))
            else
               ref_raise(RefineError (term_subterm_count_name, AddressError (a, term)))
       | (FOVar _, Path []) ->
            subterm_count term
       | (_, Compose (addr1, addr2)) ->
            term_subterm_count (term_subterm term addr1) addr2
       | _ ->
            ref_raise(RefineError (term_subterm_count_name, AddressError (a, term)))

   (*
    * Replace a subterm at the specified address.
    * Capture is not taken into account.  This function
    * allows the replacement to compute an extra value.
    *)
#ifdef VERBOSE_EXN
#  define FAIL fail
#  define DO_FAIL fail_addr fail
#else
#  define FAIL
#  define DO_FAIL raise_generic_exn
#endif

   let rec collect_hyp_bvars i hyps bvars =
      if i < 0 then bvars
      else match SeqHyp.get hyps i with
         Hypothesis (v,_) -> [v] :: collect_hyp_bvars (pred i) hyps bvars
       | Context _ -> [] :: collect_hyp_bvars (pred i) hyps bvars

   let collect_goal_bvars hyps = collect_hyp_bvars (SeqHyp.length hyps - 1) hyps

   let fail_addr (addr, term) =
      ref_raise(RefineError ("apply_*_fun_*", AddressError (addr, term)))

#define APPLY_FUN \
   let rec path_replace_term FAIL f BVARS t = function \
      i::tl -> \
         let { term_op = op; term_terms = bterms } = dest_term t in \
         let bterms, arg = path_replace_bterm FAIL f tl i BVARS bterms in \
            mk_term op bterms, arg \
    | [] -> \
         f BVARS t \
\
   and path_replace_bterm FAIL f tl i BVARS bterms = \
      match i, bterms with \
         (0, { bvars = vars; bterm = term } :: bterms) -> \
            let term, arg = path_replace_term FAIL f VARS_BVARS term tl in \
               mk_bterm vars term :: bterms, arg \
       | (_, bterm :: bterms) -> \
            let bterms, arg = path_replace_bterm FAIL f tl (pred i) BVARS bterms in \
               bterm :: bterms, arg \
       | _, [] -> \
            DO_FAIL

#define APPLY_FUN_AUX                                                                     \
   let rec apply_fun_arg_at_addr_aux FAIL f addr BVARS term =                             \
      match (get_core term, addr) with                                                    \
         (Term _, Path addr) ->                                                           \
            path_replace_term FAIL f BVARS term addr                                      \
       | (Sequent s, ArgAddr) ->                                                          \
            let term, arg = f BVARS s.sequent_args in                                     \
               mk_sequent_term (**)                                                       \
                  { sequent_args = term;                                                  \
                    sequent_hyps = s.sequent_hyps;                                        \
                    sequent_goals = s.sequent_goals                                       \
                  }, arg                                                                  \
       | (Sequent s, HypAddr i) ->                                                        \
            if i>=0 && i < SeqHyp.length s.sequent_hyps then                              \
               match SeqHyp.get s.sequent_hyps i with 										\
                  Hypothesis (v,t) -> 											\
                     let term, arg = f HYP_BVARS t in 									\
                     let aux i1 t1 = 											\
                       if i1 = i then Hypothesis (v,term) else t1 							\
                     in 												\
                        mk_sequent_term (**) 										\
                           { sequent_args = s.sequent_args; 								\
                             sequent_hyps = SeqHyp.mapi aux s.sequent_hyps; 						\
                             sequent_goals = s.sequent_goals 								\
                           }, arg 											\
                | Context (v, subterms) -> 										\
                     let slot = mk_var_term v in                                                                        \
                     let t = mk_context_term v slot subterms in 							\
                     let t, arg = f BVARS t in 										\
                     let v1, term1, subterms = dest_context t in 							\
                        if v1 = v && is_var_term term1 && dest_var term1 = v then					\
                           let aux i1 t1 = 										\
                              if i1 = i then 										\
                                 Context (v, subterms) 									\
                              else 											\
                                 t1 											\
                           in 												\
                              mk_sequent_term (**) 									\
                                 { sequent_args = s.sequent_args; 							\
                                   sequent_hyps = SeqHyp.mapi aux s.sequent_hyps; 					\
                                   sequent_goals = s.sequent_goals 							\
                                 }, arg 										\
                        else DO_FAIL                                                                                    \
            else DO_FAIL                                                                                                \
       | (Sequent s, GoalAddr i) -> 											\
            if i>=0 && i < SeqGoal.length s.sequent_goals then 								\
               let term, arg = f GOAL_BVARS (SeqGoal.get s.sequent_goals i) in 							\
               let aux i1 t1 = 												\
                 if i1 = i then term else t1 										\
               in mk_sequent_term (**)                                                    \
                     { sequent_args = s.sequent_args;                                     \
                       sequent_hyps = s.sequent_hyps;                                     \
                       sequent_goals = SeqGoal.mapi aux s.sequent_goals                   \
                     }, arg 												\
            else DO_FAIL 												\
       | (FOVar _, Path []) -> \
            f BVARS term \
       | (_, Compose (addr1, addr2)) -> 										\
            apply_fun_arg_at_addr_aux FAIL (**) 									\
               (apply_fun_arg_at_addr_aux FAIL f addr2) addr1 BVARS term 						\
       | _ -> DO_FAIL

#ifdef VERBOSE_EXN
#  define APPLY_FUN_MAIN APPLY_FUN_AUX \
   let apply_fun_arg_at_addr f addr BVARS term = \
      apply_fun_arg_at_addr_aux (addr, term) f addr BVARS term
#else
#  define apply_fun_arg_at_addr_aux apply_fun_arg_at_addr
#  define APPLY_FUN_MAIN APPLY_FUN_AUX
#endif

#define APPLY_FUN_NOARG \
   let add_unit_arg f BVARS t = \
      f BVARS t, () \
\
   let apply_fun_at_addr f addr BVARS term = \
      fst (apply_fun_arg_at_addr (add_unit_arg f) addr BVARS term)

#define BVARS
#define VARS_BVARS
#define HYP_BVARS
#define GOAL_BVARS
      APPLY_FUN
      APPLY_FUN_MAIN
      APPLY_FUN_NOARG
#undef BVARS
#undef VARS_BVARS
#undef HYP_BVARS
#undef GOAL_BVARS
#define BVARS bvars
#define VARS_BVARS (vars::bvars)
#define HYP_BVARS (collect_hyp_bvars (pred i) s.sequent_hyps BVARS)
#define GOAL_BVARS (collect_goal_bvars s.sequent_hyps BVARS)
#define path_replace_term path_var_replace_term
#define path_replace_bterm path_var_replace_bterm
#define nthpath_replace_term nthpath_var_replace_term
#ifdef VERBOSE_EXN
#  define apply_fun_arg_at_addr_aux apply_var_fun_at_addr_aux
#endif
#define apply_fun_arg_at_addr apply_var_fun_arg_at_addr
#define add_unit_arg add_var_unit_arg
#define apply_fun_at_addr apply_var_fun_at_addr
      APPLY_FUN
      APPLY_FUN_MAIN
      APPLY_FUN_NOARG
#undef apply_fun_arg_at_addr
#undef apply_fun_at_addr

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
    | HypAddr i -> "Hyp(" ^ string_of_int i ^ ")"
    | GoalAddr i -> "Hyp(" ^ string_of_int i ^ ")"
    | Compose (addr1, addr2) ->
         let addr1 = collect_string_of_address addr1 in
         let addr2 = collect_string_of_address addr2 in
            if addr1 = "" then
               addr2
            else if addr2 = "" then
               addr2
            else
               addr1 ^ "; " ^ addr2

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
         let (bt_new, args2) = apply_fun_higher_term f args bt.bterm in
            if args2 == coll then (bterms, coll)
            else
               let bt_new =
                  if args2 == args
                  then bt else
                  mk_bterm bt.bvars bt_new
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
            t, arg :: coll
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
    | ({ bvars = bvars'; bterm = term } :: bterms) as bterms' ->
         let bterms_new, args = apply_var_fun_higher_bterms f bvars coll bterms in
         let bterm_new, args = apply_var_fun_higher_term f (bvars' :: bvars) args term in
            if args == coll then
               bterms, coll
            else
               (mk_bterm bvars' bterm_new) :: bterms_new, args

   let apply_var_fun_higher f bvars term =
      apply_var_fun_higher_term f bvars [] term
end
