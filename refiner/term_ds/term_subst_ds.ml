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
 * Modified by: Jason Hickey
 *)

INCLUDE "refine_error.mlh"

open Printf
open Mp_debug

open Refine_error_sig
open Term_ds_sig
open Term_ds

IFDEF VERBOSE_EXN THEN

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Term_subst_ds%t"

let debug_subst_ds =
   create_debug (**)
      { debug_name = "subst_ds";
        debug_description = "display term_ds substitution operations";
        debug_value = false
      }

let debug_alpha_equal =
   create_debug (**)
      { debug_name = "alpha_equal";
        debug_description = "display alpha equality operations";
        debug_value = false
      }

let debug_unify =
   create_debug (**)
      { debug_name = "unify";
        debug_description = "display unification operations";
        debug_value = false
      }

ENDIF

module TermSubst
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
        with type string_set = TermType.StringSet.t
        with type hypothesis = TermType.hypothesis

        with type level_exp_var' = TermType.level_exp_var'
        with type level_exp' = TermType.level_exp'
        with type object_id = TermType.object_id
        with type param' = TermType.param'
        with type operator' = TermType.operator'
        with type term' = TermType.term'
        with type bound_term' = TermType.bound_term'

        with type term_subst = TermType.term_subst)
(RefineError : RefineErrorSig
               with type level_exp = TermType.level_exp
               with type param = TermType.param
               with type term = TermType.term
               with type bound_term = TermType.bound_term)
=
struct
   open RefineError
   open TermType
   open Term

   type term = TermType.term
   type param = TermType.param

   type unify_subst = (string list * term option) list

   type term_subst = TermType.term_subst

   let subst t tl vl =
      IFDEF VERBOSE_EXN THEN
         if List.length tl <> List.length vl then
              eprintf "subst: %a (%a) (%a)%t" (**)
              debug_print t
              print_string_list vl
              (print_any_list debug_print) tl
              eflush
      ENDIF;
      do_term_subst (List.combine vl tl) t

   let is_free_var v t = StringSet.mem (term_free_vars t) v

   let free_vars t = StringSet.elements (term_free_vars t)

   (*
    * Collect all binding vars.
    *)
   let rec binding_vars_term t =
      match t.core with
         Term t ->
            binding_vars_bterms t.term_terms
       | Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec coll_hyps i =
               if i = len then binding_vars_term seq.sequent_args else
                  match SeqHyp.get hyps i with
                     Hypothesis (v,t) ->
                        binding_vars_union t (StringSet.add v (coll_hyps (succ i)))
                   | Context (v,[]) ->
                        coll_hyps (succ i)
                   | Context (v,ts) ->
                        List.fold_right binding_vars_union ts (coll_hyps (succ i))
            in
            let goals = seq.sequent_goals in
            let len = SeqGoal.length goals in
            let rec coll_goals i =
               if i = len then coll_hyps 0 else
                  binding_vars_union (SeqGoal.get goals i) (coll_goals (succ i))
            in coll_goals 0
       | FOVar _ -> StringSet.empty
       | Subst _ -> ignore (get_core t); binding_vars_term t
       | Hashed d ->
            binding_vars_term (Weak_memo.TheWeakMemo.retrieve_hack d)

   and binding_vars_bterm bt =
      let bv = binding_vars_term bt.bterm in
      match bt.bvars with
            [] -> bv
          | [v] -> StringSet.add v bv
          | vars -> List.fold_right StringSet.add vars bv

   and binding_vars_bterms = function
      [] -> StringSet.empty
    | [bt] -> binding_vars_bterm bt
    | bt::l ->
         StringSet.union (binding_vars_bterm bt) (binding_vars_bterms l)

   and binding_vars_union t vars = StringSet.union (binding_vars_term t) vars

   let binding_vars t =
      StringSet.elements (binding_vars_term t)

   let add_vars vars term =
      StringSet.union vars (term_free_vars term)

   let free_vars_set = function
      [hd] ->
         term_free_vars hd
    | hd :: tl ->
         List.fold_left add_vars (term_free_vars hd) tl
    | [] ->
         StringSet.empty

   let free_vars_terms terms =
      StringSet.elements (free_vars_set terms)

   let is_free_var_list vars terms =
      match vars with
         [] ->
            false
       | _ ->
            let free_set = free_vars_set terms in
               List.exists (fun v -> StringSet.mem free_set v) vars

   let context_vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec context_vars i =
               if i = len then [] else
               match SeqHyp.get hyps i with
                  Hypothesis _ -> context_vars (succ i)
                | Context (v,_) -> v::context_vars (succ i)
            in context_vars 0
       | _ -> []

   (************************************************************************
    * ALPHA EQUALITY                                                       *
    ************************************************************************)

   (*
    * Recursive computation of alpha equality.
    *)
   let rec equal_params p1 p2 =
      match p1, p2 with
         Number n1, Number n2 ->
            Mp_num.eq_num n1 n2
       | ParamList pl1, ParamList pl2 ->
            List.for_all2 equal_params pl1 pl2
       | _ ->
            p1 = p2

   let rec remove_var v = function
      [] ->
         []
    | (v1, v2) :: tl when (v1 = v || v2 = v)->
         remove_var v tl
    | hd :: tl ->
         hd :: remove_var v tl

   let rec join_vars vars vs1 vs2 =
      match vs1,vs2 with
         ([],[]) -> vars
       | (v1::vt1,v2::vt2) ->
            if (v1=v2)
            then join_vars (remove_var v1 vars) vt1 vt2
            else join_vars ((v1,v2)::vars) vt1 vt2
       | _ -> raise (Failure ("join_vars"))

   let rec eq_filt_vars set1 set2 = function
      [] -> []
    | [(v1,v2)] as l ->
         if v1=v2 then []
            else if (StringSet.mem set1 v1) || (StringSet.mem set2 v2)
            then l else []
    | (((v1,v2) as p) :: tl) as l ->
         let res = eq_filt_vars set1 set2 tl in
         if (res==[]) && (v1=v2) then []
            else if (StringSet.mem set1 v1) || (StringSet.mem set2 v2) then
               if (res==tl) then l else p::res
            else res

   let rec equal_term vars t t' =
      let vars = eq_filt_vars (term_free_vars t) (term_free_vars t') vars in
      (vars == [] && t==t') || (
      match (get_core t, get_core t') with
         FOVar v, FOVar v' ->
            List_util.check_assoc v v' vars
       | Term { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         Term { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            Opname.eq name1 name2
                    & List_util.for_all2 equal_params params1 params2
                    & equal_bterms vars bterms1 bterms2
       | _ -> false )

   and equal_bterms vars btrms1 btrms2 =
      match (btrms1,btrms2) with
         [],[] -> true
       | (bt1::btrms1_tl, bt2::btrms2_tl) ->
            (equal_bterms vars btrms1_tl btrms2_tl) &&
            ((vars == [] && bt1 == bt2) || (
            equal_term (join_vars vars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm))
       | _ -> false

   let rec equal_hyps hyps1 hyps2 vars i =
      if i = SeqHyp.length hyps1 then Some vars else
         match SeqHyp.get hyps1 i, SeqHyp.get hyps2 i with
            Hypothesis (v1,t1), Hypothesis (v2,t2) ->
               if equal_term vars t1 t2 then
                  equal_hyps
                     hyps1 hyps2
                     (if v1=v2 then remove_var v1 vars else (v1,v2)::vars)
                     (succ i)
               else None
          | Context (v1,ts1), Context (v2,ts2) ->
            if v1=v2 && List_util.for_all2 (equal_term vars) ts1 ts2 then
               equal_hyps hyps1 hyps2 vars (succ i)
            else None
          | _ -> None

   let rec equal_goals goals1 goals2 vars i =
      i < 0 ||
      ( equal_term vars (SeqGoal.get goals1 i) (SeqGoal.get goals2 i) &&
        equal_goals goals1 goals2 vars (pred i) )

   let alpha_equal t1 t2 =
      LETMACRO BODY =
         match get_core t1, get_core t2 with
            Term _, Term _ ->
               (try equal_term [] t1 t2 with
                  Failure _ -> false)
          | Sequent s1, Sequent s2 ->
               (try
                  (SeqHyp.length s1.sequent_hyps = SeqHyp.length s2.sequent_hyps) &&
                  (SeqGoal.length s1.sequent_goals = SeqGoal.length s2.sequent_goals) &&
                  (equal_term [] s1.sequent_args s2.sequent_args) &&
                  (match equal_hyps s1.sequent_hyps s2.sequent_hyps [] 0 with
                      None -> false
                    | Some vars -> equal_goals s1.sequent_goals s2.sequent_goals vars (SeqGoal.length s1.sequent_goals - 1))
                with
                   Failure _ ->
                      false)
          | FOVar v1, FOVar v2 ->
               (v1=v2)
          | _ ->
               false
      IN
      IFDEF VERBOSE_EXN THEN
         let result = BODY in
            if !debug_alpha_equal then
               eprintf "alpha_equal: %b:\n%a\n%a%t" result debug_print t1 debug_print t2 eflush;
            result
      ELSE
         BODY
      ENDIF

   let alpha_equal_vars t v t' v' =
      LETMACRO BODY = try equal_term (List_util.zip v v') t t' with
                         Failure _ -> false
      IN
      IFDEF VERBOSE_EXN THEN
         if !debug_alpha_equal then
            try
               let _ = equal_term (List_util.zip v v') t t' in
               eprintf "alpha_equal_vars: true%t" eflush;
               true
            with Failure _ ->
               eprintf "alpha_equal_vars: false%t" eflush;
               false
         else
            BODY
      ELSE
         BODY
      ENDIF

   (*
    * The meaning of
    * var_subst t t' v
    * is to substitute v for occurrences of the term t' in t.
    *)
   let rec var_subst t t' v =
      if alpha_equal t t' then mk_var_term v
      else match t with
              {core = Term { term_op = op; term_terms = bterms}} ->
                 let bterms' = var_subst_bterms bterms t' v in
                    if bterms == bterms' then t else mk_term op bterms'
            | { core = FOVar v } -> t
            | _ -> raise (Invalid_argument "Term_ds.var_subst: this is not supposed to happen")

   and var_subst_bterms bterms t' v =
      match bterms with
         bt::btrms ->
            let bt' = var_subst_bterm bt t' v in
            let btrms' = var_subst_bterms btrms t' v in
               if (bt == bt') && (btrms == btrms') then bterms else bt'::btrms'
       | [] -> bterms

   and var_subst_bterm bt t' v =
      if List.mem v bt.bvars then bt
      else let term' = var_subst bt.bterm t' v in
              if bt.bterm == term' then bt else mk_bterm bt.bvars term'

   let print_string_pair out (v1, v2) =
      fprintf out "%s:%a" v1 debug_print v2

   let print_string_pair_list =
      print_any_list print_string_pair

   let eq_comp_var v t =
      match get_core t with
         FOVar v' ->
            v' = v
       | _ ->
            false

   let rec equal_fun f bvars sub t t' =
      match get_core t, get_core t' with
         FOVar v, FOVar v' ->
            ( try List_util.try_check_assoc v v' bvars with Not_found ->
               ( try f t' (List.assoc v sub) with Not_found ->
                  v = v'
            ))
       | FOVar v,_ ->
            not (List.mem_assoc v bvars) &&
            not (List_util.assoc_in_range StringSet.mem (term_free_vars t') bvars) &&
            f t' (List.assoc v sub)
       | Term t1, Term t2 ->
            Opname.eq t1.term_op.op_name t2.term_op.op_name &&
            List_util.for_all2 equal_params t1.term_op.op_params t2.term_op.op_params &&
            equal_fun_bterms f bvars sub t1.term_terms t2.term_terms
       | _ -> false

   and equal_fun_bterms f bvars sub bterms1 bterms2 =
      let equal_fun_bterm bt1 bt2 =
         equal_fun f
            (List_util.zip_list bvars bt1.bvars bt2.bvars)
            sub bt1.bterm bt2.bterm
      in
         List_util.for_all2 equal_fun_bterm bterms1 bterms2

   (* See refiner/refsig/term_subst_sig.mlz for explanation of this function *)
   let alpha_equal_fun f t vs t' items =
      IFDEF VERBOSE_EXN THEN
         if !debug_subst_ds then begin
            eprintf "Term_subst_ds.alpha_equal_fun:\n\t";
            eprintf "\tt: %a\n" debug_print t;
            eprintf "\tvs: %a\n" print_string_list vs;
            eprintf "\tt': %a\n" debug_print t';
            eprintf "\titems: ...%t" eflush
         end
      ENDIF;
      try equal_fun f [] (List_util.zip vs items) t t'  with
         Failure _ -> false
       | Not_found -> false

   (************************************************************************
    * UNIFICATION                                                          *
    ************************************************************************)

   (*
    * Unify two terms.
    *)
   let unify_exn = RefineError ("unify", StringError "terms do not unify")

   (*
    * Utilities.
    *)
   let rec rev_assoc v = function
      (v1, v2)::t ->
         if v2 = v then
            v1
         else
            rev_assoc v t
    | [] ->
         raise Not_found

   let rec zip_cons l l1 l2 =
      match l1, l2 with
         v1::t1, v2::t2 ->
            zip_cons ((v1, v2) :: l) t1 t2
       | [], [] ->
            l
       | _ ->
            raise unify_exn

   (*
    * Substitution is managed as a DAG.
    *)
   let convert_subst (vars, edge) =
      match edge with
         Some edge ->
            (vars, Some (edge, free_vars edge))
       | None ->
            (vars, None)

   let equate_vars subst v1 v2 =
      try
         Cycle_dag.equate subst v1 v2;
         subst
      with
         Cycle_dag.Cycle ->
            raise unify_exn

   let subst_add subst v term =
      try
         Cycle_dag.insert subst v term (free_vars term);
         subst
      with
         Cycle_dag.Cycle ->
            raise unify_exn

   let subst_assoc = Cycle_dag.find

   let get_subst = Cycle_dag.sort

   let subst_of_unify_subst =
      let rec flatten = function
         (nodes, term) :: terms ->
            begin
               match term with
                  Some term ->
                     flatten_nodes nodes term terms
                | None ->
                     flatten_vars nodes terms
            end
       | [] ->
            []
      and flatten_vars vars terms =
         match vars with
            [_] ->
               flatten terms
          | v1 :: ((v2 :: _) as vars) ->
               (v1, mk_var_term v2) :: flatten_vars vars terms
          | [] ->
               raise (Invalid_argument "Term_subst_ds.get_subst")
      and flatten_nodes nodes term terms =
         match nodes with
            [node] ->
               (node, term) :: flatten terms
          | node1 :: ((node2 :: _) as nodes) ->
               (node1, mk_var_term node2) :: flatten_nodes nodes term terms
          | [] ->
               raise (Invalid_argument "Term_subst_ds.get_subst")
      in
         flatten

   let unify_empty = []

   let convert_subst_of_subst (v, t) = ([v], Some t)

   let unify_subst_of_subst subst = List.map convert_subst_of_subst subst

   let add_unify_subst v t s =
      ([v], Some t) :: s

   let new_unify_var s v =
      raise (Failure "new_unify_var: not implemented")

   (*
    * The unification works over the subst.
    *)
   let rec unify_terms subst constants bvars term1 term2 =
      IFDEF VERBOSE_EXN THEN
         if !debug_unify then
            eprintf "Unify: %a/%a%t" print_term term1 print_term term2 eflush
      ENDIF;
      match get_core term1, get_core term2 with
         FOVar v, FOVar v' ->
            begin try
               if List.assoc v bvars = v' then subst else raise unify_exn
            with
               Not_found ->
                  if List_util.assoc_in_range (=) v' bvars then raise unify_exn else
                  if v=v' then subst else
                  if String_set.StringSet.mem constants v then
                     if String_set.StringSet.mem constants v' then raise unify_exn else
                     try unify_terms subst constants bvars (subst_assoc subst v') term1 with
                        Not_found ->
                           subst_add subst v' term1
                  else if String_set.StringSet.mem constants v' then
                     try unify_terms subst constants bvars (subst_assoc subst v) term2 with
                        Not_found ->
                           subst_add subst v term2
                  else
                     equate_vars subst v v'
            end
       | FOVar v, Term _ ->
            if List_util.assoc_in_dom (=) v bvars then raise unify_exn else
            unify_var_term subst constants bvars v term1 term2
       | Term _, FOVar v ->
            if List_util.assoc_in_range (=) v bvars then raise unify_exn else
            unify_var_term subst constants bvars v term2 term1
       | Term trm1, Term trm2 ->
            let op1 = trm1.term_op in
            let op2 = trm2.term_op in
               if Opname.eq op1.op_name op2.op_name &&
                  List_util.for_all2 equal_params op1.op_params op2.op_params
               then
                  unify_bterms subst constants bvars trm1.term_terms trm2.term_terms
               else
                  raise unify_exn
       | _ ->
            raise (Invalid_argument "Term_subst_ds.unify_term is not implemented for some `special' terms")

   and unify_var_term subst constants bvars v term1 term2 =
      if String_set.StringSet.mem constants v then
         raise unify_exn
      else
         try unify_terms subst constants bvars (subst_assoc subst v) term2 with
            Not_found ->
               (* Bound vars occurs check goes here *)
               subst_add subst v term2

   and unify_bterms subst constants bvars bterms1 bterms2 =
      match bterms1, bterms2 with
         (bt1 :: tl1), (bt2 :: tl2) ->
            let subst' =
               unify_terms subst constants (zip_cons bvars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm
            in
               unify_bterms subst' constants bvars tl1 tl2
       | [], [] ->
            subst
       | _ ->
            raise unify_exn

   let unify subst constants term1 term2 =
      get_subst (unify_terms (Cycle_dag.make (List.map convert_subst subst)) constants [] term1 term2)

   (*
    * Matching is like unification, but variable matches
    * are only allowed on the left.  There is no occurs-check.
    *)
   let rec check_bvars tvs = function
      [] -> ()
    | (_,v)::tl ->
         if StringSet.mem tvs v then RAISE_GENERIC_EXN else
         check_bvars tvs tl

   let rec match_terms subst bvars tm1 tm2 =
      if is_var_term tm1 then
         let v = dest_var tm1 in
            try
               let v' = List.assoc v bvars in
                  if v' = dest_var tm2 then
                     subst
                  else
                     RAISE_GENERIC_EXN
            with
               Not_found ->
                  try
                     let tm1 = List.assoc v subst in
                        if is_var_term tm2 & dest_var tm1 = dest_var tm2 then
                           subst
                        else
                           match_terms subst bvars tm1 tm2
                  with
                     Not_found ->
                        check_bvars (term_free_vars tm2) bvars;
                        (v, tm2) :: subst
      else
         let { term_op = { op_name = opname1; op_params = params1 };
               term_terms = bterms1
             } = dest_term tm1
         in
         let { term_op = { op_name = opname2; op_params = params2 };
               term_terms = bterms2
             } = dest_term tm2
         in
            if Opname.eq opname1 opname2 & params1 = params2 then
               match_bterms subst bvars bterms1 bterms2
            else
               RAISE_GENERIC_EXN

   and match_bterms subst bvars bterms1 bterms2 =
      match bterms1, bterms2 with
         (bt1 :: tl1), (bt2 :: tl2) ->
            let subst' =
               match_terms subst (zip_cons bvars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm
            in
               match_bterms subst' bvars tl1 tl2
       | [], [] ->
            subst
       | _ ->
            RAISE_GENERIC_EXN

   let match_terms subst t1 t2 =
      LETMACRO BODY = List.rev (match_terms subst [] t1 t2)
      IN
      IFDEF VERBOSE_EXN THEN
         try BODY with
            RefineError (_, GenericError) ->
               raise (RefineError ("Term_subst_ds.match_terms", TermPairMatchError (t1, t2)))
      ELSE
         BODY
      ENDIF

(************************************)
(* MM-unification module            *)
(************************************)

open Term_unif_ds
(*
module TermType = Term_ds.TermType
module AddressType = Term_addr_ds.AddressType
module RefineError = Refine_error.MakeRefineError (TermType) (AddressType)
module Term = Term_base_ds.Term (RefineError)
*)

module Unification = Term_unif_ds.TermSubstMm(Term)(RefineError)

type unify_subst_mm = unify_subst 
(* !!!! this type will be changed 
* modified: meta-prl/refiner/term_ds/term_subst_ds.ml
*           meta-prl/refiner/term_ds/Files
*           meta-prl/refiner/refsig/term_subst_sig.mlz
*           meta-prl/refiner/term_std/term_subst_std.ml     
*)

let unifiable = Unification.unifiable

let unify_mm = Unification.unify












   (************************************************************************
    * Term generalization                                                  *
    ************************************************************************)

   (*
    * Generalization computation.
    * See if the first term generalizes the second, and
    * compute the alpha conversion.  If the generalization
    * is _not_ true, then raise the exception:
    * Failure "generalization".
    *
    * Generalization is like unification, but it is one-sided:
    * vars in the first term can match terms in the second,
    * but not vice-versa.
    *
    * The generalization is with respect to matching:
    *    1. A variable matches anything
    *    2. A second order variable matches
    *       anything bound according to the subterms
    *    3. A meta-parameter matches a parameter of
    *       the same parameter type.
    *)
   let rec generalizes_term vars t1 t2 =
      if is_so_var_term t1 then
         vars
      else if is_context_term t1 then
         if is_context_term t2 then
            let _, t1', _ = dest_context t1 in
            let _, t2', _ = dest_context t2 in
               generalizes_term vars t1' t2'
         else
            raise (Failure "generalization")
      else if is_so_var_term t2 or is_context_term t2 then
         raise (Failure "generalization")
      else
         (* Regular terms *)
         let { term_op = op1; term_terms = bterms1 } = dest_term t1 in
         let { term_op = op2; term_terms = bterms2 } = dest_term t2 in
         let { op_name = name1; op_params = params1 } = dest_op op1 in
         let { op_name = name2; op_params = params2 } = dest_op op2 in
            if Opname.eq name1 name2 then
               try
                  List_util.iter2 generalizes_param params1 params2;
                  List_util.fold_left2 generalizes_bterm vars bterms1 bterms2
               with
                  Failure _ ->
                     raise (Failure "generalization")
            else
               raise (Failure "generalization")

   and generalizes_param param1 param2 =
      if param1 <> param2 then
         raise (Failure "generalization")

   and generalizes_bterm vars { bvars = vars1; bterm = term1 }
                              { bvars = vars2; bterm = term2 } =
      let aux vars v1 v2 =
         try
            if v2 = List.assoc v1 vars then
               vars
            else
               raise (Failure "generalization")
         with _ ->
               (v1, v2)::vars
      in
      let vars' = List.fold_left2 aux vars vars1 vars2 in
         generalizes_term vars' term1 term2

   let generalization = generalizes_term

   let generalizes t1 t2 =
      try let _ = generalizes_term [] t1 t2 in true with
         Failure "generalization" ->
            false

end
