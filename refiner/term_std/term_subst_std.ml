(*
 * Substitution, alpha equality, and unification.
 *
 * ----------------------------------------------------------------
 *
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
#include "refine_error.h"

open Mp_debug
open Printf
open String_set

open Refine_error_sig
open Term_std_sig
open Term_std

#ifdef VERBOSE_EXN
(* 
 * Show that the file is loading.
 *) 
let _ =
   if !debug_load then 
      eprintf "Loading Term_subst_std%t" eflush

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
#endif

module TermSubst
(Term : TermStdSig
        with type level_exp_var = TermType.level_exp_var
        with type level_exp = TermType.level_exp
        with type param = TermType.param
        with type operator = TermType.operator
        with type term = TermType.term
        with type bound_term = TermType.bound_term

        with type level_exp_var' = TermType.level_exp_var'
        with type level_exp' = TermType.level_exp'
        with type object_id = TermType.object_id
        with type param' = TermType.param'
        with type operator' = TermType.operator'
        with type term' = TermType.term'
        with type bound_term' = TermType.bound_term')
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

   type term_subst = (string * term) list
   type unify_subst = (string list * term option) list

   (************************************************************************
    * Free variable calculations                                           *
    ************************************************************************)

   (*
    * Calculate the list of free variables.
    * Also count second order variables.
    * Just recursively descend the term, keeping track
    * of bound variables.
    *)
   let rec free_vars_term gvars bvars = function
      { term_op = { op_name = opname; op_params = [Var v] }; term_terms = bterms } when Opname.eq opname var_opname ->
         (* This is a variable *)
         let gvars' =
            if List.mem v bvars or List.mem v gvars then
               gvars
            else
               v::gvars
         in
            free_vars_bterms gvars' bvars bterms
    | { term_terms = bterms } ->
         free_vars_bterms gvars bvars bterms

   and free_vars_bterms gvars bvars = function
      { bvars = vars; bterm = term}::l ->
         let bvars' = vars @ bvars in
         let gvars' = free_vars_term gvars bvars' term in
            free_vars_bterms gvars' bvars l

    | [] ->
         gvars

   (* Actual function *)
   let free_vars = free_vars_term [] []

   (* Collect over a list of terms *)
   let free_vars_terms =
      let rec aux gvars = function
         [] -> gvars
       | t::r -> aux (free_vars_term gvars [] t) r
      in
         aux []

   (*
    * See if a variable is free.
    *)
   let is_free_var v =
      let rec free_vars_term bvars = function
         { term_op = { op_name = opname; op_params = [Var v'] };
           term_terms = []
         } when Opname.eq opname var_opname ->
            v' = v
       | { term_terms = bterms } ->
            free_vars_bterms bvars bterms

      and free_vars_bterms bvars = function
         { bvars = bvars'; bterm = term }::t ->
            if List.mem v bvars' then
               free_vars_bterms bvars t
            else
               (free_vars_term (bvars' @ bvars) term) or (free_vars_bterms bvars t)

       | [] -> false
      in
         free_vars_term []

   (*
    * See if any of the variables are free in the terms.
    *)
   let is_free_var_list vars terms =
      let rec free_vars_term vars bvars = function
         { term_op = { op_name = opname; op_params = [Var v'] };
           term_terms = []
         } when Opname.eq opname var_opname ->
            List.mem v' vars
       | { term_terms = bterms } ->
            free_vars_bterms vars bvars bterms

      and free_vars_bterms vars bvars = function
         { bvars = bvars'; bterm = term }::t ->
            let first =
               let vars = List_util.subtract vars bvars' in
                  vars <> [] && free_vars_term vars (bvars' @ bvars) term
            in
               first or free_vars_bterms vars bvars t
       | [] ->
            false
      in
         List.exists (free_vars_term vars []) terms

   (*
    * Similar operation on contexts.
    *)
   let rec context_vars_term cvars = function
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = bterms
      } when Opname.eq opname context_opname ->
         let cvars' =
            if List.mem v cvars then
               cvars
            else
               v::cvars
         in
            context_vars_bterms cvars' bterms
    | { term_terms = bterms } ->
         context_vars_bterms cvars bterms

   and context_vars_bterms cvars = function
      { bterm = t }::l ->
         context_vars_bterms (context_vars_term cvars t) l
    | [] -> cvars

   let context_vars = context_vars_term []

   (*
    * Collect all binding vars.
    *)
   let rec binding_vars_term bvars = function
      { term_terms = bterms } ->
         binding_vars_bterms bvars bterms

   and binding_vars_bterms bvars = function
      { bvars = vars; bterm = t }::l ->
         binding_vars_bterms (binding_vars_term (List_util.union vars bvars) t) l
    | [] -> bvars

   let binding_vars = binding_vars_term []

   (************************************************************************
    * ALPHA EQUALITY                                                       *
    ************************************************************************)

   (*
    * Recursive computation of alpha equality.
    *)
   let equal_params p1 p2 =
      match p1, p2 with
         Number n1, Number n2 ->
            Mp_num.eq_num n1 n2
       | _ ->
            p1 = p2

   let rec equal_term_debug vars t t' =
      match t, t' with
         { term_op = { op_name = opname1; op_params = [Var v] };
           term_terms = []
         },
         { term_op = { op_name = opname2; op_params = [Var v'] };
           term_terms = []
         } when Opname.eq opname1 var_opname & Opname.eq opname2 var_opname ->
            let flag = List_util.check_assoc v v' vars in
               if flag = false then
                  eprintf "alpha_equal_term: failed var %s <> %s%t" v v' eflush;
               flag
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            let flag =
               (Opname.eq name1 name2)
               & (List.length params1 = List.length params2)
               & (List_util.for_all2 equal_params params1 params2)
               & (equal_bterms_debug vars bterms1 bterms2)
            in
               if flag = false then
                  eprintf "alpha_equal_term: failed terms\n\t%a\n\t%a%t" (**)
                     debug_print t
                     debug_print t'
                     eflush;
               flag

   and equal_bterms_debug vars bterms1 bterms2 =
      let equal_bterm = fun
         { bvars = bvars1; bterm = term1 }
         { bvars = bvars2; bterm = term2 } ->
            let flag =
               (List.length bvars1 = List.length bvars2)
               & (equal_term_debug (List_util.zip_list vars bvars1 bvars2) term1 term2)
            in
               if flag = false then
                  eprintf "alpha_equal_bterm: failed\n\t%a.%a\n\t%a.%a%t" (**)
                     print_string_list bvars1 debug_print term1
                     print_string_list bvars2 debug_print term2
                     eflush;
               flag
      in
      let flag =
         (List.length bterms1 = List.length bterms2)
         & (List_util.for_all2 equal_bterm bterms1 bterms2)
      in
         if flag = false then
            eprintf "alpha_equal_bterm: failed bterms:\n";
         flag

   let rec equal_term vars t t' =
      match t, t' with
         { term_op = { op_name = opname1; op_params = [Var v] };
           term_terms = []
         },
         { term_op = { op_name = opname2; op_params = [Var v'] };
           term_terms = []
         } when Opname.eq opname1 var_opname & Opname.eq opname2 var_opname ->
            List_util.check_assoc v v' vars
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            (Opname.eq name1 name2)
            & (List_util.for_all2 equal_params params1 params2)
            & (equal_bterms vars bterms1 bterms2)

   and equal_bterms vars bterms1 bterms2 =
      let equal_bterm = fun
         { bvars = bvars1; bterm = term1 }
         { bvars = bvars2; bterm = term2 } ->
            equal_term (List_util.zip_list vars bvars1 bvars2) term1 term2
      in
         List_util.for_all2 equal_bterm bterms1 bterms2

   let alpha_equal t1 t2 =
#ifdef VERBOSE_EXN
      if !debug_alpha_equal then
         try 
            let result = equal_term /*_debug*/ [] t1 t2 in
            eprintf "alpha_equal: %b:\n%a\n%a%t" result debug_print t1 debug_print t2 eflush;
            result
         with Failure _ ->
            eprintf "alpha_equal: false:\n%a\n%a%t" debug_print t1 debug_print t2 eflush;
            false
      else
#endif
      try equal_term [] t1 t2 with
         Failure _ -> false

   let alpha_equal_vars (t, v) (t', v') =
#ifdef VERBOSE_EXN
      if !debug_alpha_equal then
         try 
            let _ = equal_term (List_util.zip v v') t t' in
            eprintf "alpha_equal_vars: true%t" eflush;
            true
         with Failure _ ->
            eprintf "alpha_equal_vars: false%t" eflush;
            false
      else
#endif
         try equal_term (List_util.zip v v') t t' with
            Failure _ -> false

   (*
    * Check the following:
    *   that t' = t[terms[v''/v''']/v]
    *)
   let eq_comp_var v = function
      { term_op = { op_name = opname; op_params = [Var v'] };
        term_terms = []
      } when Opname.eq opname var_opname ->
         v' = v
    | _ ->
         false

   let equal_comp vars' =
      let rec equal_comp_term vars = function
         { term_op = { op_name = opname; op_params = [Var v] };
           term_terms = []
         }, t' when Opname.eq opname var_opname ->
            begin
               try equal_term vars' t' (List.assoc v vars) with
                  Not_found ->
                     match t' with
                        { term_op = { op_name = opname; op_params = [Var v'] };
                          term_terms = []
                        } when Opname.eq opname var_opname ->
                           not (List_util.assoc_in_range eq_comp_var v' vars) & v = v'
                      | _ ->
                           false
            end
       | { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            Opname.eq name1 name2 & params1 = params2 & equal_comp_bterms vars bterms1 bterms2

      and equal_comp_bterms vars bterms1 bterms2 =
         let equal_comp_bterm = fun
            { bvars = bvars1; bterm = term1 }
            { bvars = bvars2; bterm = term2 } ->
               equal_comp_term (List_util.zip_list vars bvars1 (List.map mk_var_term bvars2)) (term1, term2)
         in
            List_util.for_all2 equal_comp_bterm bterms1 bterms2
      in
         equal_comp_term

   let alpha_equal_match (t, v) (t', v'', v''', terms) =
      try equal_comp (List_util.zip v''' v'') (List_util.zip v terms) (t, t') with
         Failure _ ->
            false

   (************************************************************************
    * Substitution                                                         *
    ************************************************************************)

   (*
    * Utilities for subst.
    *)
   let rec fsubtract l = function
      [] -> l
    | h::t ->
         fsubtract (List_util.subtract l h) t

   (*
    * Add a var list.
    *)
   let add_renames_terms r l =
      let rec aux = function
         [] -> l
       | v::t -> (mk_var_term v)::(aux t)
      in
         aux r

   (*
    * Add a var list onto free vars.
    *)
   let add_renames_fv r l =
      let rec aux = function
         [] -> l
       | v::t -> [v]::(aux t)
      in
         aux r

   (*
    * New variable production.
    * renames are the variables to be renamed,
    * and fv is a list list of variables to avoid.
    * Our algorithm is slow and simple: just append an
    * index and increment until no more collisions.
    *)
   let new_vars renames fv =
      let rec new_var v i =
         (* Try the new value *)
         let rec mem' v = function
            [] -> false
          | h::t -> List.mem v h
         in
         let v' = v ^ (string_of_int i) in
            if mem' v' fv then
               new_var v (i + 1)
            else
               v'
      in
      let rec aux fv = function
         [] -> []
       | v::t ->
            (* Rename the first one, then add it to free vars *)
            let v' = new_var v 1 in
               v'::(aux ([v']::fv) t)
      in
         aux fv renames

   (*
    * First order simultaneous substitution.
    *)
   let subst term terms vars =
      let rec subst_term terms fv vars = function
         { term_op = { op_name = opname; op_params = [Var(v)] }; term_terms = [] } as t
         when Opname.eq opname var_opname->
            (* Var case *)
            begin
               try List.nth terms (List_util.find_index v vars) with
                  Not_found ->
                     t
            end
       | { term_op = op; term_terms = bterms } ->
            (* Other term *)
            { term_op = op; term_terms = subst_bterms terms fv vars bterms }

      and subst_bterms terms fv vars bterms =
         (* When subst through bterms, catch binding occurrences *)
         let rec subst_bterm = function
            { bvars = []; bterm = term } ->
               (* Optimize the common case *)
               { bvars = []; bterm = subst_term terms fv vars term }

          | { bvars = bvars; bterm = term } ->
               (* First subtract bound instances *)
               let flags = List.map (function v -> List.mem v bvars) vars in
               let vars' = List_util.remove_elements flags vars in
               let fv' = List_util.remove_elements flags fv in
               let terms' = List_util.remove_elements flags terms in

               (* If any of the binding variables are free, rename them *)
               let renames = List_util.subtract bvars (fsubtract bvars fv') in
                  if renames <> [] then
                     let fv'' = (free_vars term)::fv' in
                     let renames' = new_vars renames fv'' in
                        { bvars = subst_bvars renames' renames bvars;
                          bterm = subst_term
                                  (add_renames_terms renames' terms')
                                  (add_renames_fv renames' fv')
                                  (renames @ vars')
                                  term
                        }
                  else
                     { bvars = bvars;
                       bterm = subst_term terms' fv' vars' term
                     }
         in
            List.map subst_bterm bterms

      and subst_bvars renames' renames bvars =
         let subst_bvar v =
            try List.nth renames' (List_util.find_index v renames) with
               Not_found -> v
         in
            List.map subst_bvar bvars

      in
         subst_term terms (List.map free_vars terms) vars term

   (*
    * Inverse substitution.
    *)
   let var_subst t t' v =
      let { term_op = { op_name = opname } } = t' in
      let vt = mk_var_term v in
      let rec subst_term = function
         { term_op = { op_name = opname'; op_params = params };
           term_terms = bterms
         } as t ->
            (* Check if this is the same *)
            if Opname.eq opname' opname & alpha_equal t t' then
               vt
            else
               { term_op = { op_name = opname'; op_params = params };
                 term_terms = List.map subst_bterm bterms
               }

      and subst_bterm { bvars = vars; bterm = term } =
         { bvars = vars; bterm = subst_term term }
      in
         subst_term t

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
#ifdef VERBOSE_EXN
      if !debug_unify then
         eprintf "Unify: %a/%a%t" print_term term1 print_term term2 eflush;
#endif
      if (is_var_term term1) then
         let v = dest_var term1 in
         if (is_var_term term2) then
            let v'= dest_var term2 in
            begin try
               if List.assoc v bvars = v' then subst else raise unify_exn
            with
               Not_found ->
                  if List_util.assoc_in_range (=) v' bvars then raise unify_exn else
                  if v=v' then subst else
                  if StringSet.mem constants v then
                     if StringSet.mem constants v' then raise unify_exn else
                     try unify_terms subst constants bvars (subst_assoc subst v') term1 with
                        Not_found ->
                           subst_add subst v' term1
                  else if StringSet.mem constants v' then
                     try unify_terms subst constants bvars (subst_assoc subst v) term2 with
                        Not_found ->
                           subst_add subst v term2
                  else
                     equate_vars subst v v'
            end
         else
            if List_util.assoc_in_dom (=) v bvars then raise unify_exn else
            unify_var_term subst constants bvars v term1 term2
      else if (is_var_term term2) then
         let v = dest_var term2 in
            if List_util.assoc_in_range (=) v bvars then raise unify_exn else
            unify_var_term subst constants bvars v term2 term1
      else
         let op1 = term1.term_op in
         let op2 = term2.term_op in
            if Opname.eq op1.op_name op2.op_name && 
               List_util.for_all2 equal_params op1.op_params op2.op_params 
            then
               unify_bterms subst constants bvars term1.term_terms term2.term_terms
            else
               raise unify_exn

   and unify_var_term subst constants bvars v term1 term2 =
      if StringSet.mem constants v then
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
         if List.mem v tvs then raise_generic_exn else
         check_bvars tvs tl
    
   let rec match_terms subst bvars tm1 tm2 =
      if is_var_term tm1 then
         let v = dest_var tm1 in
            try
               let v' = List.assoc v bvars in
                  if v' = dest_var tm2 then
                     subst
                  else
                     raise_generic_exn
            with
               Not_found ->
                  try
                     let tm1 = List.assoc v subst in
                        if is_var_term tm2 & dest_var tm1 = dest_var tm2 then
                           subst
                        else
                           match_terms subst bvars (List.assoc v subst) tm2
                  with
                     Not_found ->
                        check_bvars (free_vars tm2) bvars;
                        (v, tm2) :: subst
      else
         let { term_op = { op_name = opname1; op_params = params1 };
               term_terms = bterms1
             } = tm1
         in
         let { term_op = { op_name = opname2; op_params = params2 };
               term_terms = bterms2
             } = tm2
         in
            if Opname.eq opname1 opname2 & params1 = params2 then
               match_bterms subst bvars bterms1 bterms2
            else
               raise_generic_exn

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
            raise_generic_exn

   let match_terms subst t1 t2 =
#ifdef VERBOSE_EXN
      try List.rev (match_terms subst [] t1 t2) with
         RefineError (_, GenericError) ->
            raise (RefineError ("Term_subst_std.match_terms", TermPairMatchError (t1, t2)))
#else
            List.rev (match_terms subst [] t1 t2)
#endif

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
    * The generalization is with respect to matching:
    *    1. A variable matches anything
    *    2. A second order variable matches
    *       anything bound according to the subterms
    *    3. A meta-parameter matches a parameter of
    *       the same parameter type.
    *)
   let generalization =
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

      and generalizes_bterm vars bterm1 bterm2 =
         (* Keep track of binding vars *)
         let { bvars = vars1; bterm = term1 } = dest_bterm bterm1 in
         let { bvars = vars2; bterm = term2 } = dest_bterm bterm2 in
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
      in
         (* Start it *)
         generalizes_term

   let generalizes t1 t2 =
      try let _ = generalization [] t1 t2 in true with
         Failure "generalization" ->
            false
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
