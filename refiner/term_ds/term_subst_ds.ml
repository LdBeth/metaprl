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

open Lm_debug
open Lm_symbol
open Lm_printf

open Refine_error_sig
open Term_sig
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

ENDIF

module TermSubst
(Term : TermDsSig with module TermTypes = TermType)
(RefineError : RefineErrorSig
               with type Types.level_exp = TermType.level_exp
               with type Types.param = TermType.param
               with type Types.term = TermType.term
               with type Types.bound_term = TermType.bound_term)
=
struct
   open RefineError
   open TermType
   open Term

   module SubstTypes = TermType

   type term_subst = (var * term) list

   let is_closed_term t =
      SymbolSet.is_empty (free_vars_set t)

   let rec combine_fst_flt_nodups fvs vl tl =
      match vl, tl with
       | v::vl, { core = FOVar v' } :: tl when Lm_symbol.eq v v' ->
            if vl = [] then [] else combine_fst_flt_nodups (SymbolSet.remove fvs v) vl tl
       | [v],[t] ->
            if SymbolSet.mem fvs v then [v,t] else []
       | v::vl, t::tl ->
            if SymbolSet.mem fvs v then
               (v,t) :: combine_fst_flt_nodups (SymbolSet.remove fvs v) vl tl
            else
               combine_fst_flt_nodups fvs vl tl
       | _ ->
            raise (Invalid_argument "Term_subst_ds.combine_fst_flt_nodups")

   let subst t vl tl =
      if vl=[] then t else
      match combine_fst_flt_nodups (free_vars_set t) vl tl with
         [] -> t
       | sub -> core_term (Subst (t,sub))

   let rec fst_flt_nodups fvs = function
    | (v, { core = FOVar v' }) :: tl when Lm_symbol.eq v v' ->
         if tl = [] then [] else fst_flt_nodups (SymbolSet.remove fvs v) tl
    | [v,t] as l ->
         if SymbolSet.mem fvs v then l else []
    | ((v,t) as hd :: tl) as l ->
         if SymbolSet.mem fvs v then
            let res = fst_flt_nodups (SymbolSet.remove fvs v) tl in
            if res == tl then l else hd :: res
         else
            fst_flt_nodups fvs tl
    | _ ->
            raise (Invalid_argument "Term_subst_ds.fst_flt_nodups")

   let apply_subst sub t =
      if sub == [] then t else
      match fst_flt_nodups (free_vars_set t) sub with
         [] -> t
       | sub -> core_term (Subst (t,sub))

   let subst1 term v t =
      if SymbolSet.mem (free_vars_set term) v then
         core_term (Subst (term,[v,t]))
      else
         term

   let is_var_free v t = SymbolSet.mem (free_vars_set t) v

   let free_vars_list t = SymbolSet.to_list (free_vars_set t)
   let free_vars_set = free_vars_set

   let rec need_to_rename avoid = function
      [] -> false
    | v::vs ->
         SymbolSet.mem avoid v or
         need_to_rename (SymbolSet.add avoid v) vs

   let rec compute_renames avoid avoid' = function
      [] -> [], []
    | [v] ->
         if SymbolSet.mem avoid v then
            let v' = new_name v (SymbolSet.mem avoid')
            in [v'], [v, (mk_var_term v')]
         else [v], []
    | v::vs ->
         if List.mem v vs then
            let v' = new_name v (SymbolSet.mem avoid') in
            let vs', ts = compute_renames avoid (SymbolSet.add avoid' v') vs in
               (v'::vs'), ts
         else if SymbolSet.mem avoid v then
            let v' = new_name v (SymbolSet.mem avoid') in
            let vs', ts = compute_renames avoid (SymbolSet.add avoid' v') vs in
               (v'::vs'), ((v, (mk_var_term v')) :: ts)
         else
            let vs', ts = compute_renames avoid avoid' vs in
            (v::vs'), ts

   let dest_bterm_and_rename bt avoid =
      if need_to_rename avoid bt.bvars then
         let avoid' = SymbolSet.add_list (SymbolSet.union avoid (free_vars_set bt.bterm)) bt.bvars in
         let bvars, ts = compute_renames avoid avoid' bt.bvars in
         {
            bvars = bvars;
            bterm = apply_subst ts bt.bterm
         }
      else bt

   let add_vars vars term =
      SymbolSet.union vars (free_vars_set term)

   let free_vars_terms = function
      [hd] ->
         free_vars_set hd
    | hd :: tl ->
         List.fold_left add_vars (free_vars_set hd) tl
    | [] ->
         SymbolSet.empty

   let is_some_var_free vars term =
      match vars with
         [] ->
            false
       | _ ->
            List.exists (SymbolSet.mem (free_vars_set term)) vars

   let is_some_var_free_list vars terms =
      match vars with
         [] ->
            false
       | _ ->
            List.exists (SymbolSet.mem (free_vars_terms terms)) vars

   (************************************************************************
    * ALPHA EQUALITY                                                       *
    ************************************************************************)

   (*
    * Recursive computation of alpha equality.
    *)
   let rec equal_params p1 p2 =
      match p1, p2 with
         Number n1, Number n2 ->
            Lm_num.eq_num n1 n2
       | ParamList pl1, ParamList pl2 ->
            List.for_all2 equal_params pl1 pl2
       | _ ->
            p1 = p2

   let rec remove_var v = function
      [] ->
         []
    | ((v1, v2) :: tl) as l when (v1 = v || v2 = v)->
         if v1<>v2 then (v,v) :: l else l
    | (hd :: tl) as l ->
         let tl' = remove_var v tl in
            if tl'==tl then l else hd :: tl'

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
            else if (SymbolSet.mem set1 v1) || (SymbolSet.mem set2 v2)
            then l else []
    | (((v1,v2) as p) :: tl) as l ->
         let res = eq_filt_vars set1 set2 tl in
         if (res==[]) && (v1=v2) then []
            else if (SymbolSet.mem set1 v1) || (SymbolSet.mem set2 v2) then
               if (res==tl) then l else p::res
            else res

   let rec equal_term vars t t' =
      let vars = eq_filt_vars (free_vars_set t) (free_vars_set t') vars in
      (vars == [] && t==t') || (
      match (get_core t, get_core t') with
         FOVar v, FOVar v' ->
            Lm_list_util.check_assoc v v' vars
       | Term { term_op = { op_name = name1; op_params = params1 }; term_terms = bterms1 },
         Term { term_op = { op_name = name2; op_params = params2 }; term_terms = bterms2 } ->
            Opname.eq name1 name2
                    & Lm_list_util.for_all2 equal_params params1 params2
                    & equal_bterms vars bterms1 bterms2
       | Sequent s1, Sequent s2 ->
            (SeqHyp.length s1.sequent_hyps = SeqHyp.length s2.sequent_hyps) &&
            (equal_term vars s1.sequent_args s2.sequent_args) &&
            (match equal_hyps s1.sequent_hyps s2.sequent_hyps vars 0 with
                None -> false
              | Some vars -> equal_term vars s1.sequent_concl s2.sequent_concl)
       | SOVar(v,conts,ts), SOVar(v',conts',ts') ->
            v=v' && conts = conts' && Lm_list_util.for_all2 (equal_term vars) ts ts'
       | SOContext(v,t,conts,ts), SOContext(v',t',conts',ts') ->
            v=v' && conts = conts' && equal_term (remove_var v vars) t t' && Lm_list_util.for_all2 (equal_term vars) ts ts'
       | _ -> false )

   and equal_bterms vars btrms1 btrms2 =
      match (btrms1,btrms2) with
         [],[] -> true
       | (bt1::btrms1_tl, bt2::btrms2_tl) ->
            (equal_bterms vars btrms1_tl btrms2_tl) &&
            ((vars == [] && bt1 == bt2) || (
            equal_term (join_vars vars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm))
       | _ -> false

   and equal_hyps hyps1 hyps2 vars i =
      if i = SeqHyp.length hyps1 then Some vars else
         match SeqHyp.get hyps1 i, SeqHyp.get hyps2 i with
            Hypothesis (v1,t1), Hypothesis (v2,t2) ->
               if equal_term vars t1 t2 then
                  let vars = if v1=v2 then remove_var v1 vars else (v1,v2)::vars in
                     equal_hyps hyps1 hyps2 vars (succ i)
               else None
          | Context (v1,conts1,ts1), Context (v2,conts2,ts2) ->
            if v1=v2 && conts1 = conts2 && Lm_list_util.for_all2 (equal_term vars) ts1 ts2 then
               equal_hyps hyps1 hyps2 (remove_var v1 vars) (succ i)
            else None
          | _ -> None

   let alpha_equal t1 t2 =
      DEFINE body =
         match get_core t1, get_core t2 with
            (Term _, Term _) | (Sequent _, Sequent _) ->
               equal_term [] t1 t2
          | FOVar v1, FOVar v2 ->
               (v1=v2)
          | SOVar(v1, conts1, ts1), SOVar(v2, conts2, ts2) ->
               v1=v2 && conts1=conts2 && Lm_list_util.for_all2 (equal_term []) ts1 ts2
          | SOContext(v1, t1, conts1, ts1), SOContext(v2, t2, conts2, ts2) ->
               v1=v2 && conts1=conts2 && equal_term [] t1 t2 && Lm_list_util.for_all2 (equal_term []) ts1 ts2
          | _ ->
               false
      IN
      (t1 == t2) ||
      try
         IFDEF VERBOSE_EXN THEN
            let result = body in
               if !debug_alpha_equal then
                  eprintf "alpha_equal: %b:\n%a\n%a%t" result debug_print t1 debug_print t2 eflush;
               result
         ELSE
            body
         ENDIF
      with
         Failure _ -> false

   let alpha_equal_vars t v t' v' =
      DEFINE body = try equal_term (Lm_list_util.zip v v') t t' with
                         Failure _ -> false
      IN
      IFDEF VERBOSE_EXN THEN
         if !debug_alpha_equal then
            try
               let _ = equal_term (Lm_list_util.zip v v') t t' in
               eprintf "alpha_equal_vars: true%t" eflush;
               true
            with Failure _ ->
               eprintf "alpha_equal_vars: false%t" eflush;
               false
         else
            body
      ELSE
         body
      ENDIF

   (*
    * The meaning of
    * var_subst t t' v
    * is to substitute v for occurrences of the term t' in t.
    *)
   let rec var_subst t' fv v t =
      if alpha_equal t t' then
         mk_var_term v
      else
         match t.core with
           Term { term_op = op; term_terms = bterms} ->
              let bterms' = var_subst_bterms bterms t' fv v in
                 if bterms == bterms' then t else mk_term op bterms'
         | FOVar _ -> t
         | SOVar(v', conts, ts) ->
              core_term (SOVar(v', conts, List.map (var_subst t' fv v) ts))
         | SOContext(v', t, conts, ts) ->
              core_term (SOContext(v', var_subst t' fv v t, conts, List.map (var_subst t' fv v) ts))
         | Sequent _ -> raise (Invalid_argument "Term_ds.var_subst: sequents not supported")
         | Hashed _ | Subst _ -> fail_core "var_subst"

   and var_subst_bterms bterms t' fv v =
      match bterms with
         bt::btrms ->
            let bt' = var_subst_bterm bt t' fv v in
            let btrms' = var_subst_bterms btrms t' fv v in
               if (bt == bt') && (btrms == btrms') then bterms else bt'::btrms'
       | [] -> bterms

   and var_subst_bterm bt t' fv v =
      let bt = dest_bterm_and_rename bt fv in
      let term' = var_subst t' fv v bt.bterm in
         if bt.bterm == term' then bt else mk_bterm bt.bvars term'

   let var_subst t t' v =
      var_subst t' (SymbolSet.add (free_vars_set t') v) v t

   let print_string_pair out (v1, v2) =
      fprintf out "%s:%a" v1 debug_print v2

   let print_string_pair_list =
      print_any_list print_string_pair

   let rec equal_fun f bvars sub t t' =
      match get_core t, get_core t' with
         FOVar v, FOVar v' ->
            ( try Lm_list_util.try_check_assoc v v' bvars with Not_found ->
               ( try f t' (List.assoc v sub) with Not_found ->
                  v = v'
            ))
       | FOVar v,_ ->
            not (List.mem_assoc v bvars) &&
            not (Lm_list_util.assoc_in_range SymbolSet.mem (free_vars_set t') bvars) &&
            f t' (List.assoc v sub)
       | Term t1, Term t2 ->
            Opname.eq t1.term_op.op_name t2.term_op.op_name &&
            Lm_list_util.for_all2 equal_params t1.term_op.op_params t2.term_op.op_params &&
            equal_fun_bterms f bvars sub t1.term_terms t2.term_terms
       | Sequent s1, Sequent s2 ->
            (SeqHyp.length s1.sequent_hyps = SeqHyp.length s2.sequent_hyps) &&
            (equal_fun f bvars sub s1.sequent_args s2.sequent_args) &&
            (match equal_fun_hyps s1.sequent_hyps s2.sequent_hyps f bvars sub 0 with
               None -> false
             | Some bvars ->
                  equal_fun f bvars sub s1.sequent_concl s2.sequent_concl)
       | SOVar(v1, conts1, ts1), SOVar(v2, conts2, ts2) ->
            v1=v2 && conts1 = conts2 && Lm_list_util.for_all2 (equal_fun f bvars sub) ts1 ts2
       | SOContext(v1, t1, conts1, ts1), SOContext(v2, t2, conts2, ts2) ->
            v1=v2 && conts1 = conts2 && equal_fun f ((v1,v1)::bvars) sub t1 t2 &&
               Lm_list_util.for_all2 (equal_fun f bvars sub) ts1 ts2
       | _ -> false

   and equal_fun_bterms f bvars sub bterms1 bterms2 =
      let equal_fun_bterm bt1 bt2 =
         equal_fun f
            (Lm_list_util.zip_list bvars bt1.bvars bt2.bvars)
            sub bt1.bterm bt2.bterm
      in
         Lm_list_util.for_all2 equal_fun_bterm bterms1 bterms2

   and equal_fun_hyps hyps1 hyps2 f bvars sub i =
      if i = SeqHyp.length hyps1 then Some bvars else
         match SeqHyp.get hyps1 i, SeqHyp.get hyps2 i with
            Hypothesis (v1,t1), Hypothesis (v2,t2) ->
               if equal_fun f bvars sub t1 t2 then
                  equal_fun_hyps hyps1 hyps2 f ((v1,v2)::bvars) sub (succ i)
               else None
          | Context (v1,conts1,ts1), Context (v2,conts2,ts2) ->
            if v1=v2 && conts1 = conts2 && Lm_list_util.for_all2 (equal_fun f bvars sub) ts1 ts2 then
               equal_fun_hyps hyps1 hyps2 f ((v1,v2)::bvars) sub (succ i)
            else None
          | _ -> None

   (* See refiner/refsig/term_subst_sig.mlz for explanation of this function *)
   let alpha_equal_fun f t vs t' items =
      IFDEF VERBOSE_EXN THEN
         if !debug_subst_ds then begin
            eprintf "Term_subst_ds.alpha_equal_fun:\n\t";
            eprintf "\tt: %a\n" debug_print t;
            eprintf "\tvs: %a\n" output_symbol_list vs;
            eprintf "\tt': %a\n" debug_print t';
            eprintf "\titems: ...%t" eflush
         end
      ENDIF;
      try equal_fun f [] (Lm_list_util.zip vs items) t t'  with
         Failure _ -> false
       | Not_found -> false

   (*
    * Matching is like unification, but variable matches
    * are only allowed on the left.  There is no occurs-check.
    *)
   let rec check_bvars tvs = function
      [] -> ()
    | (_,v)::tl ->
         if SymbolSet.mem tvs v then RAISE_GENERIC_EXN else
         check_bvars tvs tl

   let rec zip_cons l l1 l2 =
      match l1, l2 with
         v1::t1, v2::t2 ->
            zip_cons ((v1, v2) :: l) t1 t2
       | [], [] ->
            l
       | _ ->
            RAISE_GENERIC_EXN

   let rec match_terms subst bvars tm1 tm2 =
      match get_core tm1, get_core tm2 with
         FOVar v, FOVar v' when List.mem_assoc v bvars ->
            if v' = List.assoc v bvars then subst else RAISE_GENERIC_EXN
       | FOVar v, _ ->
            begin try
               let tm1 = List.assoc v subst in
                  if equal_term bvars tm1 tm2 then subst
                     else RAISE_GENERIC_EXN
            with
               Not_found ->
                  check_bvars (free_vars_set tm2) bvars;
                  (v, tm2) :: subst
            end
       | Term { term_op = { op_name = opname1; op_params = params1 }; term_terms = bterms1 },
         Term { term_op = { op_name = opname2; op_params = params2 }; term_terms = bterms2 }
            when Opname.eq opname1 opname2 & params1 = params2 ->
               match_bterms subst bvars bterms1 bterms2
       | SOVar(v1, cs1, ts1), SOVar(v2, cs2, ts2) when v1=v2 && cs1 = cs2 ->
            match_term_lists subst bvars ts1 ts2
       | SOContext(v1, t1, cs1, ts1), SOContext(v2, t2, cs2, ts2) when v1=v2 && cs1 = cs2 ->
            match_term_lists (match_terms subst ((v1,v1)::bvars) t1 t2) bvars ts1 ts2
       | (Sequent _, _) | (_, Sequent _) ->
            raise(Invalid_argument "Term_subst_ds.match_terms called on a sequent")
       | _ -> RAISE_GENERIC_EXN

   and match_term_lists subst bvars ts1 ts2 =
      match ts1, ts2 with
         (t1 :: tl1), (t2 :: tl2) ->
            match_term_lists (match_terms subst bvars t1 t2) bvars tl1 tl2
       | [], [] -> subst
       | _ -> RAISE_GENERIC_EXN

   and match_bterms subst bvars bterms1 bterms2 =
      match bterms1, bterms2 with
         (bt1 :: tl1), (bt2 :: tl2) ->
            let subst' =
               match_terms subst (zip_cons bvars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm
            in
               match_bterms subst' bvars tl1 tl2
       | [], [] -> subst
       | _ -> RAISE_GENERIC_EXN

   let match_terms subst t1 t2 =
      DEFINE body = List.rev (match_terms subst [] t1 t2)
      IN
      IFDEF VERBOSE_EXN THEN
         try body with
            RefineError (_, GenericError) ->
               raise (RefineError ("Term_subst_ds.match_terms", TermPairError (t1, t2)))
      ELSE
         body
      ENDIF

   (************************************************************************
    * Term standardizing.
    ************************************************************************)

   (*
    * Make all the vars different by giving them a unique numeric suffix.
    *)
   let subst_var subst v =
      try dest_var (List.assoc v subst) with
         Not_found ->
            v

   let standardize_var index v =
      let v_str = string_of_symbol v in
      let v' =
         try String.sub v_str 0 (String.rindex v_str '_') with
            Not_found ->
               v_str
      in
         Lm_symbol.make v' index

   let rec standardize_bterm index { bvars = bvars; bterm = t } =
      let bvars, subst, index =
         List.fold_left (fun (bvars, subst, index) v ->
               let v' = standardize_var index v in
               let bvars = v' :: bvars in
               let subst = (v, mk_var_term v') :: subst in
               let index = succ index in
                  bvars, subst, index) ([], [], index) bvars
      in
      let t, index = standardize_term index (apply_subst subst t) in
         { bvars = List.rev bvars; bterm = t }, index

   and standardize_bterms_step (bterms, index) bterm =
      let bterm, index = standardize_bterm index bterm in
         bterm :: bterms, index

   and standardize_terms_step (terms, index) term =
      let term, index = standardize_term index term in
         term :: terms, index

   and standardize_hyps_step (hyps, subst, index) hyp =
      match hyp with
         Hypothesis (v, t) ->
            let v' = standardize_var index v in
            let t, index = standardize_term (succ index) (apply_subst subst t) in
            let subst = (v, mk_var_term v') :: subst in
            let hyps = Hypothesis (v', t) :: hyps in
               hyps, subst, index
       | Context (cv, vars, args) ->
             let vars = List.map (subst_var subst) vars in
             let args = List.fold_left (fun args arg -> apply_subst subst arg :: args) [] args in
             let args, index = List.fold_left standardize_terms_step ([], index) args in
             let hyps = Context (cv, vars, List.rev args) :: hyps in
                hyps, subst, index

   and standardize_term index t =
      match get_core t with
         Term { term_op = op; term_terms = bterms } ->
            let bterms, index = List.fold_left standardize_bterms_step ([], index) bterms in
               mk_term op (List.rev bterms), index
       | FOVar _ ->
            t, index
       | SOVar(v, conts, ts) ->
            let ts, index = List.fold_left standardize_terms_step ([], index) ts in
               core_term(SOVar(v, conts, List.rev ts)), index
       | SOContext(v, t, conts, ts) ->
            let ts, index = List.fold_left standardize_terms_step ([], index) ts in
            let t, index = standardize_term index t in
               core_term(SOContext(v, t, conts, ts)), index
       | Sequent seq ->
            (*
             * XXX: TODO: this could be more efficient if the Array module
             * contained a fold_map function.
             *)
            let arg, index = standardize_term index seq.sequent_args in
            let hyps, subst, index =
               List.fold_left standardize_hyps_step ([], [], index) (SeqHyp.to_list seq.sequent_hyps)
            in
            let concl, index = standardize_term index (apply_subst subst seq.sequent_concl) in
            let seq = {
               sequent_args = arg;
               sequent_hyps = SeqHyp.of_list (List.rev hyps);
               sequent_concl = concl
            } in
               core_term (Sequent seq), index

       | Subst _
       | Hashed _ ->
            fail_core "standardize_term"

   let standardize t =
      fst (standardize_term 1 t)
end
