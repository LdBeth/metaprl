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

open Lm_symbol

open Printf
open Lm_debug

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
   type bound_term = TermType.bound_term
   type bound_term' = TermType.bound_term'

   type term_subst = TermType.term_subst

   let rec combine_fst_flt_nodups fvs vl tl =
      match vl, tl with
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
      [v,t] as l ->
         if SymbolSet.mem fvs v then l else []
    | ((v,t) :: tl) as l ->
         if SymbolSet.mem fvs v then
            let res = fst_flt_nodups (SymbolSet.remove fvs v) tl in
            if res == tl then l else (v,t) :: res
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
                     HypBinding (v,t) ->
                        binding_vars_union (SymbolSet.add (coll_hyps (succ i)) v) t
                   | Hypothesis (t) ->
                        binding_vars_union (coll_hyps (succ i)) t
                   | Context (_,_,[]) ->
                        coll_hyps (succ i)
                   | Context (_,_,ts) ->
                        List.fold_left binding_vars_union (coll_hyps (succ i)) ts
            in
            let goals = seq.sequent_goals in
            let len = SeqGoal.length goals in
            let rec coll_goals i =
               if i = len then coll_hyps 0 else
                  binding_vars_union (coll_goals (succ i)) (SeqGoal.get goals i)
            in coll_goals 0
       | FOVar _ -> SymbolSet.empty
       | SOVar(_, _, ts) -> List.fold_left binding_vars_union SymbolSet.empty ts
       | Subst _ -> ignore (get_core t); binding_vars_term t
       | Hashed d ->
            binding_vars_term (Weak_memo.TheWeakMemo.retrieve_hack d)

   and binding_vars_bterm bt =
      let bv = binding_vars_term bt.bterm in
      match bt.bvars with
            [] -> bv
          | [v] -> SymbolSet.add bv v
          | vars -> SymbolSet.add_list bv vars

   and binding_vars_bterms = function
      [] -> SymbolSet.empty
    | [bt] -> binding_vars_bterm bt
    | bt::l ->
         SymbolSet.union (binding_vars_bterm bt) (binding_vars_bterms l)

   and binding_vars_union vars t = SymbolSet.union (binding_vars_term t) vars

   let binding_vars t =
      SymbolSet.to_list (binding_vars_term t)

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

   let rec terms_context_vars = function
      [] -> []
    | t::ts -> (context_vars t) @ (terms_context_vars ts)

   and context_vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec hyp_context_vars i =
               if i = len then [] else
               match SeqHyp.get hyps i with
                  HypBinding (_,h) | Hypothesis h -> context_vars h @ hyp_context_vars (succ i)
                | Context (v,_,ts) -> v :: (terms_context_vars ts @ hyp_context_vars (succ i))
            in let goals = seq.sequent_goals in
            let len = SeqGoal.length goals in
            let rec goals_context_vars i =
               if i = len then [] else
                  (context_vars (SeqGoal.get goals i) @ goals_context_vars (succ i))
            in (context_vars seq.sequent_args) @ (hyp_context_vars 0) @ (goals_context_vars 0)
       | Term { term_terms = bts } ->
            terms_context_vars (List.map (fun bt -> bt.bterm) bts)
       | SOVar(_, _, ts) -> terms_context_vars ts
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
       | SOVar(v,conts,ts), SOVar(v',conts',ts') ->
            v=v' && conts = conts' && Lm_list_util.for_all2 (equal_term vars) ts ts'
       | _ -> false )

   and equal_bterms vars btrms1 btrms2 =
      match (btrms1,btrms2) with
         [],[] -> true
       | (bt1::btrms1_tl, bt2::btrms2_tl) ->
            (equal_bterms vars btrms1_tl btrms2_tl) &&
            ((vars == [] && bt1 == bt2) || (
            equal_term (join_vars vars bt1.bvars bt2.bvars) bt1.bterm bt2.bterm))
       | _ -> false

   let fake_var=Lm_symbol.add "__@@nosuchvarHACK@@__"

   let rec equal_hyps hyps1 hyps2 vars i =
      if i = SeqHyp.length hyps1 then Some vars else
         match SeqHyp.get hyps1 i, SeqHyp.get hyps2 i with
            (Hypothesis t1 | HypBinding (_,t1)) as h1, (Hypothesis t2 | HypBinding(_,t2) as h2) ->
               if equal_term vars t1 t2 then
                  let v1 = match h1 with HypBinding(v,_) -> v | _ -> fake_var in
                  let v2 = match h2 with HypBinding(v,_) -> v | _ -> fake_var in
                  let vars = if v1=v2 then remove_var v1 vars else (v1,v2)::vars in
                     equal_hyps hyps1 hyps2 vars (succ i)
               else None
          | Context (v1,conts1,ts1), Context (v2,conts2,ts2) ->
            if v1=v2 && conts1 = conts2 && Lm_list_util.for_all2 (equal_term vars) ts1 ts2 then
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
               equal_term [] t1 t2
          | Sequent s1, Sequent s2 ->
               (SeqHyp.length s1.sequent_hyps = SeqHyp.length s2.sequent_hyps) &&
               (SeqGoal.length s1.sequent_goals = SeqGoal.length s2.sequent_goals) &&
               (equal_term [] s1.sequent_args s2.sequent_args) &&
               (match equal_hyps s1.sequent_hyps s2.sequent_hyps [] 0 with
                   None -> false
                 | Some vars -> equal_goals s1.sequent_goals s2.sequent_goals vars (SeqGoal.length s1.sequent_goals - 1))
          | FOVar v1, FOVar v2 ->
               (v1=v2)
          | SOVar(v1, conts1, ts1), SOVar(v2, conts2, ts2) ->
               v1=v2 && conts1=conts2 && Lm_list_util.for_all2 (equal_term []) ts1 ts2
          | _ ->
               false
      IN
      try
         IFDEF VERBOSE_EXN THEN
            let result = BODY in
               if !debug_alpha_equal then
                  eprintf "alpha_equal: %b:\n%a\n%a%t" result debug_print t1 debug_print t2 eflush;
               result
         ELSE
            BODY
         ENDIF
      with
         Failure _ -> false

   let alpha_equal_vars t v t' v' =
      LETMACRO BODY = try equal_term (Lm_list_util.zip v v') t t' with
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
      else match t.core with
           Term { term_op = op; term_terms = bterms} ->
              let bterms' = var_subst_bterms bterms t' v in
                 if bterms == bterms' then t else mk_term op bterms'
         | FOVar _ -> t
         | SOVar(v', conts, ts) ->
               core_term (SOVar(v', conts, List.map (fun t -> var_subst t t' v) ts))
         | Sequent _ -> raise (Invalid_argument "Term_ds.var_subst: sequents not supported")
         | Hashed _ | Subst _ -> fail_core "var_subst"

   and var_subst_bterms bterms t' v =
      match bterms with
         bt::btrms ->
            let bt' = var_subst_bterm bt t' v in
            let btrms' = var_subst_bterms btrms t' v in
               if (bt == bt') && (btrms == btrms') then bterms else bt'::btrms'
       | [] -> bterms

   and var_subst_bterm bt t' v =
      if List.mem v bt.bvars then
         let vars = SymbolSet.add_list (free_vars_set bt.bterm) bt.bvars in
         let v' = new_name v (SymbolSet.mem vars) in
         let rename var = if var = v then v' else var in
         let bt' = subst1 bt.bterm v (mk_var_term v') in
         let bt'' = var_subst bt' t' v in
            if (bt''==bt') then bt
            else mk_bterm (Lm_list_util.smap rename bt.bvars) bt''
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
       | SOVar(v1, conts1, ts1), SOVar(v2, conts2, ts2) ->
            v1=v2 && conts1 = conts2 && Lm_list_util.for_all2 (equal_fun f bvars sub) ts1 ts2
       | _ -> false

   and equal_fun_bterms f bvars sub bterms1 bterms2 =
      let equal_fun_bterm bt1 bt2 =
         equal_fun f
            (Lm_list_util.zip_list bvars bt1.bvars bt2.bvars)
            sub bt1.bterm bt2.bterm
      in
         Lm_list_util.for_all2 equal_fun_bterm bterms1 bterms2

   (* See refiner/refsig/term_subst_sig.mlz for explanation of this function *)
   let alpha_equal_fun f t vs t' items =
      IFDEF VERBOSE_EXN THEN
         if !debug_subst_ds then begin
            eprintf "Term_subst_ds.alpha_equal_fun:\n\t";
            eprintf "\tt: %a\n" debug_print t;
            eprintf "\tvs: %a\n" print_symbol_list vs;
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
         FOVar v, FOVar v'
            when List.mem_assoc v bvars && v' = List.assoc v bvars -> subst
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
      LETMACRO BODY = List.rev (match_terms subst [] t1 t2)
      IN
      IFDEF VERBOSE_EXN THEN
         try BODY with
            RefineError (_, GenericError) ->
               raise (RefineError ("Term_subst_ds.match_terms", TermPairError (t1, t2)))
      ELSE
         BODY
      ENDIF

   (************************************************************************
    * Term standardizing.
    ************************************************************************)

   (*
    * Make all the vars different by giving them a unique numeric suffix.
    *)
   let rec standardize_bterm index { bvars = bvars; bterm = t } =
      let bvars, subst, index =
         List.fold_left (fun (bvars, subst, index) v ->
            let v_str = string_of_symbol v in
            let v' =
               try String.sub v_str 0 (String.rindex v_str '_') with
                  Not_found ->
                     v_str
            in
            let v' = Lm_symbol.make v' index in
            let t = mk_var_term v' in
            let bvars = v' :: bvars in
            let subst = (v, t) :: subst in
            let index = succ index in
               bvars, subst, index) ([], [], index) bvars
      in
      let bvars = List.rev bvars in
      let t = apply_subst subst t in
      let t, index = standardize_term index t in
         { bvars = bvars; bterm = t }, index

   and standardize_bterms_step (bterms, index) bterm =
      let bterm, index = standardize_bterm index bterm in
         bterm :: bterms, index

   and standardize_terms_step (terms, index) term =
      let term, index = standardize_term index term in
         term :: terms, index

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
       | Sequent _ -> raise (Invalid_argument "standardize_term")
       | Subst _| Hashed _ -> fail_core "standardize_term"

   let standardize t =
      fst (standardize_term 0 t)

end
