(*
 * This is the module that implements delayed substitution,
 * keeps track of free variables and does some sharing.
 * ----------------------------------------------------------------
 *
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

INCLUDE "refine_error.mlh"

open Lm_debug
open Lm_symbol

open Lm_printf
open Opname
open Refine_error_sig
open Term_sig
open Term_ds

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_base_ds%t"

let debug_subst =
   create_debug (**)
      { debug_name = "subst";
        debug_description = "Substitution operations";
        debug_value = false
      }

let debug_fv =
   create_debug (**)
      { debug_name = "free_vars";
        debug_description = "Display free variables calculation";
        debug_value = false
      }

module Term
   (RefineError : RefineErrorSig
    with type Types.level_exp = TermType.level_exp
    with type Types.param = TermType.param
    with type Types.term = TermType.term
    with type Types.bound_term = TermType.bound_term)
=
struct
   (************************************************************************
    * Type definitions                                                     *
    ************************************************************************)

   open TermType
   open RefineError

   module TermTypes = TermType

   (*
    * Simple substitution.
    *)
   type term_subst = (var * term) list

   module SeqHypType =
   struct
      type t = hypothesis
   end

   module SeqGoalType =
   struct
      type t = term
   end

   module SeqHyp = Seq_set.Make (SeqHypType)
   module SeqGoal = Seq_set.Make (SeqGoalType)

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Printer is installed by client.
    *)
   let print_term_ref = ref (fun _ _ -> raise (Failure "Term_ds.print_term: printer not installed"))

   let debug_print out t =
      !print_term_ref out t

   let print_term = debug_print

   let rec print_term_list out = function
      [t] ->
         debug_print out t
    | h::t ->
         debug_print out h;
         output_string out ", ";
         print_term_list out t
    | [] ->
         ()

   let install_debug_printer f =
      print_term_ref := f

   (************************************************************************
    * Free variables, substitution                                         *
    ************************************************************************)

   let rec free_vars_set t =
      match t.free_vars with
         Vars vars -> vars
       | VarsDelayed ->
            let vars =
               match t.core with
                  FOVar v -> SymbolSet.singleton v
                | Term t' ->
                     DEFINE body = bterms_free_vars t'.term_terms
                     IN
                     IFDEF VERBOSE_EXN THEN
                        if !debug_fv then
                           eprintf "Request for Term fvs: Term: %a%t" debug_print t eflush;
                        let res =
                          body
                        in
                           if !debug_fv then begin
                              eprintf "Result: ";
                              List.iter (eprintf "%s ") (List.map string_of_symbol (SymbolSet.to_list res));
                              eprintf "%t" eflush;
                           end; res
                     ELSE
                        body
                     ENDIF
                | Sequent seq ->
                     SymbolSet.union
                        (free_vars_set seq.sequent_args)
                        (hyp_fv
                           seq.sequent_hyps
                           (SeqHyp.length seq.sequent_hyps - 1)
                           (free_vars_set seq.sequent_concl))
                | Subst (t,sub) ->
                     DEFINE body =
                        SymbolSet.union
                           (List.fold_left
                              SymbolSet.remove (**)
                                 (free_vars_set t)
                                 (Lm_list_util.fst_split sub))
                             (subst_free_vars sub)
                     IN
                     IFDEF VERBOSE_EXN THEN
                        if !debug_fv then begin
                           eprintf "Request for Subst fvs: Term: %a; Subst: " debug_print t;
                           List.iter (fun (v,t) -> eprintf "(%a : %a) " output_symbol v debug_print t) sub;
                           eprintf "%t" eflush;
                        end;
                        let res =
                           body
                        in
                           if !debug_fv then begin
                              eprintf "Result: ";
                              List.iter (eprintf "%s ") (List.map string_of_symbol (SymbolSet.to_list res));
                              eprintf "%t" eflush;
                           end; res
                     ELSE
                        body
                     ENDIF
                | SOVar (v, vs, ts) ->
                     SymbolSet.add_list (terms_free_vars ts) vs
                | Hashed d ->
                     free_vars_set (Weak_memo.TheWeakMemo.retrieve_hack d)

            in t.free_vars <- Vars vars; vars

   and bterm_free_vars bt =
      SymbolSet.subtract_list (free_vars_set bt.bterm) bt.bvars

   and bterms_free_vars = function
      [] -> SymbolSet.empty
    | [bt] -> bterm_free_vars bt
    | bt::tl -> SymbolSet.union (bterms_free_vars tl) (bterm_free_vars bt)

   and terms_free_vars = function
      [] -> SymbolSet.empty
    | [t] -> free_vars_set t
    | t::tl -> SymbolSet.union (terms_free_vars tl) (free_vars_set t)

   and hyp_fv hyps i fvs =
      if i < 0 then fvs else
      hyp_fv hyps (pred i) (
      match SeqHyp.get hyps i with
         Hypothesis (v,t) ->
            SymbolSet.union (free_vars_set t) (SymbolSet.remove fvs v)
       | Context (v,conts,subterms) ->
            SymbolSet.add_list (SymbolSet.union (SymbolSet.remove fvs v) (terms_free_vars subterms)) conts)

   and subst_free_vars = function
      [] -> SymbolSet.empty
    | [(v,t)] -> free_vars_set t
    | (v,t)::tl -> SymbolSet.union (subst_free_vars tl) (free_vars_set t)

   let core_term core = { free_vars = VarsDelayed; core = core }

   let rec subst_remove v = function
      [] -> []
    | ((v',t) as h ::tl) as sub ->
         if (Lm_symbol.eq v v') then subst_remove v tl
         else let rem = subst_remove v tl in
            if rem == tl then sub else h::rem

   let do_term_subst sub t =
      IFDEF VERBOSE_EXN THEN
         if !debug_subst then
            begin
               debug_subst := false;
               eprintf "do_term_subst: { %a\n" debug_print t;
               eprintf "\tfree_vars:";
               List.iter (fun name -> eprintf " %s" name) (List.map string_of_symbol (SymbolSet.to_list (free_vars_set t)));
               eflush stderr;
               if sub == [] then eprintf "\t empty substitution\n" else
               List.iter (fun (v, t) -> eprintf "\t%a: %a\n" output_symbol v debug_print t) sub;
               eprintf "}%t" eflush;
               debug_subst := true
            end
      ENDIF;
      match SymbolSet.fst_mem_filt (free_vars_set t) sub with
         [] -> t
       | sub' -> core_term (Subst (t,sub'))

   (* Filter out any shadowed substs *)
   let rec filter_sub_vars bvars = function
      [] -> []
    | ((v,_)::t) when List.mem v bvars -> filter_sub_vars bvars t
    | [_] as l -> l
    | (h :: t) as l ->
         let t' = filter_sub_vars bvars t in
         if t' == t then l else h::t'

   (*
    * Make a variable.
    *)
   let mk_var_term v = core_term (FOVar v)

   (*
    * New variable production.
    * renames are the variables to be renamed,
    * and av is a list list of variables to avoid.
    *)
   let rec new_vars av = function
      [] -> ([],[])
    | v::vt ->
         let v' = new_name v (SymbolSet.mem av) in
         let (vs,ts) = (new_vars (SymbolSet.add av v') vt) in
            ((v,v')::vs, (v,mk_var_term v')::ts)

   let rec rename_bvars vs = function
      [] -> []
    | v::tl ->
         Lm_list_util.try_assoc v vs :: rename_bvars vs tl

   let do_bterm_subst sub bt =
      let btrm = bt.bterm in
      match bt.bvars with
         [] ->
            begin match SymbolSet.fst_mem_filt (free_vars_set btrm) sub with
               [] -> bt
             | sub ->
                  { bvars = []; bterm = core_term (Subst (btrm,sub))}
            end
       | bvrs ->
            begin match SymbolSet.fst_mem_filt (free_vars_set btrm)
                                               (filter_sub_vars bt.bvars sub) with
               [] -> bt
             | sub ->
                  let sub_fvars = subst_free_vars sub in
                  begin match SymbolSet.mem_filt sub_fvars bvrs with
                     [] ->
                        { bvars = bvrs;
                          bterm = core_term (Subst (btrm,sub))}
                   | capt_vars ->
                        let avoidvars = SymbolSet.union sub_fvars (free_vars_set btrm) in
                        let (vs,ts) = new_vars avoidvars capt_vars in
                        let new_t = do_term_subst ts btrm in
                        { bvars = rename_bvars vs bvrs;
                          bterm = core_term (Subst (new_t,sub)) }
                  end
            end

   (************************************************************************
    * De/Constructors                                                 *
    ************************************************************************)

   let fail_core s =
      raise (Invalid_argument ("Term_ds." ^ s ^ ": get_core returned a Subst or a Hashed"))

   let rec check_conts sub = function
      [] -> ()
    | c :: cts ->
         if List.mem_assoc c sub then
            (* Since substitutions are delayed, it's too late to raise RefineError *)
            raise(Invalid_argument("Term_base_ds.check_conts: substitution clashed with an SO context "^ (string_of_symbol c)));
         check_conts sub cts

   let rec get_core t =
      match t.core with
         Subst (tt,sub) ->
            let core =
               match get_core tt with
                  FOVar v ->
                     (* since sub was not eliminated, v should be in sub *)
                     let new_term = List.assoc v sub in
                        get_core (new_term)
                | SOVar (v, conts, ts) ->
                     check_conts sub conts;
                     SOVar(v, conts, List.map (do_term_subst sub) ts)
                | Term ttt ->
                     Term { term_op = ttt.term_op;
                            term_terms = List.map (do_bterm_subst sub) ttt.term_terms }
                | Sequent { sequent_args = args;
                            sequent_hyps = hyps;
                            sequent_concl = concl } ->
                     let sub_vars = subst_free_vars sub in
                     let all_vars = SymbolSet.union sub_vars (free_vars_set tt) in
                     (* XXX: n8gray: I'd like to be able to fold directly here but there's no Seq_set.fold_left *)
                     let sub', hyp_list = hyps_subst hyps (SeqHyp.length hyps) sub all_vars sub_vars [] 0 in
                        Sequent {
                           sequent_args = do_term_subst sub args;
                           sequent_hyps = SeqHyp.of_list (List.rev hyp_list);
                           sequent_concl = do_term_subst sub' concl;
                        }
                | Subst _ | Hashed _ -> fail_core "get_core"
            in
               t.core <- core;
               core
       | Hashed d ->
            (* Preserve the hash as long as possible *)
            get_core (Weak_memo.TheWeakMemo.retrieve_hack d)

       | core ->
            core

   and hyps_subst hyps len sub all_vars sub_vars new_hyps i =
      if i = len then sub, new_hyps else
      match SeqHyp.get hyps i with
         Hypothesis (v,t) as hyp ->
            let t' = do_term_subst sub t in
            let sub = subst_remove v sub in
            (* Rename v if it might capture a free var in the subst *)
            if SymbolSet.mem sub_vars v then
               let v' = new_name v (SymbolSet.mem all_vars) in
               let sub = (v, mk_var_term v') :: sub in
               (* XXX: These would not be needed if new_name was guaranteed unique *)
               let sub_vars = SymbolSet.add sub_vars v' in
               let all_vars = SymbolSet.add all_vars v' in
                  hyps_subst hyps len sub all_vars sub_vars (Hypothesis (v', t) :: new_hyps) (i + 1)
            else
               let hyp = if t == t' then hyp else Hypothesis (v, t') in
                  hyps_subst hyps len sub all_vars sub_vars (hyp :: new_hyps) (i + 1)
       | Context (v,conts,ts) as hyp ->
            if SymbolSet.mem sub_vars v then
               raise(Invalid_argument "Term_base_ds.get_core: free variable got captured by a context");
            let ts' = Lm_list_util.smap (do_term_subst sub) ts in
            let sub = subst_remove v sub in
            let hyp = if ts == ts' then hyp else Context (v, conts, ts) in
               hyps_subst hyps len sub all_vars sub_vars (hyp :: new_hyps) (i + 1)

   let mk_op name params = { op_name = name; op_params = params }

   let ops_eq op1 op2 = Opname.eq op1.op_name op2.op_name && op1.op_params = op2.op_params

   let is_simple_bterm bt = (bt.bvars = [])
   let no_bvars = List.for_all is_simple_bterm
   let mk_simple_bterm term = { bvars = []; bterm = term }
   let make_bterm x = x (* external make_bterm : bound_term -> bound_term = "%identity" *)
   let mk_bterm vars term = { bvars = vars; bterm = term }

   (************************************************************************
    * Variables                                                            *
    ************************************************************************)

   (*
    * See if a term is a variable.
    *)

   let rec is_var_term t =
      match t.core with
         FOVar _ -> true
       | Subst _ -> let _ = get_core t in is_var_term t
       | _ -> false

   (*
    * Destructor for a variable.
    *)
   let rec dest_var t =
      match t.core with
         FOVar v -> v
       | Subst _ -> let _ = get_core t in dest_var t
       | _ -> REF_RAISE(RefineError ("dest_var", TermMatchError (t, "not a var")))

   (* xlists *)
   let xnil_opname = Opname.xnil_opname
   let xcons_opname = Opname.xcons_opname

   let xnil_term =
      core_term (Term { term_op = { op_name = xnil_opname; op_params = [] }; term_terms = [] })

   let xconcl_term =
      core_term (Term { term_op = { op_name = xconcl_opname; op_params = [] }; term_terms = [] })

   let rec is_xlist_term t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = []; bterm = _ }; { bvars = []; bterm = b }]
         } -> Opname.eq opname xcons_opname && is_xlist_term b
       | Term { term_op = { op_name = opname; op_params = [] }; term_terms = [] } -> Opname.eq opname xnil_opname
       | _ ->
            false

   let rec dest_xlist t =
      match get_core t with
         Term { term_op = { op_name = opname; op_params = [] };
                term_terms = [{ bvars = []; bterm = a };
                              { bvars = []; bterm = b }]
              } when Opname.eq opname xcons_opname ->
            a::(dest_xlist b)
       | Term { term_op = { op_name = opname; op_params = [] }; term_terms = [] } when Opname.eq opname xnil_opname ->
            []
       | _ ->
            REF_RAISE(RefineError ("dest_xlist", TermMatchError (t, "not a list")))

   let rec mk_xlist_term =
      let cons_op = { op_name = xcons_opname; op_params = [] } in function
         h::t ->
            core_term (Term { term_op = cons_op; term_terms = [mk_simple_bterm h; mk_simple_bterm (mk_xlist_term t)] })
    | [] ->
         xnil_term

   let rec dest_term t =
      match t.core with
         Term t -> t
       | Sequent _ ->
            raise (Invalid_argument "Term_base_ds.dest_term: dest_term called on a sequent")
       | SOVar (v, conts, terms) -> {
            term_op = { op_name = var_opname; op_params = [Var v] };
            term_terms = List.map mk_simple_bterm (terms @ [mk_xlist_term (List.map mk_var_term conts)]) }
       | FOVar v ->
             { term_op = { op_name = var_opname; op_params = [Var v] }; term_terms = [] }
       | Subst _ -> let _ = get_core t in dest_term t
       | Hashed d -> dest_term (Weak_memo.TheWeakMemo.retrieve_hack d)

   let dest_bterm x = x (* external dest_bterm : bound_term -> bound_term = "%identity" *)

   let mk_sequent_term seq = core_term (Sequent seq)

   let dest_simple_bterm = function
      { bvars = []; bterm = tt } -> tt
    | _ -> REF_RAISE(RefineError ("dest_simple_bterm", StringError "bvars exist"))

   let make_term t =
      if Opname.eq t.term_op.op_name var_opname then
         match t.term_op.op_params, t.term_terms with
            [Var v], [] -> core_term (FOVar v)
          | [Var v], bterms ->
               begin try
                  let bterms, conts = Lm_list_util.split_last bterms in
                     core_term (SOVar (v,
                                       List.map dest_var (dest_xlist (dest_simple_bterm conts)),
                                       List.map dest_simple_bterm bterms))
               with RefineError _ | Failure _ ->
                  raise(Invalid_argument "Term.base_ds.make_term with var_opname and subterms, but the term does not look like a SO variable")
               end
          | _ -> raise(Invalid_argument "Term.base_ds.make_term with var_opname, but the term does not look like a variable")
      else core_term (Term t)

   let mk_term op bterms =
      make_term { term_op = op; term_terms = bterms }

   let mk_level_var v i =
      { le_var = v; le_offset = i }

   let mk_level i l =
      { le_const = i; le_vars = l }

   let subterms_of_term t =
      List.map (fun bt -> bt.bterm) (dest_term t).term_terms

   let subterm_arities_aux bterm = List.length bterm.bvars

   let subterm_arities term =
         List.map subterm_arities_aux (dest_term term).term_terms
   (*
    * Operator names.
    *)
   let rec opname_of_term t = match t.core with
      Term t -> t.term_op.op_name
    | Sequent _ -> sequent_opname
    | FOVar _ | SOVar _ -> var_opname
    | Subst _ -> let _ = get_core t in opname_of_term t
    | Hashed d -> opname_of_term (Weak_memo.TheWeakMemo.retrieve_hack d)

   (* These are trivial identity functions *)

   let make_op x = x (* external make_op : operator' -> operator = "%identity" *)
   let dest_op x = x (* external dest_op : operator -> operator' = "%identity" *)
   let make_param x = x (* external make_param : param' -> param = "%identity" *)
   let dest_param x = x (* external dest_param : param -> param' = "%identity" *)
   let dest_params x = x (* external dest_params : param list -> param' list = "%identity" *)
   let make_level x = x (* external make_level : level_exp' -> level_exp = "%identity" *)
   let dest_level x = x (* external dest_level : level_exp -> level_exp' = "%identity" *)
   let make_level_var x = x (* external make_level_var : level_exp_var' -> level_exp_var = "%identity" *)
   let dest_level_var x = x (* external dest_level_var : level_exp_var -> level_exp_var' = "%identity" *)
   let make_object_id x = x (* external make_object_id : param list -> object_id = "%identity" *)
   let dest_object_id x = x (* external dest_object_id : object_id -> param list = "%identity" *)

   (*
    * Descriptor operations.
    *)
   let mk_descriptor_term d = core_term (Hashed d)

   let dest_descriptor t =
      match t.core with
         Hashed d ->
            Some d
       | _ ->
            None

   (************************************************************************
    * Tools for "simple" terms                                             *
    ************************************************************************)

   (*
    * "Simple" terms have no parameters and no binding variables.
    *)
   let is_simple_term_opname name t = match get_core t with
      Term { term_op = { op_name = name'; op_params = [] };
             term_terms = bterms
           } when Opname.eq name' name -> no_bvars bterms
    | _ -> false

   let mk_any_term op terms = mk_term op (List.map mk_simple_bterm terms)

   let mk_simple_term name terms =
      mk_any_term { op_name = name; op_params = [] } terms

   let dest_simple_term t = match dest_term t with
      { term_op = { op_name = name; op_params = [] };
         term_terms = bterms } ->
            name, List.map dest_simple_bterm bterms
    | _ -> REF_RAISE(RefineError ("dest_simple_term", TermMatchError (t, "params exist")))

   let dest_simple_term_opname name t = match dest_term t with
      { term_op = { op_name = name'; op_params = [] };
         term_terms = bterms } ->
         if Opname.eq name name' then List.map dest_simple_bterm bterms
         else
            REF_RAISE(RefineError ("dest_simple_term_opname", TermMatchError (t, "opname mismatch")))
    | _ -> REF_RAISE(RefineError ("dest_simple_term_opname", TermMatchError (t, "params exist")))
end
