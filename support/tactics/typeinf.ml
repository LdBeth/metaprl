(*
 * @begin[doc]
 * @module[Typeinf]
 *
 * This module implements a simple type inference algorithm based
 * on Hindley-Milner type inference~@cite[DM82].  This is
 * a @emph{generic} resource definition that can be used to implement
 * type inference in various logics.
 *
 * @docoff
 * @end[doc]
 *
 * jyh: I don't know how this works, so I am at a loss to document it...
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * @email{jyh@cs.caltech.edu}
 *
 * @end[license]
 *)
open Lm_symbol
open Lm_debug

open Term_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Term_match_table
open Unify_mm

open Tactic_type
open Tactic_type.Tacticals
open Tactic_type.Sequent

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Typeinf%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A function that analyzes the sequent to gather type info.
 * It gets a clause from the current sequent or its assumptions.
 *)
type typeinf_subst_fun = term_subst -> (var option * term) -> term_subst

(*
 * A type inference is performed in a type context,
 * which maps variables to type.
 *)
type opt_eqs_type = (term * term) list
type typeinf_func = SymbolSet.t -> term_subst -> eqnlist -> opt_eqs_type -> term_subst -> term -> eqnlist * opt_eqs_type * term_subst * term

(*
 * This resource is used to analyze the sequent to gather type info.
 * The subst_fun gets a clause from the current sequent or its
 * assumptions.
 *)
type typeinf_subst_info = term * typeinf_subst_fun

(*
 * Modular components also get a recursive instance of
 * the inference algorithm.
 *)
type typeinf_comp = typeinf_func -> typeinf_func

(*
 * This is the resource addition.
 *)
type typeinf_resource_info = term * typeinf_comp

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Infer the type of a term from the table.
 *)
let identity x = x

let collect tbl subst (so, t) =
   try (snd (lookup tbl t)) subst (so, t) with
      Not_found ->
         raise (RefineError ("Typeinf.collect", StringTermError ("can't collect type for", t)))

(*
 * Resource.
 *)
let resource typeinf_subst =
   table_resource_info identity collect

(*
 * Projector.
 *)
let collect_decls p =
   let collect = get_resource_arg p get_typeinf_subst_resource in
   let rec filter_hyps subst hyps i len =
      if i = len then
         subst
      else
         match SeqHyp.get hyps i with
            Hypothesis (v, t) ->
               let subst = (v, t) :: subst in
               let subst =
                  try collect subst (Some v, t) with
                     RefineError _ ->
                        subst
               in
                  filter_hyps subst hyps (i + 1) len
          | _ ->
               filter_hyps subst hyps (i + 1) len
   in
   let goal, assums = dest_msequent (Sequent.msequent p) in
   let { sequent_hyps = hyps } = TermMan.explode_sequent goal in
   let num_hyps = TermMan.num_hyps goal in
   let rec filter_assums subst = function
      assum :: tl ->
         let num_hyps' = TermMan.num_hyps assum in
            if num_hyps' <= num_hyps then
               let concl = TermMan.nth_concl assum 1 in
               let subst =
                  try collect subst (None, concl) with
                     RefineError _ ->
                        subst
               in
                  filter_assums subst tl
            else
               subst
    | [] ->
         subst
   in
   let subst = filter_hyps [] hyps 0 (SeqHyp.length hyps) in
      filter_assums subst assums

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Infer the type of a term from the table.
 *)
let infer tbl =
   let rec aux consts decls eqs opt_eqs defs t =
      if is_fso_var_term t then
         let v = dest_fso_var t in
            try eqs, opt_eqs, defs, List.assoc v decls with
               Not_found ->
                  raise (RefineError ("typeinf", StringVarError ("Undeclared variable", v)))
      else
         let inf =
            try snd (lookup tbl t) with
               Not_found ->
                  raise (RefineError ("typeinf", StringTermError ("Don't know how to infer type for", t)))
         in
            inf aux consts decls eqs opt_eqs defs t
   in
      aux

(*
 * The resource itself.
 *)
let resource typeinf =
   table_resource_info identity infer

(*
 * Projector.
 *)
let rec collect_consts = function
   [] -> SymbolSet.empty
 | [v,t] -> SymbolSet.add (free_vars_set t) v
 | (v,t)::tl ->
      SymbolSet.add (SymbolSet.union (free_vars_set t) (collect_consts tl)) v

let rec try_append_eqs eqs consts = function
   [] -> eqs, []
 | ((t1,t2)::opt_eqs) as o_eqs ->
      begin try
         let eqs', opt_eqs' =
            try_append_eqs (unify_eqnl_eqnl (eqnlist_append_eqn eqs t1 t2) consts)consts opt_eqs
         in
            eqs', (if opt_eqs'==opt_eqs then o_eqs else (t1,t2)::opt_eqs')
      with RefineError _ ->
         try_append_eqs eqs consts opt_eqs
      end

let typeinf_final consts eqs opt_eqs defs t =
   let eqs = unify_eqnl_eqnl eqs consts in
   let all_eqs,opt_eqs = try_append_eqs eqs consts opt_eqs in
   let subst = unify_eqnl all_eqs consts in
   eqs, opt_eqs, subst, apply_subst defs (apply_subst subst t)

let infer_type p t =
   let decls = collect_decls p in
   let consts = SymbolSet.union (collect_consts decls) (free_vars_set t) in
   let inf = get_resource_arg p get_typeinf_resource in
   try
      let eqs,opt_eqs,defs,t = inf consts decls eqnlist_empty [] [] t in
      let _,_,_,t = typeinf_final consts eqs opt_eqs defs t
      in
         t
   with
      RefineError _ ->
         raise (RefineError ("infer_type", StringTermError("Type inference failed", t)))

let infer_type_args p t =
   let t =
      try get_with_arg p with
         RefineError _ ->
            infer_type p t
   in
      [t]

let infer_type_2args p t =
   match get_with_args p with
      [_] as l ->
         infer_type p t :: l
    | l ->
         l

let vnewname consts defs v =
   new_name v (fun v -> SymbolSet.mem consts v || List.mem_assoc v defs)

let infer_const t _ _ _ eqs opt_eqs defs _ = eqs, opt_eqs, defs, t
let infer_map f inf consts decls eqs opt_eqs defs t =
   inf consts decls eqs opt_eqs defs (f t)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
