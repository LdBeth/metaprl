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

open Lm_debug
open Lm_symbol

open Refiner.Refiner.TermType
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError
open Term_match_table
open Unify_mm

open Tactic_type.Tacticals
open Tactic_type.Sequent

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Simp_typeinf%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Some generic renaming of type variables.
 *)
type ty_var = var
type ty_var_set = SymbolSet.t
type tenv = ty_var_set
type venv = term SymbolTable.t

(*
 * A type inference is performed in a type context,
 * which maps variables to type.
 *
 * An inference function takes as arguments :
 * 1) consts - a set of variables that should be treated as
 * constants when we use unification to figure things out.
 * 2) tenv - set of all bound type variables
 * 3) venv - a table of variable names and
 * the types these variables were declared with.
 * 4) eqs - a list of equations we have on our type variables
 * 5) t - a term whoose type we want to infer
 *
 * An inference function returns:
 * 1) A new term constructed by the type inference function;
 * this term can be arbitrary, but it is often a new term
 * that represents the input term with type annotations added.
 * 2) Updated eqs,
 * 3) a type for the term (that can contain new type variables)
 *)
type simp_typeinf_func = ty_var_set -> tenv -> venv -> eqnlist -> term -> term * eqnlist * term

(*
 * Modular components also get a recursive instance of
 * the inference algorithm.
 *)
type simp_typeinf_comp = simp_typeinf_func -> simp_typeinf_func

(*
 * This is the resource addition.
 *)
type simp_typeinf_resource_info = term * simp_typeinf_comp

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Infer the type of a term from the table.
 *)
let infer tbl =
   let rec infer_term consts tenv venv eqs t =
      let inf =
         try lookup tbl select_all t with
            Not_found ->
               raise (RefineError ("simp_typeinf", StringTermError ("Don't know how to infer type for", t)))
      in
         inf infer_term consts tenv venv eqs t
   in
      infer_term

(*
 * The resource itself.
 *)
let resource (simp_typeinf_resource_info, simp_typeinf_func) simp_typeinf =
   table_resource_info infer

let typeinf_final consts eqs t ty =
   let subst = unify_eqnl eqs consts in
      apply_subst subst t, apply_subst subst ty

let simp_infer_type p t =
   let consts = free_vars_set t in
   let inf = get_resource_arg p get_simp_typeinf_resource in
   let t, eqs, ty = inf consts SymbolSet.empty SymbolTable.empty eqnlist_empty t in
      typeinf_final consts eqs t ty

let simp_infer_type_args p t =
   let t =
      try get_with_arg p with
         RefineError _ ->
            snd (simp_infer_type p t)
   in
      [t]

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
