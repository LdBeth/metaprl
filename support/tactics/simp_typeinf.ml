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

open Printf
open Lm_symbol
open Lm_debug

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermAddr
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Term_match_table
open Mp_resource
open Unify_mm

open Tactic_boot_sig

open Tactic_type
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

(*
 * String table.
 *)
module TyVarBase =
struct
   type elt = ty_var
   type data = term

   let print _ _ =
      ()

   let compare = Lm_symbol.compare

   let append = (@)
end

module TyVarSet = SymbolSet
module TyEnv = Red_black_table.MakeTable (TyVarBase)

(*
 * A type inference is performed in a type context,
 * which maps variables to type.
 *
 * An inference function takes as arguments :
 * 1) consts - a set of variables that should be treated as
 * constants when we use unification to figure things out.
 * 2) env - a table of variable names and
 * the types these variables were declared with.
 * 3) eqs - a list of equations we have on our type variables
 * 4) t - a term whoose type we want to infer
 *
 * An inference function returns:
 * 1) A new term constructed by the type inference function;
 * this term can be arbitrary, but it is often a new term
 * that represents the input term with type annotations added.
 * 2) Updated eqs,
 * 3) a type for the term (that can contain new type variables)
 *)
type simp_typeinf_func = TyVarSet.t -> TyEnv.t -> eqnlist -> term -> term * eqnlist * term

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
let identity x = x

let infer tbl =
   let rec infer_term consts tenv eqs t =
      let inf =
         try snd (lookup tbl t) with
            Not_found ->
               raise (Invalid_argument "Simp_typeinf.infer: missing a default case")
      in
         inf infer_term consts tenv eqs t
   in
      infer_term

(*
 * The resource itself.
 *)
let resource simp_typeinf =
   table_resource_info identity infer

let typeinf_final consts eqs t ty =
   let subst = unify_eqnl eqs consts in
      apply_subst t subst, apply_subst ty subst

let simp_infer_type p t =
   let consts = free_vars_set t in
   let inf = get_resource_arg p get_simp_typeinf_resource in
      try
         let t, eqs, ty = inf consts TyEnv.empty eqnlist_empty t in
            typeinf_final consts eqs t ty
      with
         RefineError _ ->
            raise (RefineError ("infer_type", StringTermError ("Type inference failed", t)))

let simp_infer_type_args p t =
   let t =
      try get_with_arg p with
         RefineError _ ->
            snd (simp_infer_type p t)
   in
      [t]

(*
 * Generating new symbols.
 *)
let gensym () =
   new_symbol_string "$ty"

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
