(*
 * The term classes define a very simple type system.
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
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * Author: Aleksey Nogin <nogin@cs.cornell.edu>
 * Modified By: Jason Hickey <jyh@cs.cornell.edu>
 *)
open Lm_printf
open Lm_symbol

open Opname
open Term_sig
open Term_base_sig
open Term_man_sig
open Term_shape_sig
open Term_subst_sig
open Term_ty_sig

module TermTy (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType) =
struct
   open TermType
   open Term
   open TermMan
   open TermSubst

   type term = TermType.term

   type ty_param = term poly_ty_param
   type ty_bterm = term poly_ty_bterm
   type ty_term  = (term, term) poly_ty_term

   (************************************************************************
    * Implementation.
    *)

   (*
    * Printing.
    *)
   let string_of_ty_param param =
      match param with
         TyNumber  -> "TyNumber"
       | TyString  -> "TyString"
       | TyToken t -> "TyToken " ^ string_of_opname (opname_of_term t)
       | TyShape -> "TyShape"
       | TyVar     -> "TyVar"
       | TyLevel   -> "TyLevel"
       | TyQuote   -> "TyQuote"

   (*
    * Compute a canonical term from the class.
    *)
   let term_of_ty ty_term =
      ty_term.ty_term

   (*
    * Equality.
    *)
   let rec equal_lists pred l1 l2 =
      match l1, l2 with
         h1 :: t1, h2 :: t2 ->
            pred h1 h2 && equal_lists pred t1 t2
       | [], [] ->
            true
       | _ ->
            false

   let eq_param param1 param2 =
      match param1, param2 with
         TyNumber, TyNumber
       | TyString, TyString
       | TyLevel, TyLevel
       | TyVar, TyVar
       | TyShape, TyShape
       | TyQuote, TyQuote ->
            true
       | TyToken t1, TyToken t2 ->
            alpha_equal t1 t2
       | _ ->
            false

   let eq_bterm bterm1 bterm2 =
      let { ty_bvars = bvars1; ty_bterm = term1 } = bterm1 in
      let { ty_bvars = bvars2; ty_bterm = term2 } = bterm2 in
         equal_lists alpha_equal bvars1 bvars2 && alpha_equal term1 term2

   let eq_ty ty_term1 ty_term2 =
      let { ty_opname = opname1;
            ty_params = params1;
            ty_bterms = bterms1;
            ty_type   = ty1
          } = ty_term1
      in
      let { ty_opname = opname2;
            ty_params = params2;
            ty_bterms = bterms2;
            ty_type   = ty2
          } = ty_term2
      in
         (Opname.eq opname1 opname2)
         && (equal_lists eq_param params1 params2)
         && (equal_lists eq_bterm bterms1 bterms2)
         && (alpha_equal ty1 ty2)

   let eq ty_term1 ty_term2 =
      let { ty_term = term1 } = ty_term1 in
      let { ty_term = term2 } = ty_term2 in
         alpha_equal term1 term2 && eq_ty ty_term1 ty_term2
end
