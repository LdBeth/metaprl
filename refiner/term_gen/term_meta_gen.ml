(*
 * Meta terms include implications, etc.
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

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_meta_sig
open Term_subst_sig

module TermMeta (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term = TermType.term)
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type meta_term = TermType.meta_term) =
struct
   open TermType
   open Term
   open RefineError

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type term = TermType.term
   type meta_term = TermType.meta_term

   (************************************************************************
    * META-TERMS                                                           *
    ************************************************************************)

   (*
    * Unzip a metaimplication into a list of terms.
    *)
   let rec unzip_mimplies = function
      MetaTheorem t ->
         [t]
    | MetaImplies (MetaTheorem a, t) ->
         a :: unzip_mimplies t
    | t -> ref_raise(RefineError ("unzip_mimplies", MetaTermMatchError t))

   let rec zip_mimplies = function
      [h] -> MetaTheorem h
    | h::t -> MetaImplies (MetaTheorem h, zip_mimplies t)
    | [] -> ref_raise(Invalid_argument "zip_mimplies")

   (*
    * Implication with bindings.
    *)
   let rec strip_mfunction = function
      MetaTheorem t ->
         MetaTheorem t
    | MetaImplies (a, t) ->
         MetaImplies (a, strip_mfunction t)
    | MetaFunction (v, a, t) ->
         MetaImplies (a, strip_mfunction t)
    | MetaIff (t1, t2) ->
         MetaIff (strip_mfunction t1, strip_mfunction t2)

   let unzip_mfunction t =
      let rec collect l = function
         MetaTheorem t ->
            List.rev l, t
       | MetaImplies (MetaTheorem a, t) ->
            collect ((None, a) :: l) t
       | MetaFunction (v, MetaTheorem a, t) ->
            collect ((Some v, a) :: l) t
       | t ->
            ref_raise(RefineError ("unzip_mfunction", MetaTermMatchError t))
      in
         collect [] t

   let zip_mfunction args goal =
      let rec collect = function
         (Some v, a) :: t ->
            MetaFunction (v, MetaTheorem a, collect t)
       | (None, a) :: t ->
            MetaImplies (MetaTheorem a, collect t)
       | [] ->
            MetaTheorem goal
      in
         collect args

   (*
    * Unzip a rewrite term.
    *)
   let rec unzip_mrewrite = function
      MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
         [], redex, contractum
    | MetaImplies(MetaTheorem a, t) ->
         let l, redex, contractum = unzip_mrewrite t in
            a::l, redex, contractum
    | t -> ref_raise(RefineError ("unzip_mrewrite", MetaTermMatchError t))

   (*
    * Calculate context vars.
    *)
   let rec binding_vars = function
      MetaTheorem t ->
         TermSubst.binding_vars t
    | MetaImplies (a, b) ->
         List_util.union (binding_vars a) (binding_vars b)
    | MetaFunction (v, a, b) ->
         List_util.union (binding_vars a) (binding_vars b)
    | MetaIff (a, b) ->
         List_util.union (binding_vars a) (binding_vars b)

   let rec context_vars = function
      MetaTheorem t ->
         TermSubst.context_vars t
    | MetaImplies (a, b) ->
         List_util.union (context_vars a) (context_vars b)
    | MetaFunction (v, a, b) ->
         List_util.union (context_vars a) (context_vars b)
    | MetaIff (a, b) ->
         List_util.union (context_vars a) (context_vars b)

   (*
    * Induction forms.
    *)
   let meta_for_all f =
      let rec aux = function
         MetaTheorem t -> f t
       | MetaImplies (a, b) ->
            aux a & aux b
       | MetaFunction (v, a, b) ->
            aux a & aux b
       | MetaIff (a, b) ->
            aux a & aux b
      in
         aux

   let meta_for_all2 f t1 t2 =
      let rec aux = function
         MetaTheorem a1, MetaTheorem a2 ->
            f a1 a2
       | MetaImplies (a1, b1), MetaImplies (a2, b2) ->
            aux (a1, a2) & aux (b1, b2)
       | MetaFunction (_, a1, b1), MetaFunction (_, a2, b2) ->
            aux (a1, a2) & aux (b1, b2)
       | MetaIff (a1, b1), MetaIff (a2, b2) ->
            aux (a1, a2) & aux (b1, b2)
       | _ ->
            ref_raise(Failure "meta_for_all2")
      in
         aux (t1, t2)

   (*
    * Alpha equality.
    *)
   let meta_alpha_equal t1 t2 =
      try meta_for_all2 TermSubst.alpha_equal t1 t2 with
         Failure "meta_for_all2" ->
            false
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
