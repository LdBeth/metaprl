(*
 * Alexey's ``special'' terms to be used in reduction rules
 *
 * "canon_var" plays the same role as "var" in reduction rules but
 * the correspondig subterm should be evaluated before the reduction
 * ("call by value" instead of "call by name")
 *
 * subst (v1,v2,v3,...,vm.T;t1;t2;t3;...;tn)
 * it is an error if m!=n
 * if n=m then subst(...) is T with v1 substituted to t2, v2 - to t2, etc.
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
 * Author: Alexey Nogin
 *)

#include "refine_error.h"

open Opname
open Refine_error_sig
open Term_std_sig
open Term_std

module TermEval
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

   type term = Term.term

   (*
    * Manifest terms are injected into the "perv" module.
    *)
   let xperv = make_opname ["Perv"]

   let canon_var_opname = mk_opname "canon_var" xperv

   (*
    * See if a term is a "canon_var".
    *)
   let is_canon_var_term = function
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = []
      } when Opname.eq opname canon_var_opname -> true
    | _ -> false

   (*
    * Destructor for a "canon_var".
    *)
   let dest_canon_var = function
      { term_op = { op_name = opname; op_params = [Var v] };
        term_terms = []
      } when Opname.eq opname canon_var_opname -> v
     | t -> ref_raise(RefineError ("dest_canon_var", TermMatchError (t, "bad arity")))

   (*
    * Make a "canon_var".
    *)
   let mk_canon_var_term v =
      { term_op = { op_name = canon_var_opname; op_params = [Var v] };
        term_terms = []
      }

   let subst_opname = mk_opname "subst" xperv

   (*
    * See if a term is a "subst" term.
    *)

   let rec is_subst_term_args = function
      [],[] -> true
    | var::vars,{bvars = []}::bterms -> is_subst_term_args (vars,bterms)
    | _ -> false

   let is_subst_term = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = {bvars = vars; bterm = main_term}::bterms
      } when Opname.eq opname subst_opname -> is_subst_term_args (vars,bterms)
    | _ -> false

   (*
    * Destructor for a "subst" term.
    *)

   let rec dest_subst_args t = function
      [],[] -> [],[]
    | var::vars,{bvars = []; bterm = term}::bterms ->
         let tail = dest_subst_args t (vars,bterms) in
         (var::fst tail),(term::snd tail)
    | _ -> ref_raise(RefineError ("dest_subst_args", TermMatchError (t, "bad arity")))

   let dest_subst = function
      { term_op = { op_name = opname; op_params = [] };
        term_terms = {bvars = vars; bterm = main_term}::bterms
      } as t when Opname.eq opname subst_opname -> (main_term, dest_subst_args t (vars,bterms))
     | t -> ref_raise(RefineError ("dest_subst", TermMatchError (t, "bad arity")))

   (*
    * Make a "subst" term.
    *)

   let mk_subst_term main_term subst =
      let sub = List.split subst in
      let vars = fst sub in
      let terms = List.map (function term -> {bvars=[]; bterm = term}) (snd sub) in
      { term_op = { op_name = subst_opname; op_params = [] };
        term_terms = {bvars=vars; bterm=main_term}::terms
      }

   let make_subst_term main_term vars trms =
      if List.length vars != List.length trms then
         ref_raise(Invalid_argument "make_subst_term")
      else
         let terms = List.map (function term -> {bvars=[]; bterm = term}) trms in
            { term_op = { op_name = subst_opname; op_params = [] };
              term_terms = {bvars=vars; bterm=main_term}::terms
            }

   let make_1subst_term main_term v t =
      { term_op = { op_name = subst_opname; op_params = [] };
        term_terms = [ {bvars=[v]; bterm=main_term};{bvars=[]; bterm=t} ]
      }

   let make_2subst_term main_term v1 v2 t1 t2 =
      { term_op = { op_name = subst_opname; op_params = [] };
        term_terms = [ {bvars=[v1;v2]; bterm=main_term};{bvars=[]; bterm=t1};{bvars=[]; bterm=t2} ]
      }
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
