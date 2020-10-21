(*
 * Hash terms.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol

open Term_sig
open Term_ty_sig

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan

(*
 * Include a hash.
 *)
let hash_int code i =
   (code lsl 2) lxor (code lsr 2) lxor i

let hash_item code item =
   hash_int code (Hashtbl.hash_param max_int max_int item)

let hash_item_list code items =
   List.fold_left hash_item code items

(*
 * Variable environment maps variables to DeBruijn numbers.
 *)
type venv =
   { venv_index : int;
     venv_table : int SymbolTable.t
   }

let venv_empty =
   { venv_index  = 0;
     venv_table  = SymbolTable.empty
   }

let venv_add_var venv code v =
   let code = hash_int code 0x57c81be3 in
   let { venv_index = index; venv_table = table } = venv in
   let venv = { venv_index = succ index; venv_table = SymbolTable.add table v index } in
      venv, code

let venv_add_vars venv code vars =
   List.fold_left (fun (venv, code) v ->
         venv_add_var venv code v) (venv, code) vars

let venv_find_var venv v =
   SymbolTable.find venv.venv_table v

(*
 * Variables are translated to indexes.
 *)
let hash_var venv code v =
   try hash_int code (venv_find_var venv v) with
      Not_found ->
         hash_item code v

(* unused
let hash_var_list venv code vl =
   List.fold_left (hash_var venv) code vl
*)

(*
 * Hash a term.  We try to be careful here--the
 * hash should be functional with respect to alpha-equality.
 *)
let rec hash_term venv code e =
   if is_var_term e then
      hash_var_term venv code e
   else if is_so_var_term e then
      hash_so_var_term venv code e
   else if is_context_term e then
      hash_context_term venv code e
   else if is_sequent_term e then
      hash_sequent_term venv code e
   else
      hash_normal_term venv code e

and hash_term_list venv code el =
   List.fold_left (hash_term venv) code el

and hash_var_term venv code e =
   hash_var venv code (dest_var e)

and hash_so_var_term venv code e =
   let v, cvars, args = dest_so_var e in
   let code = hash_item code v in
   let code = hash_item_list code cvars in
      hash_term_list venv code args

and hash_context_term venv code e =
   let v, e, cvars, args = dest_context e in
   let code = hash_item code v in
   let code = hash_term venv code e in
   let code = hash_item_list code cvars in
      hash_term_list venv code args

and hash_sequent_term venv code e =
   let { sequent_args = arg;
         sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent e
   in
   let code = hash_term venv code arg in
   let venv, code =
      SeqHyp.fold (fun (venv, code) _ hyp ->
            match hyp with
               Hypothesis (v, e) ->
                  let code = hash_term venv code e in
                  let venv, code = venv_add_var venv code v in
                     venv, code
             | Context (v, cvars, args) ->
                  let code = hash_item code v in
                  let code = hash_item_list code cvars in
                  let code = hash_term_list venv code args in
                     venv, code) (venv, code) hyps
   in
      hash_term venv code concl

and hash_normal_term venv code e =
   let { term_op = op; term_terms = bterms } = dest_term e in

   (*
    * Since Lm.num has adopted hash safe Zarith, this should work fine.
    *)
   let code = hash_item code op in
      List.fold_left (hash_bterm venv) code bterms

and hash_bterm venv code e =
   let { bvars = bvars; bterm = e } = dest_bterm e in
   let venv, code = venv_add_vars venv code bvars in
      hash_term venv code e

(*
 * Types.
 *)
let hash_ty_param venv code p =
   match p with
      TyNumber ->
         hash_int code 0x3fc43893
    | TyString ->
         hash_int code 0x254bcc62
    | TyToken e ->
         hash_term venv (hash_int code 0x34dd6810) e
    | TyLevel ->
         hash_int code 0x158cabc4
    | TyVar ->
         hash_int code 0x0902bb25
    | TyShape ->
         hash_int code 0x211b22ae
    | TyOperator ->
         hash_int code 0x3276cb0c
    | TyQuote ->
         hash_int code 0x1c1b8520

let hash_ty_param_list venv code pl =
   List.fold_left (hash_ty_param venv) code pl

let hash_ty_bterm venv code bterm =
   let { ty_bvars = bvars; ty_bterm = e } = bterm in
      hash_term venv (hash_term_list venv code bvars) e

let hash_ty_bterm_list venv code bterms =
   List.fold_left (hash_ty_bterm venv) code bterms

let hash_ty venv code ty =
   let { ty_term   = term;
         ty_opname = opname;
         ty_params = params;
         ty_bterms = bterms;
         ty_type   = ty
       } = ty
   in
   let code = hash_term venv code term in
   let code = hash_item code opname in
   let code = hash_ty_param_list venv code params in
   let code = hash_ty_bterm_list venv code bterms in
      hash_term venv code ty

(************************************************************************
 * External functions.
 *)
let hash_term e =
   hash_term venv_empty 0 e

let hash_ty ty =
   hash_ty venv_empty 0 ty

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
