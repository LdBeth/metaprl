doc <:doc<
   @begin[doc]
   @module[Var]

   The @tt[Var] module provides utilities to
   generate new variables that are guaranteed to be distinct
   from all other bound variables in a proof goal.

   There are three basic functions implemented here.
   @begin[verbatim]
   val new_var         : string -> string list -> string
   val maybe_new_var   : string -> string list -> string
   val maybe_new_vars  : string list -> string list -> string list
   @end[verbatim]

   The function $@tt[new_var]@space v@space @i[vars]$ generates a new variable
   ``similar'' to $v$, but not contained in $@i[vars]$.  In this
   case ``similar'' means that the variable has the same name, but
   it may have a numerical suffix to make it distinct. @tt[maybe_] forms attempt to
   use the priginal name, if possible, and only append the suffix when necessary.
   @end[doc]

   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.

   Copyright (C) 1998 Jason Hickey, Cornell University

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Jason Hickey @email{jyh@cs.caltech.edu}
   Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}

   @end[license]
>>

doc <:doc<
   @begin[doc]
   @parents
   @end[doc]
>>
extends Summary
doc docoff

open Printf
open Lm_symbol
open Lm_debug
open Lm_string_util

open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Tactic_type
open Perv

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Var%t"

(*
 * Generate a new variable disjoint from the given vars.
 *)
let mem' vars v =
   List.mem v vars

let new_var v vars =
   new_name v (mem' vars)

let maybe_new_var v vars =
   if List.mem v vars then
      new_var v vars
   else v

let maybe_new_var_set v vars =
   if SymbolSet.mem vars v then
      new_name v (SymbolSet.mem vars)
   else v

let maybe_new_vars vars vars' =
   let rec aux l l' = function
      h::t ->
         let h' = maybe_new_var h l in
            aux (h'::l) (h'::l') t
    | [] -> l'
   in
      aux vars' [] vars

let maybe_new_var_arg p v =
   let vars = Sequent.avoid_vars p in
      maybe_new_var_set v vars

let bv = Lm_symbol.add "vv"

let var_subst_to_bind t1 t2 =
   let vs = free_vars_set t1 in
   let v =
      if SymbolSet.mem vs bv then
         new_name bv (SymbolSet.mem vs)
      else
         bv
   in
      mk_bind1_term v (var_subst t1 t2 v)

let get_bind_from_arg_or_concl_subst p t =
   try
      let b = Tacticals.get_with_arg p in
         if is_bind1_term b then b
         else raise generic_refiner_exn (* will be immedeiatelly caugh *)
   with RefineError _ ->
      var_subst_to_bind (Sequent.concl p) t

let get_bind_from_arg_or_hyp_subst p i t =
   try
      let b = Tacticals.get_with_arg p in
         if is_bind1_term b then b
         else raise generic_refiner_exn (* will be immedeiatelly caugh *)
   with RefineError _ ->
      var_subst_to_bind (Sequent.nth_hyp p i) t

(*
 * Symbol generation.
 *)
let new_symbol_term v =
   mk_var_term (new_symbol v)

let new_symbol_string_term s =
   mk_var_term (new_symbol_string s)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
