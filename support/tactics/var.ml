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
open Mp_debug
open String_util
open String_set

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
 * Split a varname into root, suffix
 * according to the pattern %s%d
 *)
let split_var v =
   let len = String.length v in
      if len = 0 then
         v, 0
      else
         let rec search i =
            if i = 0 then
               v, 0
            else if is_digit v.[i - 1] then
               search (i - 1)
            else if i = len then
               v, 0
            else
               String.sub v 0 i, int_of_string (String.sub v i (len - i))
         in
            search len

(*
 * Generate a new variable disjoint from the given vars.
 * If the var has a name matching "%s%d", then keep the same form
 * and use the smallest index to keep the var name disjoint.
 *)
let mem' vars v = List.mem v vars

let new_var v vars =
   String_util.vnewname (fst (split_var v)) (mem' vars)

let maybe_new_var v vars =
   if List.mem v vars then
      new_var v vars
   else
      v

let maybe_new_vars vars vars' =
   let rec aux l l' = function
      h::t ->
         let h' = maybe_new_var h l in
            aux (h'::l) (h'::l') t
    | [] -> l'
   in
      aux vars' [] vars

let maybe_new_var_arg p v =
   let vars = Sequent.declared_vars p in
      maybe_new_var v vars

let bv = "bnd"

let var_subst_to_bind t1 t2 =
   let vs = free_vars_set t1 in
   let v =
      if StringSet.mem vs bv then vnewname bv (StringSet.mem vs) else bv
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
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
