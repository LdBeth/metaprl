(*
 * Utilities used in the rewriter.
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

INCLUDE "refine_error.mlh"

open Printf
open Mp_debug
open Opname
open Term_sig
open Term_base_sig
open Refine_error_sig

open Rewrite_type_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite_util%t"

let debug_rewrite =
   create_debug (**)
      { debug_name = "rewrite";
        debug_description = "Show term rewriting operations";
        debug_value = false
      }

let debug_subst =
   create_debug (**)
      { debug_name = "subst";
        debug_description = "Show substitution operations";
        debug_value = false
      }

module MakeRewriteUtil
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term = TermType.term
    with type term' = TermType.term'
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term'
    with type operator = TermType.operator
    with type operator' = TermType.operator'
    with type param = TermType.param
    with type param' = TermType.param'
    with type level_exp = TermType.level_exp
    with type level_exp' = TermType.level_exp'
    with type object_id = TermType.object_id)
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
   (RewriteTypes : RewriteTypesSig)
   =
struct
   open TermType
   open Term
   open RefineError
   open RewriteTypes

   type term = TermType.term
   type rstack = RewriteTypes.rstack

   let iter2_exn = RefineError("Rewrite.iter2", StringError "length mismatch")
   let rev_iter2_exn = RefineError("Rewrite.rev_iter2", StringError "length mismatch")
   let redex_params_iter_exn = RefineError("Rewrite.match_redex_params_iter", StringError "length mismatch")

   let rec iter2 f al bl =
      match (al, bl) with
         h1::t1, h2::t2 ->
            f h1 h2;
            iter2 f t1 t2
       | [], [] ->
            ()
       | _ ->
            REF_RAISE(iter2_exn)

   let rec rev_iter2 f a b =   match (a,b) with
         ([], []) -> ()
       | (ha::ta, hb::tb) -> rev_iter2 f ta tb; f ha hb
       | _ -> REF_RAISE (rev_iter2_exn)

   (*
    * Upgrade a second-order instance to a pattern
    *)
   let rec rstack_upgrade v = function
      (SOVarInstance (v', i))::t when v' = v ->
         (SOVarPattern (v, i))::t
    | ((SOVarPattern (v', _))::t as stack) when v' = v ->
         stack
    | (FOVarPattern v')::t when v' = v ->
         (SOVarPattern (v, 0))::t
    | h::t ->
         h::(rstack_upgrade v t)
    | [] ->
         raise (Invalid_argument "rstack_upgrade")

   (*
    * Check the arity of a variable.
    *)
   let rec rstack_check_arity v arity = function
      [] ->
         raise (Failure "Rewrite.rstack_check_arity")
    | h::t ->
         match h with
            FOVarPattern v' ->
               if v' = v then
                  if arity = 0 then
                     ()
                  else
                     REF_RAISE(RefineError ("rstack_check_arity", RewriteSOVarArity v))
               else
                  rstack_check_arity v arity t
          | SOVarPattern (v', i) ->
               if v' = v then
                  if i = arity then
                     ()
                  else
                     REF_RAISE(RefineError ("rstack_check_arity", RewriteSOVarArity v))
               else
                  rstack_check_arity v arity t
          | SOVarInstance (v', i) ->
               if v' = v then
                  if i = arity then
                     ()
                  else
                     REF_RAISE(RefineError ("rstack_check_arity", RewriteSOVarArity v))
               else
                  rstack_check_arity v arity t
          | _ ->
               rstack_check_arity v arity t

   (*
    * Membership functions.
    *)
   let rstack_mem_prop v = function
      FOVarPattern v' -> v = v'
    | SOVarPattern (v', _) -> v = v'
    | SOVarInstance (v', _) -> v = v'
    | FOVar v' -> v = v'
    | CVar v' -> v = v'
    | PIVar v' -> v = v'
    | PSVar v' -> v = v'
    | PLVar v' -> v = v'

   let rstack_so_mem_prop v = function
      FOVarPattern v' -> v = v'
    | SOVarPattern (v', _) -> v = v'
    | SOVarInstance (v', _) -> v = v'
    | _ -> false

   let rstack_pattern_mem_prop v = function
      FOVarPattern v' -> v = v'
    | SOVarPattern (v', _) -> v = v'
    | _ -> false

   let rstack_fo_mem_prop v = function
      FOVar v' -> v = v'
    | _ -> false

   let rstack_p_mem_prop v = function
      PIVar v' -> v = v'
    | PSVar v' -> v = v'
    | PLVar v' -> v = v'
    | _ -> false

   let rstack_c_mem_prop v = function
      CVar v' -> v = v'
    | _ -> false

   let rstack_mem v = List.exists (rstack_mem_prop v)
   let rstack_so_mem v = List.exists (rstack_so_mem_prop v)
   let rstack_pattern_mem v = List.exists (rstack_pattern_mem_prop v)
   let rstack_fo_mem v = List.exists (rstack_fo_mem_prop v)
   let rstack_p_mem v = List.exists (rstack_p_mem_prop v)
   let rstack_c_mem v = List.exists (rstack_c_mem_prop v)

   let array_rstack_mem v = Array_util.exists (rstack_mem_prop v)
   let array_rstack_so_mem v = Array_util.exists (rstack_so_mem_prop v)
   let array_rstack_fo_mem v = Array_util.exists (rstack_fo_mem_prop v)
   let array_rstack_c_mem v = Array_util.exists (rstack_c_mem_prop v)
   let array_rstack_p_mem v = Array_util.exists (rstack_p_mem_prop v)

   (*
    * Indexing.
    *)
   let rstack_index v l = List_util.find_item (rstack_mem_prop v) l
   let rstack_so_index v l = List_util.find_item (rstack_so_mem_prop v) l
   let rstack_fo_index v l = List_util.find_item (rstack_fo_mem_prop v) l
   let rstack_p_index v l = List_util.find_item (rstack_p_mem_prop v) l
   let rstack_c_index v l = List_util.find_item (rstack_c_mem_prop v) l

   let array_rstack_index v l = Array_util.find_index (rstack_mem_prop v) l
   let array_rstack_so_index v l = Array_util.find_index (rstack_so_mem_prop v) l
   let array_rstack_fo_index v l = Array_util.find_index (rstack_fo_mem_prop v) l
   let array_rstack_p_index v l = Array_util.find_index (rstack_p_mem_prop v) l
   let array_rstack_c_index v l = Array_util.find_index (rstack_c_mem_prop v) l

   (*
    * Find the index of a binding var into the stack
    * given an association list of indices.
    *)
   let var_index bvars t =
      let s = dest_var t in
         try List.assoc s bvars with
            Not_found -> REF_RAISE(RefineError ("var_index", RewriteFreeSOVar s))

   let svar_index bvars s =
      try List.assoc s bvars with
         Not_found -> REF_RAISE(RefineError ("var_index", RewriteFreeSOVar s))
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
