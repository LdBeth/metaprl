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

open Lm_debug
open Lm_printf
open Term_sig
open Term_base_sig
open Term_addr_sig
open Refine_error_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite_util%t"

let debug_rewrite =
   create_debug (**)
      { debug_name = "rewrite";
        debug_description = "Term rewriting operations";
        debug_value = false
      }

let debug_subst =
   create_debug (**)
      { debug_name = "subst";
        debug_description = "Substitution operations";
        debug_value = false
      }

module MakeRewriteUtil
   (TermType : TermSig)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (Term : TermBaseSig with module TermTypes = TermType)
   (RefineError : RefineErrorSig
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type term = TermType.term
    with type bound_term = TermType.bound_term)
   =
struct
   module RewriteTypes = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr)
   open Term
   open RefineError
   open RewriteTypes

   type term = TermType.term
   type rstack = RewriteTypes.rstack

   let iter2_exn = RefineError("Rewrite.iter2", StringError "length mismatch")
   let rev_iter2_exn = RefineError("Rewrite.rev_iter2", StringError "length mismatch")
   let redex_params_iter_exn = RefineError("Rewrite.match_redex_params_iter", StringError "length mismatch")

   let rec iter2_3 f arg1 arg2 arg3 al bl =
      match (al, bl) with
         h1::t1, h2::t2 ->
            f arg1 arg2 arg3 h1 h2;
            iter2_3 f arg1 arg2 arg3 t1 t2
       | [], [] ->
            ()
       | _ ->
            REF_RAISE(iter2_exn)

   let rec iter2_1 f arg1 al bl =
      match (al, bl) with
         h1::t1, h2::t2 ->
            f arg1 h1 h2;
            iter2_1 f arg1 t1 t2
       | [], [] ->
            ()
       | _ ->
            REF_RAISE(iter2_exn)

   let rec rev_iter2_3 f arg1 arg2 arg3 a b = match (a,b) with
         ([], []) -> ()
       | (ha::ta, hb::tb) -> rev_iter2_3 f arg1 arg2 arg3 ta tb; f arg1 arg2 arg3 ha hb
       | _ -> REF_RAISE (rev_iter2_exn)

   (*
    * Upgrade a second-order instance to a pattern
    *)
   let rec rstack_upgrade v = function
      (SOVarInstance (v', conts, i))::t when Lm_symbol.eq v' v ->
         (SOVarPattern (v, conts, i))::t
    | FreeFOVarInstance v' :: t when Lm_symbol.eq v' v ->
         FreeFOVarPattern v :: t
    | h::t ->
         h::(rstack_upgrade v t)
    | [] ->
         raise (Invalid_argument "rstack_upgrade")

   (*
    * Check the arity of a variable.
    *)
   let check_arity v conts arity = function
      SOVarPattern  (_, conts', i)
    | SOVarInstance (_, conts', i) ->
         if conts <> conts' then
            REF_RAISE(RefineError ("Rewrite_util.check_arity", StringVarError("bound contexts mismatch", v)));
         if arity <> i then
            REF_RAISE(RefineError ("Rewrite_util.check_arity", RewriteSOVarArity v))
    | _ ->
         REF_RAISE(RefineError ("Rewrite_util.check_arity", RewriteSOVarArity v))

   let rec rstack_check_arity v conts arity = function
      [] ->
         raise (Invalid_argument "Rewrite_util.rstack_check_arity")
    | ((FreeFOVarPattern v' | FreeFOVarInstance v' | SOVarPattern (v', _, _) | SOVarInstance (v', _, _) | FOVar v' | CVar v' | PVar (v', _)) as h) :: t ->
         if Lm_symbol.eq v' v then check_arity v conts arity h else rstack_check_arity v conts arity t

   (*
    * Membership functions.
    *)
   let rstack_var = function
      FreeFOVarPattern v
    | FreeFOVarInstance v
    | SOVarPattern (v, _, _)
    | SOVarInstance (v, _, _)
    | FOVar v
    | CVar v
    | PVar (v, _) ->
         v

   let rstack_mem_prop v rs =
      Lm_symbol.eq (rstack_var rs) v

   let rstack_so_mem_prop v = function
      SOVarPattern (v', _, _)
    | SOVarInstance (v', _, _) ->
         Lm_symbol.eq v v'
    | _ -> false

   let rstack_pattern_mem_prop v = function
      SOVarPattern (v', _, _) ->
         Lm_symbol.eq v v'
    | FreeFOVarPattern v' ->
         Lm_symbol.eq v v'
    | _ ->
         false

   let rstack_freefo_mem_prop v = function
      FreeFOVarPattern v' | FreeFOVarInstance v' -> Lm_symbol.eq v v'
    | _ -> false

   let rstack_fo_mem_prop v = function
      FOVar v' -> Lm_symbol.eq v v'
    | _ -> false

   let rstack_p_mem_prop shape v = function
      PVar (v', shape') -> v = v' && shape = shape'
    | _ -> false

   let rstack_c_mem_prop v = function
      CVar v' -> Lm_symbol.eq v v'
    | _ -> false

   let rstack_mem v = List.exists (rstack_mem_prop v)
   let rstack_so_mem v = List.exists (rstack_so_mem_prop v)
   let rstack_pattern_mem v = List.exists (rstack_pattern_mem_prop v)
   let rstack_freefo_mem v = List.exists (rstack_freefo_mem_prop v)
   let rstack_fo_mem v = List.exists (rstack_fo_mem_prop v)
   let rstack_p_mem shape v = List.exists (rstack_p_mem_prop shape v)
   let rstack_c_mem v = List.exists (rstack_c_mem_prop v)

   let array_rstack_mem v = Lm_array_util.exists (rstack_mem_prop v)
   let array_rstack_so_mem v = Lm_array_util.exists (rstack_so_mem_prop v)
   let array_rstack_freefo_mem v = Lm_array_util.exists (rstack_freefo_mem_prop v)
   let array_rstack_fo_mem v = Lm_array_util.exists (rstack_fo_mem_prop v)
   let array_rstack_c_mem v = Lm_array_util.exists (rstack_c_mem_prop v)
   let array_rstack_p_mem shape v = Lm_array_util.exists (rstack_p_mem_prop shape v)

   (*
    * Indexing.
    *)
   let rstack_index v l = Lm_list_util.find_item (rstack_mem_prop v) l
   let rstack_so_index v l = Lm_list_util.find_item (rstack_so_mem_prop v) l
   let rstack_freefo_index v l = Lm_list_util.find_item (rstack_freefo_mem_prop v) l
   let rstack_fo_index v l = Lm_list_util.find_item (rstack_fo_mem_prop v) l
   let rstack_p_index shape v l = Lm_list_util.find_item (rstack_p_mem_prop shape v) l
   let rstack_c_index v l = Lm_list_util.find_item (rstack_c_mem_prop v) l

   let array_rstack_index v l = Lm_array_util.find_index (rstack_mem_prop v) l
   let array_rstack_so_index v l = Lm_array_util.find_index (rstack_so_mem_prop v) l
   let array_rstack_freefo_index v l = Lm_array_util.find_index (rstack_freefo_mem_prop v) l
   let array_rstack_fo_index v l = Lm_array_util.find_index (rstack_fo_mem_prop v) l
   let array_rstack_p_index shape v l = Lm_array_util.find_index (rstack_p_mem_prop shape v) l
   let array_rstack_c_index v l = Lm_array_util.find_index (rstack_c_mem_prop v) l

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
