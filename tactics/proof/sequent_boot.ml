(*
 * Utilities for tactics.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Lm_debug
open Lm_symbol

open Term_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Refine

open Tactic_boot

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Sequent%t"

module Sequent =
struct
   (*
    * Types.
    *)
   type extract = TacticInternalType.extract
   type conv = TacticInternalType.conv
   type tactic = TacticInternalType.tactic
   type tactic_arg = TacticInternalType.tactic_arg
   type sentinal = TacticInternalType.sentinal
   type raw_attribute = TacticInternalType.raw_attribute

   (*
    * Two tactic_arguments are equal when they have
    * equal msequent parts.  Their labels, etc, are
    * not compared.
    *)
   let tactic_arg_alpha_equal = TacticInternal.tactic_arg_alpha_equal
   let tactic_arg_alpha_equal_concl = TacticInternal.tactic_arg_alpha_equal_concl

   (*
    * Addressing.
    *)
   let goal = TacticInternal.goal

   let msequent = TacticInternal.msequent

   let concl arg =
      TacticInternal.nth_concl arg 1

   let label = TacticInternal.label

   let args p =
      let { sequent_args = args } = TermMan.explode_sequent (goal p) in
         TermMan.dest_xlist args

   let num_assums = TacticInternal.num_assums
   let nth_assum = TacticInternal.nth_assum

   (*
    * Sequent parts.
    *)
   let hyp_count arg =
      TermMan.num_hyps (goal arg)

   let get_pos_hyp_num arg i =
      if i < 0 then
         (hyp_count arg) + i + 1
      else
         i

   let assum_hyp_count arg i =
      TermMan.num_hyps (nth_assum arg i)

   let nth_hyp p i = TacticInternal.nth_hyp p (get_pos_hyp_num p i)
   let nth_binding p i = TacticInternal.nth_binding p (get_pos_hyp_num p i)

   let clause_addr p i =
      TermAddr.nth_clause_addr (goal p) i

   let assum_clause_addr p i j =
      TermAddr.nth_clause_addr (nth_assum p i) j

   let get_decl_number arg v =
      TermMan.get_decl_number (goal arg) v

   let avoid_vars arg =
      let seq = msequent arg in
      let goal, _ = dest_msequent seq in
         SymbolSet.add_list (msequent_free_vars seq) (TermMan.declared_vars goal)

   let explode_sequent arg =
      TermMan.explode_sequent (goal arg)

   let rec all_hyps_aux hyps l i =
      if i = 0 then l else
      let i = pred i in
         match Term.SeqHyp.get hyps i with
            Hypothesis t | HypBinding (_, t) ->
               all_hyps_aux hyps (t::l) i
          | Context _ ->
               all_hyps_aux hyps l i

   let all_hyps arg =
      let hyps = (explode_sequent arg).sequent_hyps in
         all_hyps_aux hyps [] (Term.SeqHyp.length hyps)

   (*
    * Argument functions.
    *)
   let get_term_arg       = TacticInternal.get_term
   let get_term_list_arg  = TacticInternal.get_term_list
   let get_type_arg       = TacticInternal.get_type
   let get_int_arg        = TacticInternal.get_int
   let get_bool_arg       = TacticInternal.get_bool
   let get_string_arg     = TacticInternal.get_string
   let get_resource_arg   = TacticInternal.get_resource
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
