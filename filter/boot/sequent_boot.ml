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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf
open Mp_debug

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
   type tactic_value = TacticInternalType.tactic_value
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

   (*
    * Sequent parts.
    *)
   let hyp_count arg =
      TermMan.num_hyps (goal arg)

   let hyp_count_addr arg =
      let goal = goal arg in
         TermMan.hyp_range_addr goal (TermMan.num_hyps goal)

   let hyp_split_addr arg i =
      let goal = goal arg in
      let count = TermMan.num_hyps goal in
      let j, k =
         if i < 0 then
            count + i + 1, (-1) - i
         else
            i, count - i
      in
         TermMan.hyp_range_addr goal j, TermMan.hyp_range_addr goal k

   let hyp_indices arg i =
      TermMan.hyp_indices_addr (goal arg) i

   let get_pos_hyp_num arg i =
      if i < 0 then
         (hyp_count arg) + i + 1
      else
         i

   let nth_hyp p i = TacticInternal.nth_hyp p (get_pos_hyp_num p i)
   let nth_binding p i = TacticInternal.nth_binding p (get_pos_hyp_num p i)

   let num_assums = TacticInternal.num_assums
   let nth_assum = TacticInternal.nth_assum

   let clause_addr p i =
      TermMan.nth_clause_addr (goal p) (get_pos_hyp_num p i)

   let get_decl_number arg v =
      TermMan.get_decl_number (goal arg) v

   let declared_vars arg =
      let seq = msequent arg in
      let vars = msequent_free_vars seq in
      let goal, _ = dest_msequent seq in
         vars @ (TermMan.declared_vars goal)

   let explode_sequent arg =
      TermMan.explode_sequent (goal arg)

   let is_free_seq_var i v p =
      TermMan.is_free_seq_var (get_pos_hyp_num p i) v (goal p)

   (*
    * Argument functions.
    *)
   let get_term_arg       = TacticInternal.get_term
   let get_term_list_arg  = TacticInternal.get_term_list
   let get_type_arg       = TacticInternal.get_type
   let get_int_arg        = TacticInternal.get_int
   let get_bool_arg       = TacticInternal.get_bool
   let get_string_arg     = TacticInternal.get_string
   let get_subst_arg      = TacticInternal.get_subst
   let get_resource_arg   = TacticInternal.get_resource
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
