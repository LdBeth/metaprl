(*
 * Addressed operations on terms.
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
 * Authors: Alexey Nogin
 *)

open Refine_error_sig
open Term_ds_sig
open Term_ds
open Term_op_sig
open Term_addr_sig

type addr =
   Path of int list
 | ArgAddr
 | HypAddr of int
 | GoalAddr of int
 | Compose of addr * addr

module TermAddr (**)
   (Term : TermDsSig
    with type level_exp_var = TermType.level_exp_var
    with type level_exp = TermType.level_exp
    with type param = TermType.param
    with type operator = TermType.operator
    with type term = TermType.term
    with type term_core = TermType.term_core
    with type bound_term = TermType.bound_term
    with type esequent = TermType.esequent
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals

    with type hypothesis = TermType.hypothesis
    with type level_exp_var' = TermType.level_exp_var'
    with type level_exp' = TermType.level_exp'
    with type object_id = TermType.object_id
    with type param' = TermType.param'
    with type operator' = TermType.operator'
    with type term' = TermType.term'
    with type bound_term' = TermType.bound_term')
   (TermOp : TermOpSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr)
: TermAddrSig
  with type term = TermType.term
  with type address = addr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
