(*
 * Manifest terms.
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
 * Copyright (C) 1998 Jason Hickey, Alexey Nogin, Cornell University
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
 * Authors: Jason Hickey, Alexey Nogin
 *)

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_op_sig
open Term_subst_sig
open Term_man_gen_sig

module TermMan (**)
   (Term : TermSig)
   (TermBase : TermBaseSig
    with type term = Term.term
    with type term' = Term.term'
    with type bound_term = Term.bound_term
    with type bound_term' = Term.bound_term'
    with type operator = Term.operator
    with type operator' = Term.operator'
    with type param = Term.param
    with type param' = Term.param'
    with type level_exp = Term.level_exp
    with type level_exp' = Term.level_exp'
    with type level_exp_var = Term.level_exp_var
    with type level_exp_var' = Term.level_exp_var'
    with type hypothesis = Term.hypothesis
    with type seq_hyps = Term.seq_hyps
    with type seq_goals = Term.seq_goals)
   (TermOp : TermOpSig
    with type term = Term.term)
   (TermSubst : TermSubstSig
    with type term = Term.term
    with type param = Term.param)
   (RefineError : RefineErrorSig
    with type term = Term.term)
: TermManGenSig
  with type term = Term.term
  with type bound_term = Term.bound_term
  with type operator = Term.operator
  with type level_exp = Term.level_exp
  with type esequent = Term.esequent
  with type hypothesis = Term.hypothesis
  with type match_term = Term.match_term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
