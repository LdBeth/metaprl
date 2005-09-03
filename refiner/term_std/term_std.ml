(*
 * This is the simple term module, where the
 * implementation of the term mirrors the interface.
 * Destructors are identity functions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
open Lm_debug
open Lm_symbol

open Opname
open Term_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Term_std%t"

(*
 * Type of terms.
 *)
module TermType =
struct
   (*
    * The type are just the naive types.
    *)
   type level_exp_var = level_exp_var'
   and level_exp = level_exp'
   and param = param'
   and operator = operator'

   and term = term'
   and bound_term = bound_term'

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   and level_exp_var' = poly_level_exp_var
   and level_exp' = level_exp_var poly_level_exp
   and param' = (level_exp, param) poly_param
   and object_id = param list

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   and operator' = param poly_operator

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
   and term' = (operator, bound_term) poly_term
   and bound_term' = term poly_bound_term

   type meta_term = term poly_meta_term
   type hypothesis = term poly_hypothesis
   type seq_hyps = hypothesis Seq_set.linear_set

   type esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_concl : term
      }

   (*
    * Define types used in pattern matching.
    *)
   type match_param = (param, level_exp) poly_match_param
   type match_term =
      MatchTerm of string list * match_param list * bound_term' list
    | MatchSequent of string list * match_term * hypothesis list * term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
