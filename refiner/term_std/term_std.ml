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
open Lm_symbol

open Printf

open Lm_debug
open Opname
open Refine_error_sig

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
   and level_exp_var' = { le_var : var; le_offset : int }

   and level_exp' = { le_const : int; le_vars : level_exp_var list }

   (*
    * Parameters have a number of simple types.
    *)
   and param' =
      Number of Lm_num.num
    | String of string
    | Token of string
    | Var of var
    | MNumber of var
    | MString of var
    | MToken of var
    | MLevel of level_exp
    | ObId of object_id
    | ParamList of param list

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   and object_id = param list

   and operator' = { op_name : opname; op_params : param list }

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
   and term' = { term_op : operator; term_terms : bound_term list }
   and bound_term' = { bvars : var list; bterm : term }

   (*
    * The terms in the framework include
    * a meta-implication and met-iff.
    *)
   type meta_term =
      MetaTheorem of term
    | MetaImplies of meta_term * meta_term
    | MetaFunction of term * meta_term * meta_term
    | MetaIff of meta_term * meta_term
    | MetaLabeled of string * meta_term

   type hypothesis =
      HypBinding of var * term
    | Hypothesis of term
    | Context of var * var list * term list

   type seq_hyps = hypothesis SEQ_SET.linear_set
   type seq_goals = term SEQ_SET.linear_set

   type esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_goals : seq_goals
      }

   (*
    * Define a type of parameters used in pattern matching.
    * The main difference is lack of meta-variables, numbers
    * have an optional constant representation for small numbers,
    * and there are no Nuprl5 params.
    *)
   type match_param =
      MatchNumber of Lm_num.num * int option
    | MatchString of string
    | MatchToken of string
    | MatchVar of var
    | MatchLevel of level_exp
    | MatchUnsupported

   type match_term =
      MatchTerm of string list * match_param list * bound_term' list
    | MatchSequent of term * hypothesis list * term list
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
