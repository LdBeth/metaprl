(*
 * This is the module that implements delayed substitution,
 * keeps track of free variables and does some sharing.
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
   show_loading "Loading Term_ds%t"

module TermType =
struct
   (************************************************************************
    * Types                                                                *
    ************************************************************************)

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   type level_exp_var' = poly_level_exp_var
   type level_exp_var = level_exp_var'

   type level_exp' = level_exp_var poly_level_exp
   type level_exp = level_exp'

   type param' = (level_exp, param) poly_param
   and object_id = param list
   and param = param'

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   type operator' = param poly_operator
   type operator =  operator'

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *
    * free_vars - set of the free variables
    *
    * Subst (BSubst) - delayed simultanious substitution
    *)

   type term_subst = (var * term) list
   and term_core =
      Term of term'
    | Subst of term * term_subst
    | Sequent of esequent
    | FOVar of var
    | SOVar of var * var list * term list
    | Hashed of term Weak_memo.TheWeakMemo.descriptor
    | SOContext of var * term * var list * term list
   and term = { mutable free_vars : lazy_vars; mutable core : term_core }
   and bound_term = bound_term'
   and term' = (operator, bound_term) poly_term
   and bound_term' = term poly_bound_term
   and hypothesis = term poly_hypothesis
   and seq_hyps = hypothesis Seq_set.linear_set
   and esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_concl : term
      }
   and lazy_vars =
      Vars of SymbolSet.t
    | VarsDelayed

   (*
    * Define a type of parameters used in pattern matching.
    * The main difference is lack of meta-variables, numbers
    * have an optional constant representation for small numbers,
    * and there are no Nuprl5 params.
    *)
   type match_param =
      MatchNumber of Lm_num.num * int option
    | MatchString of string
    | MatchToken of opname * string list
    | MatchVar of var
    | MatchLevel of level_exp
    | MatchUnsupported

   type match_term =
      MatchTerm of string list * match_param list * bound_term' list
    | MatchSequent of string list * match_term * hypothesis list * term

   type meta_term = term poly_meta_term
end
