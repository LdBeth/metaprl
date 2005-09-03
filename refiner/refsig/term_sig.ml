(*
 * Define terms.  This is the abstract term interface.
 * There may be several implementations of terms.
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
 * Copyright (C) 1998-2004 MetaPRL Group.
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
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol

open Opname

type 'term poly_hypothesis =
   Hypothesis of var * 'term
 | Context of var * var list * 'term list

(*
 * The terms in the framework include
 * a meta-implication and meta-iff.
 *)
(* %%MAGICBEGIN%% *)
type 'term poly_meta_term =
   MetaTheorem of 'term
 | MetaImplies of 'term poly_meta_term * 'term poly_meta_term
 | MetaFunction of 'term * 'term poly_meta_term * 'term poly_meta_term
 | MetaIff of 'term poly_meta_term * 'term poly_meta_term
 | MetaLabeled of string * 'term poly_meta_term

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
type poly_level_exp_var = { le_var : var; le_offset : int }
type 'le_var poly_level_exp = { le_const : int; le_vars : 'le_var list }

(*
 * Shape is the identifier of an operator in MetaPRL. Two operators are
 * considered to be the same if they have the same shape and are considered
 * to be unrelated otherwise.
 *)
type shape_param =
   ShapeNumber
 | ShapeString
 | ShapeToken
 | ShapeLevel
 | ShapeVar
 | ShapeShape
 | ShapeOperator
 | ShapeQuote

type shape = {
   shape_opname  : opname;
   shape_params  : shape_param list;
   shape_arities : int list
}

type 'param op_param = {
   opparam_name : opname;
   opparam_params : 'param list;
   opparam_arities : int list
}

(*
 * Parameters have a number of simple types.
 *)
type ('level_exp, 'param) poly_param =
   Number of Lm_num.num
 | String of string
 | Token of opname
 | Var of var
 | Shape of shape
 | Operator of 'param op_param
 | MNumber of var
 | MString of var
 | MToken of var
 | MShape of var
 | MOperator of var
 | MLevel of 'level_exp
 | Quote

   (* Special Nuprl5 values *)
 | ObId of 'param list
 | ParamList of 'param list

(*
 * Define a type of parameters used in pattern matching.
 * The main difference is lack of meta-variables, numbers
 * have an optional constant representation for small numbers,
 * and there are no Nuprl5 params.
 *)
type ('param, 'level_exp) poly_match_param =
   MatchNumber of Lm_num.num * int option
 | MatchString of string
 | MatchToken of opname * string list
 | MatchShape of shape
 | MatchOperator of 'param op_param
 | MatchVar of var
 | MatchLevel of 'level_exp
 | MatchUnsupported

(*
 * Bound terms.
 *)
type 'term poly_bound_term =
   { bvars : var list;
     bterm : 'term
   }

(*
 * Operators.
 *)
type 'param poly_operator =
   { op_name   : opname;
     op_params : 'param list
   }

(*
 * Terms.
 *)
type ('operator, 'bound_term) poly_term =
   { term_op    : 'operator;
     term_terms : 'bound_term list
   }

module type TermSig =
sig
   (************************************************************************
    * Types                                                                *
    ************************************************************************)

   (*
    * Operators have a name and parameters.
    *
    * When this file is copied to term_std.mli, the
    * following for type definitions become abstract.
    *)
   type level_exp_var
   and level_exp
   and param
   and operator

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
   and term
   and bound_term

   (************************************************************************
    * Interface types                                                      *
    ************************************************************************)

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   and level_exp_var' = poly_level_exp_var
   and level_exp' = level_exp_var poly_level_exp
   and operator' = param poly_operator

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
   and term' = (operator, bound_term) poly_term
   and bound_term' = term poly_bound_term

   and object_id = param list
   and param' = (level_exp, param) poly_param

   type meta_term = term poly_meta_term

   (************************************************************************
    * SEQUENTS                                                             *
    ************************************************************************)

   type hypothesis = term poly_hypothesis
   type seq_hyps

   type esequent =
      { sequent_args  : term;
        sequent_hyps  : seq_hyps;
        sequent_concl : term
      }
(* %%MAGICEND%% *)

   (************************************************************************
    * DESTRUCTION
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
