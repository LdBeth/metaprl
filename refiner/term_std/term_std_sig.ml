(*
 * Signature for the term_std module.
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

open Opname
open Refine_error_sig
open Term_base_sig

(*
 * The types for the standard refiner.
 *)
module type TermStdTypeSig =
sig
   (************************************************************************
    * Type definitions                                                     *
    ************************************************************************)

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
    | Context of var * term list

   type seq_hyps = hypothesis SEQ_SET.linear_set
   type seq_goals = term SEQ_SET.linear_set

   type esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_goals : seq_goals
      }

end

(*
 * Basic operations on terms.
 *)
module type TermStdSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type level_exp_var
   type level_exp
   type param
   type operator
   type term
   type bound_term
   type seq_hyps
   type seq_goals

   type hypothesis
   type level_exp_var'
   type level_exp'
   type object_id
   type param'
   type operator'
   type term'
   type bound_term'
   type match_param

   module SeqHyp : Linear_set.LinearSetSig with type elt = hypothesis with type t = seq_hyps
   module SeqGoal : Linear_set.LinearSetSig with type elt = term with type t = seq_goals

   (************************************************************************
    * De/Constructors                                                      *
    ************************************************************************)

   (*
    * General interface.
    *)
   val mk_term : operator -> bound_term list -> term
   val make_term : term' -> term
   val dest_term : term -> term'
   val mk_op : opname -> param list -> operator
   val make_op : operator' -> operator
   val dest_op : operator -> operator'
   val mk_bterm : var list -> term -> bound_term
   val make_bterm : bound_term' -> bound_term
   val dest_bterm : bound_term -> bound_term'
   val make_param : param' -> param
   val dest_param : param -> param'
   val dest_params : param list -> param' list
   val mk_level : int -> level_exp_var list -> level_exp
   val make_level : level_exp' -> level_exp
   val dest_level : level_exp -> level_exp'
   val mk_level_var : var -> int -> level_exp_var
   val make_level_var : level_exp_var' -> level_exp_var
   val dest_level_var : level_exp_var -> level_exp_var'

   val make_object_id : param list -> object_id
   val dest_object_id : object_id  ->  param list

   (* Projections *)
   val opname_of_term : term -> opname
   val subterms_of_term : term -> term list
   val subterm_count : term -> int
   val subterm_arities : term -> int list

   (*
    * A variable is a term with opname "var", and a single
    * var parameter that is the name of the variable.
    *)
   val var_opname : opname
   val context_opname : opname
   val xperv : opname
   val sequent_opname : opname

   val is_var_term : term -> bool
   val dest_var : term -> var
   val mk_var_term : var -> term

   val is_so_var_term : term -> bool
   val dest_so_var : term -> var * term list
   val mk_so_var_term : var -> term list -> term

   val is_context_term : term -> bool
   val dest_context : term -> var * term * term list
   val mk_context_term : var -> term -> term list -> term

   (*
    * Simple terms have no paramaters and
    * all subterms have no binding vars.
    *)
   val mk_any_term : operator -> term list -> term
   val mk_simple_term : opname -> term list -> term
   val dest_simple_term : term -> (opname * term list)
   val is_simple_term_opname : opname -> term -> bool
   val dest_simple_term_opname : opname -> term -> term list

   val mk_simple_bterm : term -> bound_term
   val dest_simple_bterm : bound_term -> term

   (*
    * We allow a term printer to be injected.
    *)
   val debug_print : out_channel -> term -> unit
   val print_term : out_channel -> term -> unit
   val print_term_list : out_channel -> term list -> unit
   val install_debug_printer : (out_channel -> term -> unit) -> unit

   (*
    * Destruct a term for easy pattern-matching
    *)
   val explode_term : term -> string list * match_param list * bound_term' list

   (*
    * This function is not implemented, and it always returns the
    * invalid descriptor.
    *)
   val dest_descriptor : term -> term Weak_memo.TheWeakMemo.descriptor option
   val mk_descriptor_term : term Weak_memo.TheWeakMemo.descriptor -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
