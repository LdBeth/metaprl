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
open Term_sig

type out_channel = Lm_printf.out_channel

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
   and param' = (level_exp, param) poly_param
   and object_id = param list
   and operator = operator'

   and term = term'
   and bound_term = bound_term'

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   and level_exp_var' = poly_level_exp_var
   and level_exp' = level_exp_var poly_level_exp

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

   (*
    * The terms in the framework include
    * a meta-implication and met-iff.
    *)
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
   type match_param = level_exp poly_match_param
   type match_term =
      MatchTerm of string list * match_param list * bound_term' list
    | MatchSequent of string list * match_term * hypothesis list * term
end

(*
 * Basic operations on terms.
 *)
module type TermStdSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   module TermTypes : TermStdTypeSig
   open TermTypes

   module SeqHyp : Lm_linear_set_sig.LinearSetSig with type elt = hypothesis with type t = seq_hyps

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
   val ops_eq : operator -> operator -> bool
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
   val subterm_arities : term -> int list

   (*
    * A variable is a term with opname "var", and a single
    * var parameter that is the name of the variable.
    *)
   val is_var_term : term -> bool
   val dest_var : term -> var
   val mk_var_term : var -> term

   (*
    * Simple terms have no paramaters and
    * all subterms have no binding vars.
    *)
   val mk_any_term : operator -> term list -> term
   val mk_simple_term : opname -> term list -> term
   val dest_simple_term : term -> (opname * term list)
   val is_simple_term_opname : opname -> term -> bool
   val dest_simple_term_opname : opname -> term -> term list

   val is_simple_bterm : bound_term -> bool
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
