(*
 * Types for term_ds.
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

module type TermDsTypeSig =
sig
   (************************************************************************
    * Types                                                                *
    ************************************************************************)

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   type level_exp_var' = { le_var : var; le_offset : int }
   type level_exp_var = level_exp_var'

   type level_exp' = { le_const : int; le_vars : level_exp_var list }
   type level_exp = level_exp'

   (*
    * Parameters have a number of simple types.
    *)
   type param' =
      Number of Lm_num.num
    | String of string
    | Token of string
    | Var of var
    | MNumber of var
    | MString of var
    | MToken of var
    | MLevel of level_exp

      (* Special Nuprl5 values *)
    | ObId of object_id
    | ParamList of param list

   and object_id = param list
   and param = param'

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   type operator' = { op_name : opname; op_params : param list }
   type operator =  operator'

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *
    * free_vars - set of the free variables
    *
    * Subst - delayed simultanious substitution
    * Hashed descriptor: a descriptor into the global hash table
    *
    * Please read docs/term_ds_safety.txt if you plan to change
    * and/or modify any code that changes mutable fields.
    *)
   type term_subst = (var * term) list
   and term_core =
      Term of term'
    | Subst of term * term_subst
    | Sequent of esequent
    | FOVar of var
    | SOVar of var * var list * term list
    | Hashed of term Weak_memo.TheWeakMemo.descriptor
   and term = { mutable free_vars : lazy_vars; mutable core : term_core }
   and bound_term = bound_term'
   and term' = { term_op : operator; term_terms : bound_term list }
   and bound_term' = { bvars : var list; bterm : term }
   and hypothesis =
      HypBinding of var * term
    | Hypothesis of term
    | Context of var * var list * term list
   and esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_goals : seq_goals
      }
   and seq_hyps = hypothesis SEQ_SET.linear_set
   and seq_goals = term SEQ_SET.linear_set
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
    | MatchToken of string
    | MatchVar of var
    | MatchLevel of level_exp
    | MatchUnsupported

   type match_term =
      MatchTerm of string list * match_param list * bound_term' list
    | MatchSequent of string list * match_term list * hypothesis list * term list

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
end

module type TermDsSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type level_exp_var
   type level_exp
   type param
   type operator
   type term
   type term_core
   type bound_term
   type esequent
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

   type term_subst

   module SeqHyp : Lm_linear_set_sig.LinearSetSig
                   with type elt = hypothesis
                   with type t = seq_hyps
   module SeqGoal : Lm_linear_set_sig.LinearSetSig
                    with type elt = term
                    with type t = seq_goals

   (************************************************************************
    * De/Constructors                                                      *
    ************************************************************************)

   val free_vars_set: term -> SymbolSet.t
   val bterm_free_vars: bound_term -> SymbolSet.t

   val new_vars : SymbolSet.t -> var list -> (var * var) list * (var * term) list
   val rename_bvars : (var * var) list -> var list -> var list

   (*
    * Simultaneous delayed substitution.
    * These two functions would behave incorrectly if the term_subst mentions
    * the same variable more than once. Use Term_subst_ds.subst when there is
    * a possibility of duplicates.
    *)
   val do_term_subst : term_subst -> term -> term
   val do_bterm_subst : term_subst -> bound_term -> bound_term

   val get_core : term -> term_core
   val fail_core : string -> 'a
   val core_term : term_core -> term
   val dest_term : term -> term'
   val make_term : term' -> term
   val mk_op : opname -> param list -> operator
   val mk_term : operator -> bound_term list -> term
   val mk_bterm : var list -> term -> bound_term
   val make_bterm : bound_term' -> bound_term
   val dest_bterm : bound_term -> bound_term'
   val mk_level : int -> level_exp_var list -> level_exp
   val mk_level_var : var -> int -> level_exp_var
   val mk_sequent_term : esequent -> term

   val is_simple_bterm : bound_term -> bool
   val no_bvars : bound_term list -> bool
   val mk_simple_bterm : term -> bound_term
   val dest_simple_bterm : bound_term -> term

   (* Projections *)
   val opname_of_term : term -> opname
   val subterms_of_term : term -> term list
   val subterm_arities : term -> int list

   (* These are trivial identity functions *)
   val make_op : operator' -> operator
   val dest_op : operator -> operator'
   val make_param : param' -> param
   val dest_param : param -> param'
   val dest_params : param list -> param' list
   val make_level : level_exp' -> level_exp
   val dest_level : level_exp -> level_exp'
   val make_level_var : level_exp_var' -> level_exp_var
   val dest_level_var : level_exp_var -> level_exp_var'
   val make_object_id : param list -> object_id
   val dest_object_id : object_id  ->  param list

   (*
    * From external point of view, a variable is a term
    * with opname "var", and a single var parameter
    * that is the name of the variable.
    *
    * However, for efficiency purposes, internally
    * in Term_ds FO variables are represented specially
    *)

   val var_opname : opname
   val context_opname : opname
   val xperv : opname
   val sequent_opname : opname

   val is_var_term : term -> bool
   val dest_var : term -> var
   val mk_var_term : var -> term

   (* xlist terms are needed here to allow SO variable term' representation *)
   val xnil_opname : opname
   val xcons_opname : opname
   val xnil_term : term
   
   val is_xlist_term : term -> bool
   val dest_xlist : term -> term list
   val mk_xlist_term : term list -> term

   (*
    * Simple terms have no paramaters and
    * all subterms have no binding vars.
    *)
   val mk_any_term : operator -> term list -> term
   val mk_simple_term : opname -> term list -> term
   val dest_simple_term : term -> (opname * term list)
   val is_simple_term_opname : opname -> term -> bool
   val dest_simple_term_opname : opname -> term -> term list

   (*
    * We allow a term printer to be injected.
    *)
   val debug_print : out_channel -> term -> unit
   val print_term : out_channel -> term -> unit
   val print_term_list : out_channel -> term list -> unit
   val install_debug_printer : (out_channel -> term -> unit) -> unit

   (*
    * Wrap descriptors.
    *)
   val dest_descriptor : term -> term Weak_memo.TheWeakMemo.descriptor option
   val mk_descriptor_term : term Weak_memo.TheWeakMemo.descriptor -> term
end

