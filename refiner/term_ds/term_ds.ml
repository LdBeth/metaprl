(*
 * This is the module that implements delayed substitution,
 * keeps track of free variables and does some sharing.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

open Printf
open Nl_debug
open Opname

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_ds%t" eflush

module TermType =
struct
   (************************************************************************
    * Sets of strings                                                      *
    ************************************************************************)

   module OrderedString =
      struct
         type t = string
         let compare = Pervasives.compare
       end

   module StringSet = Red_black_set.Make (OrderedString)

   (************************************************************************
    * Types                                                                *
    ************************************************************************)

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   type level_exp_var' = { le_var : string; le_offset : int }
   type level_exp_var = level_exp_var'

   type level_exp' = { le_const : int; le_vars : level_exp_var list }
   type level_exp = level_exp'

   (*
    * Parameters have a number of simple types.
    *)
   type param' =
      Number of Nl_num.num
    | String of string
    | Token of string
    | Level of level_exp
    | Var of string
    | MNumber of string
    | MString of string
    | MToken of string
    | MLevel of string
    | MVar of string

      (* Special Nuprl5 values *)
    | ObId of object_id
    | ParamList of param list

      (* Num operations *)
    | MSum of param * param
    | MDiff of param * param
    | MProduct of param * param
    | MQuotient of param * param
    | MRem of param * param
    | MLessThan of param * param

      (* Comparisons *)
    | MEqual of param * param
    | MNotEqual of param * param

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
    * Subst (BSubst) - delayed simultanious substitution
    *)

   type term_subst = (string * term) list
   and term_core =
      Term of term'
    | Subst of term * term_subst
    | Sequent of esequent
    | FOVar of string
   and term = { mutable free_vars : lazy_vars; mutable core : term_core }
   and bound_term_core =
      BTerm of bound_term'
    | BSubst of bound_term * term_subst
   and bound_term = { mutable bfree_vars : lazy_vars; mutable bcore: bound_term_core }
   and term' = { term_op : operator; term_terms : bound_term list }
   and bound_term' = { bvars : string list; bterm : term }
   and hypothesis =
      Hypothesis of string * term
    | Context of string * term list
   and seq_hyps = hypothesis array
   and seq_goals = term array
   and esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_goals : seq_goals
      }
   and lazy_vars =
      Vars of StringSet.t
    | VarsDelayed

   (*
    * The terms in the framework include
    * a meta-implication and met-iff.
    *)
   type meta_term =
      MetaTheorem of term
    | MetaImplies of meta_term * meta_term
    | MetaFunction of term * meta_term * meta_term
    | MetaIff of meta_term * meta_term

end
