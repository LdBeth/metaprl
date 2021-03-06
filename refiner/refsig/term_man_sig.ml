(*
 * Operations on "manifest" terms.  These are
 * terms that have a pervasive definition.
 *
 * Sequents are manifest only for efficiency.
 * The refiner does not use them, but they are
 * included because many logics use sequents.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol

type var_info =
   ParamVar
 | FirstOrderVar
 | SecondOrderVar of int * int
 | ContextVar of int * int
 | SequentContextVar of int * int

module type TermManSig =
sig
   module ManTypes : Term_sig.TermSig
   open ManTypes

   (************************************************************************
    * Simplified operations on manifest terms                              *
    ************************************************************************)

   (* Level expression operations *)
   val mk_const_level_exp : int -> level_exp
   val mk_var_level_exp : var -> level_exp
   val max_level_exp : level_exp -> level_exp -> int -> level_exp
   val incr_level_exp : level_exp -> level_exp

   val level_le : level_exp -> level_exp -> bool
   val level_lt : level_exp -> level_exp -> bool

   (* A SO variable has has term and context parameters *)
   val is_so_var_term : term -> bool
   val dest_so_var : term -> var * var list * term list
   val mk_so_var_term : var -> var list -> term list -> term

   (* An FO var or an SO var without subterms, but possibly with contexts *)
   val is_fso_var_term : term -> bool
   val dest_fso_var : term -> var

   val is_context_term : term -> bool
   val dest_context : term -> var * term * var list * term list
   val mk_context_term : var -> term -> var list -> term list -> term

   (* A term is FO if it does not have bvars *)
   val is_fo_term : term -> bool

   (*
    * Context info is:  var -> (sequent_flag, context_arity, term_arity).
    * Second-order var info is: var -> (context_arity, term_arity).
    *)
   val context_vars_info :  (bool * int * int) SymbolTable.t -> term -> (bool * int * int) SymbolTable.t
   val so_vars_info :  (int * int) SymbolTable.t -> term -> (int * int) SymbolTable.t
   val param_vars_info : SymbolSet.t -> term -> SymbolSet.t
   val context_vars_info_list :  (bool * int * int) SymbolTable.t -> term list -> (bool * int * int) SymbolTable.t
   val so_vars_info_list :  (int * int) SymbolTable.t -> term list -> (int * int) SymbolTable.t
   val param_vars_info_list : SymbolSet.t -> term list -> SymbolSet.t

   val all_contexts : term -> SymbolSet.t
   val all_meta_variables : term -> SymbolSet.t (* returns both free contexts and all SO variables *)
   val context_vars : term -> SymbolSet.t * SymbolSet.t (* sequent conts, normal conts *)

   (* All vars in the term, even the bound ones *)
   val all_vars : term -> SymbolSet.t
   val all_vars_terms : term list -> SymbolSet.t
   val all_vars_info : var_info SymbolTable.t -> term -> var_info SymbolTable.t

   (*
    * Destruct a term for easy pattern-matching.
    *
    * For variables (both FO and SO) we use the "standard"
    * form with opname ["var"] and a MatchVar parameter.
    *)
   val explode_term : term -> match_term

   (*
    * Sequents.
    *)
   val is_sequent_term : term -> bool
   val mk_sequent_term : esequent -> term
   val explode_sequent : term -> esequent
   (* explode_sequent_and_rename is analoguos to dest_bterm_and_rename *)
   val explode_sequent_and_rename : term -> SymbolSet.t -> esequent

   (* Indexing starts at 1 *)
   (* Functions will raise RefineError when int is out of range *)
   (* nth_hyp and nth_binding will raise RefineError when index refers to a context *)
   val nth_hyp : term -> int -> term
   val nth_binding : term -> int -> var
   val args : term -> term
   val hyps : term -> term list
   val concl : term -> term
   val sequent_args : term -> term
   val sequent_hyps : term -> term list
   val sequent_concl : term -> term
   val num_hyps : term -> int
   val declared_vars : term -> var list
   val get_decl_number : term -> var -> int
   val get_hyp_number : term -> term -> int
   val replace_concl : term -> term -> term

   val is_xrewrite_term : term -> bool
   val mk_xrewrite_term : term -> term -> term
   val dest_xrewrite : term -> term * term

   (*
    * Primitive lists.
    *)
   val is_xnil_term : term -> bool
   val xnil_term : term

   val is_xcons_term : term -> bool
   val mk_xcons_term : term -> term -> term
   val dest_xcons : term -> term * term

   val is_xlist_term : term -> bool
   val dest_xlist : term -> term list
   val mk_xlist_term : term list -> term

   (*
    * Empty conclusion in a sequent.
    *)
   val xconcl_term : term

   (*
    * Primitive strings.
    *)
   val is_xstring_term : term -> bool
   val mk_xstring_term : string -> term
   val dest_xstring : term -> string

   val is_xstring_dep0_term : term -> bool
   val mk_xstring_dep0_term : string -> term -> term
   val dest_xstring_dep0_term : term -> string * term

   (*
    * Primitive abstractions.
    *)
   val is_xbind_term : term -> bool
   val mk_xbind_term : var -> term -> term

   (*
    * Construct a redex out of some vars, params, and other terms.
    *)
   val construct_redex : var array -> term list -> term list -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
