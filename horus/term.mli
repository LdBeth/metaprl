(*
 * MetaPRL terms.
 * This is the non-abstract definition.
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

open Opname;;

(************************************************************************
 * Types                                                                *
 ************************************************************************)

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
type levelExpVar = { le_var : string; le_offset : int };;

type levelExp = { le_const : int; le_vars : levelExpVar list };;

(*
 * Parameters have a number of simple types.
 *)
type param =
   Number of int
 | String of string
 | Token of string
 | Level of levelExp
 | Var of string

   (* Meta-terms used for defining rewrites *)
 | MNumber of string
 | MString of string
 | MToken of string
 | MLevel of string
 | MVar of string

   (* Num operations *)
 | MSum of param * param
 | MDiff of param * param
 | MProduct of param * param
 | MQuotient of param * param
 | MRem of param * param;;

(*
 * An operator combines a name with a list of parameters.
 * The order of params is significant.
 *)
type operator = { op_name : opname; op_params : param list };;

(*
 * A term has an operator, and a finite number of subterms
 * that may be bound.
 *)
type term = { term_op : operator; term_terms : boundTerm list }
and boundTerm = { bvars : string list; bterm : term };;

(*
 * Address of a subterm.
 *)
type address;;

(************************************************************************
 * De/Constructors                                                      *
 ************************************************************************)

(* Errors during matching *)
exception TermMatch of term;;

(*
 * General interface.
 *)
val mk_term : operator -> boundTerm list -> term;;
val mk_op : opname -> param list -> operator;;
val mk_bterm : string list -> term -> boundTerm;;
val mk_level : int -> levelExpVar list -> levelExp;;
val mk_level_var : string -> int -> levelExpVar;;

val normalize_term : term -> term;;

(* Addresses *)
val string_of_address : address -> string;;
val make_address : int list -> address;;
val make_seq_address : int -> address;;
val nth_cdr_addr : int -> address;;

(* Projections *)
val opname_of_term : term -> opname;;

(*
 * Simple terms have no paramaters and
 * all subterms have no binding vars.
 *)
val mk_simple_term : opname -> term list -> term;;
val dest_simple_term : term -> (opname * term list);;

(* Special cases *)
val is_dep0_term : opname -> term -> bool;;
val mk_dep0_term : opname -> term -> term;;
val dest_dep0_term : opname -> term -> term;;
val one_subterm : term -> term;;

val is_dep0_dep0_term : opname -> term -> bool;;
val mk_dep0_dep0_term : opname -> term -> term -> term;;
val dest_dep0_dep0_term : opname -> term -> term * term;;
val two_subterms : term -> term * term;;

val is_dep0_dep0_dep0_term : opname -> term -> bool;;
val mk_dep0_dep0_dep0_term : opname -> term -> term -> term -> term;;
val dest_dep0_dep0_dep0_term : opname -> term -> term * term * term;;
val three_subterms : term -> term * term * term;;

val is_dep1_term : opname -> term -> bool;;
val mk_dep1_term : opname -> string -> term -> term;;
val dest_dep1_term : opname -> term -> string * term;;

val is_dep0_dep1_term : opname -> term -> bool;;
val mk_dep0_dep1_term : opname -> string -> term -> term -> term;;
val dest_dep0_dep1_term : opname -> term -> string * term * term;;

val is_dep0_dep2_term : opname -> term -> bool;;
val mk_dep0_dep2_term : opname -> string -> string -> term -> term -> term;;
val dest_dep0_dep2_term : opname -> term -> string * string * term * term;;

val is_dep2_dep0_term : opname -> term -> bool;;
val mk_dep2_dep0_term : opname -> string -> string -> term -> term -> term;;
val dest_dep2_dep0_term : opname -> term -> string * string * term * term;;

val is_dep0_dep1_dep1_term : opname -> term -> bool;;
val mk_dep0_dep1_dep1_term : opname -> term -> string -> term -> string -> term -> term;;
val dest_dep0_dep1_dep1_term : opname -> term -> term * string * term * string * term;;

val is_dep0_dep2_dep2_term : opname -> term -> bool;;
val mk_dep0_dep2_dep2_term : opname -> term -> string -> string -> term -> string -> string -> term -> term;;
val dest_dep0_dep2_dep2_term : opname -> term -> term * string * string * term * string * string * term;;

val is_dep0_dep2_dep0_dep2_term : opname -> term -> bool;;
val mk_dep0_dep2_dep0_dep2_term : opname -> term -> string -> string -> term -> term -> string -> string -> term -> term;;
val dest_dep0_dep2_dep0_dep2_term : opname -> term -> term * string * string * term * term * string * string * term;;

val is_dep0_dep0_dep3_term : opname -> term -> bool;;
val mk_dep0_dep0_dep3_term : opname -> term -> term -> string -> string -> string -> term -> term;;
val dest_dep0_dep0_dep3_term : opname -> term -> term * term * string * string * string * term;;

val is_string_dep0_term : opname -> term -> bool;;
val mk_string_dep0_term : opname -> string -> term -> term;;
val dest_string_dep0_term : opname -> term -> string * term;;

val is_number_term : opname -> term -> bool;;
val mk_number_term : opname -> int -> term;;
val dest_number_term : opname -> term -> int;;

val is_univ_term : opname -> term -> bool;;
val mk_univ_term : opname -> levelExp -> term;;
val dest_univ_term : opname -> term -> levelExp;;

val is_token_term : opname -> term -> bool;;
val mk_token_term : opname -> string -> term;;
val dest_token_term : opname -> term -> string;;

(************************************************************************
 * Operations                                                           *
 ************************************************************************)

(*
 * Subterm addressing.
 *)
exception IncorrectAddress of address * term;;
exception BadAddressPrefix of address * address;;

val term_subterm :  term -> address -> term;;
val replace_subterm : term -> address -> term -> term;;
val apply_fun_at_addr : (term -> term) -> address -> (term -> term);;
val remove_addr_prefix : address -> address -> address;;
val subterm_arities : term -> int list;;

(*
 * Term operations.
 * subst: simultaneous subst of terms for vars.
 * var_subst: subst of var for a term.
 *)
val subst : term -> term list -> string list -> term;;
val var_subst : term -> term -> string -> term;;
val alpha_equal : term -> term -> bool;;
val alpha_equal_vars : (term * string list) -> (term * string list) -> bool;;
val alpha_equal_match : (term * string list) ->
       (term * string list * string list * term list) ->
       bool;;

(*
 * Rename the binding variables so they are all distinct.
 *)
val standardize_apart : term -> term;;

(*
 * Get the list of free variables.
 *)
val is_free_var : string -> term -> bool;;
val free_vars : term -> string list;;
val free_vars_terms : term list -> string list;;
val context_vars : term -> string list;;
val binding_vars : term -> string list;;

(*
 * Comparisons.
 *)
exception BadParamMatch of param * param;;

val compare_level_exps : levelExp -> levelExp -> int;;
val compare_params : param -> param -> int;;
val compare_pattern_params : param -> param -> int;;
val compare_operators : operator -> operator -> int;;
val compare_pattern_operators : operator -> operator -> int;;

type term_subst = (string * term) list;;

exception BadMatch of term * term;;

val unify : term_subst -> term -> term -> term_subst;;

(************************************************************************
 * Simplified operations on manifest terms                              *
 ************************************************************************)

(* Level expression operations *)
val mk_const_level_exp : int -> levelExp;;
val mk_var_level_exp : string -> levelExp;;
val incr_level_exp : levelExp -> levelExp;;
val max_level_exp : levelExp -> levelExp -> levelExp;;

val level_cumulativity : levelExp -> levelExp -> bool;;

(*
 * A variable is a term with opname "var", and a single
 * var parameter that is the name of the variable.
 *)
val is_var_term : term -> bool;;
val dest_var : term -> string;;
val mk_var_term : string -> term;;
val mk_var_op : string -> operator;;

val is_so_var_term : term -> bool;;
val dest_so_var : term -> string * term list;;
val mk_so_var_term : string -> term list -> term;;

val is_context_term : term -> bool;;
val dest_context : term -> string * term * term list;;
val mk_context_term : string -> term -> term list -> term;;

(*
 * Sequents.
 * This should be visible only to sequents, but oh well.
 *)
val is_sequent_term : term -> bool;;

val is_hyp_term : term -> bool;;
val dest_hyp : term -> string * term * term;;
val mk_hyp_term : string -> term -> term -> term;;

val is_concl_term : term -> bool;;
val dest_concl : term -> term * term;;
val mk_concl_term : term -> term -> term;;
val null_concl : term;;

val is_sequent_term : term -> bool;;
val dest_sequent : term -> term list;;
val goal_of_sequent : term -> term;;
val mk_sequent_term : term list -> term;;

val nth_hyp : term -> int -> string * term;;
val nth_concl : term -> int -> term;;
val num_hyps : term -> int;;
val declared_vars : term -> string list;;
val declarations : term -> (string * term) list;;
val get_decl_number : term -> string -> int;;
val is_free_seq_var : int -> string -> term -> bool;;

val concl_addr : term -> int * int;;
val mk_seq_subgoals : term -> term list -> term list;;

(*
 * Primitive lists.
 *)
val is_xnil_term : term -> bool;;
val xnil_term : term;;

val is_xcons_term : term -> bool;;
val mk_xcons_term : term -> term -> term;;
val dest_xcons : term -> term * term;;

val is_xlist_term : term -> bool;;
val dest_xlist : term -> term list;;
val mk_xlist_term : term list -> term;;

(*
 * Primitive strings.
 *)
val is_xstring_term : term -> bool;;
val mk_xstring_term : string -> term;;
val dest_xstring : term -> string;;

(*
 * Primitive abstractions.
 *)
val mk_xlambda_term : string -> term -> term;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
