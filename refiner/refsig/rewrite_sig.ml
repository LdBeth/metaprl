(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
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
 *
 *)

module type RewriteSig =
sig
   (* Import the term types *)
   type term
   type level_exp
   type operator
   type address

   (* Packaged rewrite rule *)
   type rewrite_rule

   (* Separated forms *)
   type rewrite_redex
   type rewrite_contractum
   type rewrite_stack

   (*
    * Types for redex matching.
    *)
   type rewrite_type =
      RewriteTermType of string
    | RewriteFunType of string
    | RewriteContextType of string
    | RewriteStringType of string
    | RewriteIntType of string
    | RewriteLevelType of string

   type rewrite_item =
      RewriteTerm of term
    | RewriteFun of (term list -> term)
    | RewriteContext of (term -> term list -> term)
    | RewriteString of string
    | RewriteInt of int
    | RewriteLevel of level_exp

   (*
    * Separate analysis.
    *)
   val compile_redex : string array -> term -> rewrite_redex
   val compile_redices : string array -> term list -> rewrite_redex
   val compile_contractum : rewrite_redex -> term -> rewrite_contractum
   val extract_redex_types : rewrite_redex -> rewrite_type list
   val apply_redex :
      rewrite_redex -> address array ->
      term -> term list -> rewrite_stack
   val apply_redex' :
      rewrite_redex -> address array ->
      term -> term list -> rewrite_stack * rewrite_item list
   val make_contractum : rewrite_contractum -> rewrite_stack -> term

   (* Rewrite constructor/destructors *)
   val term_rewrite : string array * string array ->
      term list -> term list -> rewrite_rule
   val fun_rewrite : term -> (term -> term) -> rewrite_rule

   (* Apply a rewrite to a term *)
   val apply_rewrite : rewrite_rule -> address array * string array * string list list ->
      term -> term list -> term list * string array

   (*
    * See if a rule may apply to a particular term
    * described by its operator and it arities.
    *)
   val relevant_rule : operator -> int list -> rewrite_rule -> bool

   (*
    * Get some info for the evaluator.
    *)
   val rewrite_operator : rewrite_rule -> operator
   val rewrite_eval_flags : rewrite_rule -> (int * bool) list
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

