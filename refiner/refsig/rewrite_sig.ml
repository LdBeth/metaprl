(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open String_set

module type RewriteSig =
sig
   (* Import the term types *)
   type term
   type level_exp
   type operator
   type address

   (* Packaged rewrite rule *)
   type rewrite_rule

   (* Separated form *)
   type rewrite_redex

   (*
    * Types for redex matching.
    *)
   type rewrite_type =
      RewriteTermType of string
    | RewriteFunType of string
    | RewriteContextType of string
    | RewriteStringType of string
    | RewriteNumType of string
    | RewriteLevelType of string

   type rewrite_item =
      RewriteTerm of term
    | RewriteFun of (term list -> term)
    | RewriteContext of (term -> term list -> term)
    | RewriteString of string
    | RewriteNum of Mp_num.num
    | RewriteLevel of level_exp

   (* Names of the contexts to be passed as arguments *)
   type rewrite_args_spec = string array

   (* Sizes (+1) for sequent contexts, strings for new variable names, bound variables *)
   (* Non-positive sizes mean counting the hyps to skip from the end *)
   type rewrite_args = int array * StringSet.t

   (*
    * In "Strict" mode the rewriter should behave as described in the
    * "Sequent Schemata" paper @cite[NH02],@cite[Section 3]{Nog02}
    * In "Strict" mode the rewriter is guaranteed to always preserve
    * the binding structure on the terms, no matter what rules are being
    * applied.
    * (XXX Note: right now it does not behave quite that way, but it's
    * a bug).
    *
    * The "Relaxed" mode is a hack that does not have an exact semantics.
    *
    * Basically, "Strict" mode means that whenever a pattern specifies a binding
    * occurence explicitly, all the bound occurrences have to be also explicitly
    * specified. In "Strict" mode, <<lambda{x.'t}>> would only match a term
    * where "t" does not have * free instances of "x". In "Relaxed" mode there
    * is no such restriction.
    *
    * By default, the rules and rewrites are compiled in "Strict" mode
    * and display forms - in "Relaxed" mode
    *)
   type strict = Strict | Relaxed

   (* Rewrites with no arguments *)
   val empty_args_spec : rewrite_args_spec
   val empty_args : rewrite_args

   (*
    * Separate analysis.
    *)
   val compile_redex : strict -> string array -> term -> rewrite_redex
   val compile_redices : strict -> string array -> term list -> rewrite_redex
   val extract_redex_types : rewrite_redex -> rewrite_type list
   val test_redex_applicability :
      rewrite_redex -> int array ->
      term -> term list -> unit
   val apply_redex :
      rewrite_redex -> int array ->
      term -> term list -> rewrite_item list

   (* Rewrite constructor/destructors *)
   val term_rewrite : strict -> rewrite_args_spec ->
      term list -> term list -> rewrite_rule
   val fun_rewrite : strict -> term -> (term -> term) -> rewrite_rule

   (* Apply a rewrite to a term *)
   val apply_rewrite :
      rewrite_rule -> (* rule *)
      rewrite_args -> (* contexts, variable names, bound variables *)
      term ->         (* redex *)
      term list ->    (* parameters *)
      term list       (* contracta, actual variable names that were matched *)

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

