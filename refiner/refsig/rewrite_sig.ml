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
open Lm_symbol

open Opname

type out_channel = Lm_printf.out_channel

(* Names of the sequent and regular contexts to be passed as arguments *)
type rewrite_args_spec = {
   spec_ints: var array;
   spec_addrs: var array
}

(* Sizes (+1) for sequent contexts, bound variables *)
(* Non-positive sizes mean counting the hyps to skip from the end *)
type 'addr rw_args_poly = {
   arg_ints: int array;
   arg_addrs: 'addr array
}

(*
 * In "Strict" mode the rewriter should behave as described in the
 * "Sequent Schemata" paper @cite[NH02],@cite[Section 3]{Nog02}
 * In "Strict" mode the rewriter is guaranteed to always preserve
 * the binding structure on the terms, no matter what rules are being
 * applied.
 * Note - when the rewriter takes a term list as inputs, only the first element
 * of the list (the "real" redex) is taken to be fully Strict, the rest of the
 * list (rule/rewrite "parameters") are allowed to have variables that are going
 * to be captured.
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

(*
 * Types for redex matching.
 *)
type rewrite_type =
   RewriteTermType
 | RewriteFunType
 | RewriteContextType
 | RewriteStringType
 | RewriteNumType
 | RewriteTokenType
 | RewriteLevelType
 | RewriteVarType (* corresponds to RewriteString(RewriteMetaParam _) *)

type 'a rewrite_param =
   RewriteParam of 'a
 | RewriteMetaParam of var

module type RwTypesSig =
sig
   include Term_sig.TermSig
   type address
end

module type RewriteSig =
sig
   module RwTypes : RwTypesSig
   open RwTypes

   type rw_args = address rw_args_poly
   type rewrite_args = rw_args * SymbolSet.t

   (* Packaged rewrite rule *)
   type rewrite_rule

   (* Separated form *)
   type rewrite_redex

   type rewrite_item =
      RewriteTerm of term
    | RewriteFun of (term list -> term)
    | RewriteContext of (term -> term list -> term)
    | RewriteString of string rewrite_param
    | RewriteToken of opname rewrite_param
    | RewriteNum of Lm_num.num rewrite_param
    | RewriteLevel of level_exp

   (* Rewrites with no arguments *)
   val empty_args_spec : rewrite_args_spec
   val empty_rw_args : rw_args
   val empty_args : rewrite_args

   (*
    * Separate analysis.
    *)
   val compile_redex : strict -> rewrite_args_spec -> term -> rewrite_redex
   val compile_redices : strict -> rewrite_args_spec -> term list -> rewrite_redex
   val extract_redex_types : rewrite_redex -> (rewrite_type * var) list
   val test_redex_applicability :
      rewrite_redex -> rw_args ->
      term -> term list -> unit
   val apply_redex :
      rewrite_redex -> rw_args ->
      term -> term list -> rewrite_item list

   (* Rewrite constructor/destructors *)
   val term_rewrite : strict -> rewrite_args_spec -> term list -> term list -> rewrite_rule
   val fun_rewrite : strict -> term -> (term -> term) -> rewrite_rule

   (* Apply a rewrite to a term *)
   val apply_rewrite :
      rewrite_rule -> (* rule *)
      rewrite_args -> (* contexts, bound variables *)
      term ->         (* redex *)
      term list ->    (* parameters *)
      term list       (* contracta *)

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

   (*
    * For debugging.
    *)
   val print_rewrite_redex : out_channel -> rewrite_redex -> unit
   val print_rewrite_rule : out_channel -> rewrite_rule -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

