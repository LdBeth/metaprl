(*
 * Types used by filter machine.
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_symbol

open Term_shape_sig
open Refiner.Refiner
open TermType
open TermMeta
open Opname
open Dform

(************************************************************************
 * COMMON ERRORS                                                        *
 ************************************************************************)

(*
 * Input exceptions.
 *)
exception BadParamCast of param * string
exception ParseError of string

(*
 * Generic exception.
 *)
exception BadCommand of string

(*
 * .ml does not match its .mli
 *)
exception IterfImplemMismatch of string

(*
 * OMakefile instructs omake to watch this file for changes and warn
 * that filter_magic.ml may need to be updated when the types in this file
 * change. _Do not delete_ the following marker.
 * %%MAGICBEGIN%%
 *)

(************************************************************************
 * COMMON TYPES                                                         *
 ************************************************************************)

(*
 * A "quoted" term is given in exploded form.
 *)
type quote_term = string * param list * bound_term list

(*
 * Module paths, but empty paths are not allowed.
 *)
exception EmptyModulePath of string

type module_path = string list

(*
 * Annotated terms may have a name.
 *)
type aterm = { aname : term option; aterm : term }

(************************************************************************
 * A "bytecode" description of how to construct a term                  *
 ************************************************************************)

type 'expr param_constr =
   ConPStr of string
 | ConPMeta of var
 | ConPNum of Lm_num.num
 | ConPInt of 'expr
 | ConPExpr of 'expr

type 'expr param_constructor = 'expr param_constr * shape_param

type 'expr bvar_constructor =
   ConBVarConst of string
 | ConBVarExpr of 'expr

type ('term, 'expr) term_constructor =
   ConTerm of 'term     (* a constant term *)
 | ConExpr of 'expr
 | ConVar of 'expr
 | ConConstruct of opname * 'expr param_constructor list * ('term, 'expr) bterm_constructor list
 | ConSequent of ('term, 'expr) term_constructor * ('term, 'expr) hyp_constructor list * ('term, 'expr) term_constructor

and ('term, 'expr) bterm_constructor =
   'expr bvar_constructor list * ('term, 'expr) term_constructor

and ('term, 'expr) hyp_constructor =
   ConContext of 'expr * ('term, 'expr) term_constructor list
 | ConHypList of 'expr
 | ConHypothesis of 'expr * ('term, 'expr) term_constructor

(************************************************************************
 * Summary (filter_summary) TYPES                                      *
 ************************************************************************)

(*
 * 'proof: type of proofs for rewrites, etc.
 * 'ctyp: type of type definitions for resources
 * 'expr: type of bnd_expressions used in ML definitions
 * 'item: type of summary items
 *)

(*
 * A str_item can have MetaPRL stuff in it.
 *)
type 'term prl_binding =
   BindTerm of 'term
 | BindOpname of opname
 | BindNum of Lm_num.num

type ('item, 'term) bnd_expr = {
   item_bindings : (string * 'term prl_binding) list;
   item_item : 'item
}

type ('expr, 'term) resource_def =
   ((MLast.loc * string * 'expr list) list, 'term) bnd_expr

(*
 * Resource descriptions.
 *)
type 'ctyp resource_sig = {
   resource_input : 'ctyp;
   resource_output : 'ctyp
}

type ('ctyp, 'expr) resource_str = {
   res_input : 'ctyp;
   res_output : 'ctyp;
   res_body : 'expr;
}

type 'term param =
   ContextParam of var
 | TermParam of 'term

(*
 * Proof is type unit in interface.
 *)
type ('term, 'proof, 'expr) rewrite_info =
   { rw_name : string;
     rw_redex : 'term;
     rw_contractum : 'term;
     rw_proof : 'proof;
     rw_resources : ('expr, 'term) resource_def
   }

type ('term, 'proof, 'expr) cond_rewrite_info =
   { crw_name : string;
     crw_params : 'term param list;
     crw_args : 'term list;
     crw_redex : 'term;
     crw_contractum : 'term;
     crw_proof : 'proof;
     crw_resources : ('expr, 'term) resource_def
   }

type ('term, 'meta_term, 'proof, 'expr) rule_info =
   { rule_name : string;
     rule_params : 'term param list;
     rule_stmt : 'meta_term;
     rule_proof : 'proof;
     rule_resources : ('expr, 'term) resource_def
   }

type ('term, 'expr) mlterm_info =
   { mlterm_name : string;
     mlterm_params : 'term param list;
     mlterm_term : 'term;
     mlterm_def : ('expr, 'term) bnd_expr option;
     mlterm_resources : ('expr, 'term) resource_def
   }

type 'term opname_info =
   { opname_name : string;
     opname_term : 'term
   }

(*
 * A parent command lists all the modules that are recursively
 * opened, and it lists all the resources that were discovered.
 *)
type 'ctyp parent_info =
   { parent_name : module_path;
     parent_resources : (string * 'ctyp resource_sig) list
   }

type ('term, 'expr) opname_definition =
   { opdef_name : string;
     opdef_opname : string;
     opdef_term : 'term;
     opdef_definition : 'term;
     opdef_resources : ('expr, 'term) resource_def
   }

type dform_option =
   DFormInheritPrec
 | DFormPrec of string
 | DFormParens

type ('term, 'expr) dform_ml_def =
   { dform_ml_printer : string;
     dform_ml_buffer : string;
     dform_ml_code : ('expr, 'term) bnd_expr;
   }

type ('term, 'expr) dform_def =
   NoDForm
 | TermDForm of 'term
 | MLDForm of ('term, 'expr) dform_ml_def

(*
 * Dform descriptions.
 * The definition is not required in the interface.
 *)
type ('term, 'expr) dform_info =
   { dform_name : string;
     dform_modes : dform_modes;
     dform_options : dform_option list;
     dform_redex : 'term;
     dform_def : ('term, 'expr) dform_def
   }

(*
 * Define a precedence relation.
 *)
type prec_rel_info =
   { prec_rel : Precedence.relation;
     prec_left : string;
     prec_right : string
   }

type 'expr resource_expr = 'expr

(*
 * Reource improvement.
 *)
type ('expr, 'term) improve_info = {
   improve_name : string;
   improve_expr : ('expr, 'term) bnd_expr
}

(*
 * Magic block needs a variable to bind the magic number to.
 *)
type 'item magic_info =
   { magic_name : string;
     magic_code : 'item list
   }

(*
 * Conversion functions.
 *)
type ('term1, 'meta_term1, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1,
      'term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) convert =
   { term_f       : 'term1 -> 'term2;
     meta_term_f  : 'meta_term1 -> 'meta_term2;
     proof_f      : string -> 'proof1 -> 'proof2;
     resource_f   : 'resource1 -> 'resource2;
     ctyp_f       : 'ctyp1  -> 'ctyp2;
     expr_f       : 'expr1  -> 'expr2;
     item_f       : 'item1  -> 'item2
   }

type grammar_update =
   Infix of string
 | Suffix of string

(*
 * The summary contains information about everything in the file.
 * We use the summary for both interfaces and implementations.
 * If the implementation requires a term for the definition,
 * we use a term option so that the term does not have to be
 * provided in the interface.
 *
 * A MagicBlock (not currently used!) is a block of code that we use to compute
 * a magic number.  The magic number changes whenever the code changes.
 *)
type ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item, 'module_info) summary_item_type =
   Rewrite     of ('term, 'proof, 'expr) rewrite_info
 | CondRewrite of ('term, 'proof, 'expr) cond_rewrite_info
 | Rule        of ('term, 'meta_term, 'proof, 'expr) rule_info
 | Opname      of 'term opname_info
 | MLRewrite   of ('term, 'expr) mlterm_info
 | MLAxiom     of ('term, 'expr) mlterm_info
 | Parent      of 'ctyp parent_info
 | Module      of string * 'module_info
 | DForm       of ('term, 'expr) dform_info
 | Prec        of string
 | PrecRel     of prec_rel_info
 | Id          of int
 | Resource    of string * 'resource
 | Improve     of ('expr, 'term) improve_info
 | MLGramUpd   of grammar_update
 | SummaryItem of ('item, 'term) bnd_expr
 | ToploopItem of 'item
 | MagicBlock  of 'item magic_info
 | Comment     of 'term
 | InputForm   of ('term, 'proof, 'expr) rewrite_info
 | Definition  of ('term, 'expr) opname_definition
 | PRLGrammar  of Filter_grammar.t

(* %%MAGICEND%% *)

(*
 * These type define what info do we need during parsing to identify opnames and context bindings
 * The context_fun should return the Some list when the SO variable bindings are known from the proof context
 *)
type opname_fun = string list -> shape_param list -> int list -> Opname.opname
type context_fun = var -> int -> var list option

(*
 * Grammars to extend.
 *)
module type TermGrammarSig =
sig
   val mk_opname         : MLast.loc -> opname_fun
   val mk_var_contexts   : MLast.loc -> context_fun
   val term_eoi          : term Grammar.Entry.e
   val term              : term Grammar.Entry.e
   val parsed_term       : term Grammar.Entry.e
   val quote_term        : quote_term Grammar.Entry.e
   val mterm             : meta_term Grammar.Entry.e
   val bmterm            : meta_term Grammar.Entry.e
   val singleterm        : aterm Grammar.Entry.e
   val parsed_bound_term : aterm Grammar.Entry.e
   val xdform            : term Grammar.Entry.e
   val term_con_eoi      : (term, MLast.expr) term_constructor Grammar.Entry.e
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
