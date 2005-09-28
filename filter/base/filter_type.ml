(*
 * Types used by filter machine.
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_symbol

open Term_sig
open Term_shape_sig
open Term_ty_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermTy
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
 * Module paths, but empty paths are not allowed.
 *)
exception EmptyModulePath of string

type module_path = string list

(*
 * Annotated terms may have a name.
 *)
type 'term poly_aterm = { aname : 'term option; aterm : 'term }
type aterm = term poly_aterm

(************************************************************************
 * A "bytecode" description of how to construct a term                  *
 ************************************************************************)

type 'expr param_constr =
   ConPStr of string
 | ConPMeta of var
 | ConPNum of Lm_num.num
 | ConPInt of 'expr
 | ConPExpr of 'expr
 | ConPToken of opname

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

type ('item, 'term) bnd_expr =
   { item_bindings : (string * 'term prl_binding) list;
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
   IntParam of var
 | AddrParam of var
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

type 'term iform_info =
   { iform_name : string;
     iform_redex : 'term;
     iform_contractum : 'term
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

(*
 * A parent command lists all the modules that are recursively
 * opened, and it lists all the resources that were discovered.
 *)
type 'ctyp parent_info =
   { parent_name : module_path;
     parent_resources : (string * 'ctyp resource_sig) list
   }

type ('term, 'expr) term_def =
   { term_def_name : string;
     term_def_value : 'term;
     term_def_resources : ('expr, 'term) resource_def
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

type typeclass_parent =
   ParentNone
 | ParentExtends of opname
 | ParentInclude of opname

(*
 * Declarations have multiple classes.
 *)
type shape_class =
   ShapeNormal
 | ShapeIForm

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
 | InputForm   of 'term iform_info
 | PRLGrammar  of Filter_grammar.t

   (* JYH: added opname classes 2004/01/16 *)
 | DeclareTypeClass   of shape_class * opname * opname * typeclass_parent
 | DeclareType        of shape_class * ('term, 'term) poly_ty_term * opname
 | DeclareTerm        of shape_class * ('term, 'term) poly_ty_term
 | DefineTerm         of shape_class * ('term, 'term) poly_ty_term * ('term, 'expr) term_def
 | DeclareTypeRewrite of 'term * 'term
(* %%MAGICEND%% *)

(*
 * These type define what info do we need during parsing to identify opnames and context bindings
 * The context_fun should return the Some list when the SO variable bindings are known from the proof context
 *)
type opname_kind_fun        = op_kind -> string list -> shape_param list -> int list -> Opname.opname
type context_fun            = var -> int -> var list option
type infer_term_fun         = term -> term
type check_rule_fun         = meta_term -> term list -> unit
type check_rewrite_fun      = meta_term -> term list -> unit
type check_type_rewrite_fun = term -> term -> unit
type check_dform_fun        = term -> term -> unit
type check_iform_fun        = meta_term -> unit
type check_production_fun   = term list -> term -> unit

type quotation_expander     = Filter_grammar.quotation_expander
type check_input_term_fun   = MLast.loc -> term -> unit
type check_input_mterm_fun  = MLast.loc -> meta_term -> unit

type apply_iforms_fun       = MLast.loc -> quotation_expander -> term -> term
type apply_iforms_mterm_fun = MLast.loc -> quotation_expander -> meta_term -> term list -> meta_term * term list
type term_of_string_fun     = MLast.loc -> quotation_expander -> string -> string -> term

(*
 * Grammars to extend.
 *)
module type TermGrammarSig =
sig
   (* Provided by environments *)
   val opname_prefix      : MLast.loc -> opname
   val mk_opname_kind     : MLast.loc -> opname_kind_fun
   val mk_var_contexts    : MLast.loc -> context_fun
   val infer_term         : MLast.loc -> infer_term_fun
   val check_rule         : MLast.loc -> check_rule_fun
   val check_rewrite      : MLast.loc -> check_rewrite_fun
   val check_type_rewrite : MLast.loc -> check_type_rewrite_fun
   val check_dform        : MLast.loc -> check_dform_fun
   val check_iform        : MLast.loc -> check_iform_fun
   val check_production   : MLast.loc -> check_production_fun

   (* Filter_grammar *)
   val check_input_term   : check_input_term_fun
   val check_input_mterm  : check_input_mterm_fun

   val apply_iforms       : apply_iforms_fun
   val apply_iforms_mterm : apply_iforms_mterm_fun
   val term_of_string     : term_of_string_fun

   (* Grammar *)
   val opname            : opname Grammar.Entry.e
   val opname_name       : string Grammar.Entry.e
   val term_eoi          : term Grammar.Entry.e
   val term              : term Grammar.Entry.e
   val quote_term        : ty_term Grammar.Entry.e
   val mterm             : term poly_meta_term Grammar.Entry.e
   val bmterm            : term poly_meta_term Grammar.Entry.e
   val singleterm        : term poly_aterm Grammar.Entry.e
   val xdform            : term Grammar.Entry.e
   val term_con_eoi      : (term, MLast.expr) term_constructor Grammar.Entry.e

   val parsed_term       : term Grammar.Entry.e
   val parsed_bound_term : aterm Grammar.Entry.e
end

(*
 * Grammars to extend.
 *)
module type ParsedTermGrammarSig =
sig
   (* The results have abstract types *)
   type parsed_term
   type parsed_bound_term
   type parsed_meta_term = parsed_term poly_meta_term

   (* Quotation access *)
   val dest_quot       : string -> string * string
   val parse_quotation : MLast.loc -> string -> string -> string -> parsed_term
   val convert_comment : MLast.loc -> term -> term

   (* Term conversion *)
   val parse_term           : MLast.loc -> parsed_term -> term
   val parse_term_with_vars : MLast.loc -> parsed_term -> term
   val parse_rule           : MLast.loc -> string -> parsed_meta_term -> parsed_term list -> meta_term * term list * (term -> term)
   val parse_rewrite        : MLast.loc -> string -> parsed_meta_term -> parsed_term list -> meta_term * term list * (term -> term)
   val parse_type_rewrite   : MLast.loc -> parsed_term -> parsed_term -> term * term
   val parse_iform          : MLast.loc -> string -> parsed_meta_term -> meta_term
   val parse_dform          : MLast.loc -> parsed_term -> parsed_term -> term * term
   val parse_production     : MLast.loc -> parsed_term list -> parsed_term -> term list * term
   val parse_define         : MLast.loc -> string -> parsed_term -> parsed_term -> term * term

   (* For input terms from other parsers *)
   val mk_parsed_term : term -> parsed_term

   (* Grammar *)
   val opname            : opname Grammar.Entry.e
   val opname_name       : string Grammar.Entry.e
   val term_eoi          : parsed_term Grammar.Entry.e
   val term              : parsed_term Grammar.Entry.e
   val quote_term        : (parsed_term, parsed_term) poly_ty_term Grammar.Entry.e
   val mterm             : parsed_term poly_meta_term Grammar.Entry.e
   val bmterm            : parsed_term poly_meta_term Grammar.Entry.e
   val singleterm        : parsed_term poly_aterm Grammar.Entry.e
   val xdform            : parsed_term Grammar.Entry.e
   val term_con_eoi      : (parsed_term, MLast.expr) term_constructor Grammar.Entry.e

   (* Parsed versions *)
   val parsed_term       : term Grammar.Entry.e
   val parsed_bound_term : term poly_aterm Grammar.Entry.e

   (************************************************
    * !!! WARNING, UNSAFE !!!
    * !!! The following functions bypass
    * !!! either the parser or the type checker.
    *)

   (* Bypass everything: the iforms, the parser, and the type checker *)
   val raw_term_of_parsed_term : parsed_term -> term

   (* Bypass both the parser and the type checker *)
   val raw_input_term_of_parsed_term : parsed_term -> term

   (* Bypass the parser, but do type checking *)
   val unparsed_term_of_parsed_term : MLast.loc -> parsed_term -> term

   (* Bypass the type checker, but do parsing *)
   val unchecked_term_of_parsed_term : parsed_term -> term

   (* Bypass the type checker, the iforms, but do parsing *)
   val quoted_term_of_parsed_term : MLast.loc -> parsed_term -> term

   (* Bypass the parser, but do typechecking *)
   val parse_quoted_term : MLast.loc ->
      term ->           (* token type *)
      term ->           (* bvar type *)
      term ->           (* term type *)
      parsed_term ->
      term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
