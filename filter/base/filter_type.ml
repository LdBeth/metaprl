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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta

(************************************************************************
 * COMMON ERRORS                                                        *
 ************************************************************************)

(*
 * Input exceptions.
 *)
exception NotANumber of string         (* Int param is not a number *)
exception BadParam of string           (* Bogus parameter format *)
exception BadLevelExp of level_exp     (* Level expression has wrong type *)
exception BadParamCast of param * string
exception BadArgList of (string list * term) list
exception BadBinder of term
exception ParseError of string

(*
 * Generic exception.
 *)
exception BadCommand of string

(************************************************************************
 * COMMON TYPES                                                         *
 ************************************************************************)

(*
 * A "quoted" term is given in exploded form.
 *)
type quote_term = string * Term.param list * bound_term list

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
 * Summary (filtyer_summary) TYPES                                      *
 ************************************************************************)

(*
 * 'proof: type of proofs for rewrites, etc.
 * 'ctyp: type of type definitions for resources
 * 'expr: type of expressions used in ML definitions
 * 'item: type of summary items
 *)

type 'expr resource_def = (MLast.loc * string * 'expr list) list

(*
 * Proof is type unit in interface.
 *)
type ('term, 'proof, 'expr) rewrite_info =
   { rw_name : string;
     rw_redex : 'term;
     rw_contractum : 'term;
     rw_proof : 'proof;
     rw_resources : 'expr resource_def
   }

type ('term, 'proof, 'expr) axiom_info =
   { axiom_name : string;
     axiom_stmt : 'term;
     axiom_proof : 'proof;
     axiom_resources : 'expr resource_def
   }

type ('term, 'proof, 'expr) cond_rewrite_info =
   { crw_name : string;
     crw_params : 'term param list;
     crw_args : 'term list;
     crw_redex : 'term;
     crw_contractum : 'term;
     crw_proof : 'proof;
     crw_resources : 'expr resource_def
   }

and ('term, 'meta_term, 'proof, 'expr) rule_info =
   { rule_name : string;
     rule_params : 'term param list;
     rule_stmt : 'meta_term;
     rule_proof : 'proof;
     rule_resources : 'expr resource_def
   }

and ('term, 'expr) mlterm_info =
   { mlterm_name : string;
     mlterm_params : 'term param list;
     mlterm_term : 'term;
     mlterm_contracta : 'term list;
     mlterm_def : 'expr option;
     mlterm_resources : 'expr resource_def
   }

(*
 * A parent command lists all the modules that are recursively
 * opened, and it lists all the resources that were discovered.
 *)
and 'ctyp parent_info =
   { parent_name : module_path;
     parent_opens : module_path list;
     parent_resources : 'ctyp resource_info list
   }

and 'term opname_info =
   { opname_name : string;
     opname_term : 'term
   }

and dform_modes =
   Modes of string list       (* include these modes *)
 | ExceptModes of string list (* exclude these modes *)
 | AllModes

(*
 * Dform descriptions.
 * The definition is not required in the interface.
 *)
and ('term, 'expr) dform_info =
   { dform_name : string;
     dform_modes : dform_modes;
     dform_options : dform_option list;
     dform_redex : 'term;
     dform_def : ('term, 'expr) dform_def
   }

and dform_option =
   DFormInheritPrec
 | DFormPrec of string
 | DFormParens
 | DFormInternal

and ('term, 'expr) dform_def =
   NoDForm
 | TermDForm of 'term
 | MLDForm of ('term, 'expr) dform_ml_def

and ('term, 'expr) dform_ml_def =
   { dform_ml_printer : string;
     dform_ml_buffer : string;
     dform_ml_contracta : 'term list;
     dform_ml_code : 'expr
   }

(*
 * Define a precedence relation.
 *)
and prec_rel_info =
   { prec_rel : Precedence.relation;
     prec_left : string;
     prec_right : string
   }

(*
 * Resource descriptions.
 *)
and 'ctyp resource_info =
   { resource_name : string;
     resource_extract_type : 'ctyp;
     resource_improve_type : 'ctyp;
     resource_data_type : 'ctyp;
     resource_arg_type : 'ctyp
   }

(*
 * Magic block needs a variable to bind the magic number to.
 *)
and 'item magic_info =
   { magic_name : string;
     magic_code : 'item list
   }

and 'term param =
   ContextParam of string
 | VarParam of string
 | TermParam of 'term

(*
 * Conversion functions.
 *)
type ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1,
      'term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) convert =
   { term_f       : 'term1 -> 'term2;
     meta_term_f  : 'meta_term1 -> 'meta_term2;
     proof_f      : string -> 'proof1 -> 'proof2;
     ctyp_f       : 'ctyp1  -> 'ctyp2;
     expr_f       : 'expr1  -> 'expr2;
     item_f       : 'item1  -> 'item2
   }

(*
 * The summary contains information about everything in the file.
 * We use the summary for both interfaces and implementations.
 * If the implementation requires a term for the definition,
 * we use a term option so that the term does not have to be
 * provided in the interface.
 *
 * A MagicBlock is a block of code that we use to compute
 * a magic number.  The magic number changes whenever the code changes.
 *)

type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item, 'module_info) summary_item_type =
   Rewrite of ('term, 'proof, 'expr) rewrite_info
 | CondRewrite of ('term, 'proof, 'expr) cond_rewrite_info
 | Axiom of ('term, 'proof, 'expr) axiom_info
 | Rule of ('term, 'meta_term, 'proof, 'expr) rule_info
 | Opname of 'term opname_info
 | MLRewrite of ('term, 'expr) mlterm_info
 | MLAxiom of ('term, 'expr) mlterm_info
 | Parent of 'ctyp parent_info
 | Module of string * 'module_info
 | DForm of ('term, 'expr) dform_info
 | Prec of string
 | PrecRel of prec_rel_info
 | Id of int
 | Resource of 'ctyp resource_info
 | Infix of string
 | SummaryItem of 'item
 | ToploopItem of 'item
 | MagicBlock of 'item magic_info
 | Comment of 'term

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
