(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
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
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

(************************************************************************
 * TYPES								*
 ************************************************************************)

(*
 * A module_base contains information about a collection of modules.
 * A module_info contains information about a specific module.
 *
 * 'proof: type of proofs for rewrites, etc.
 * 'ctyp: type of type definitions for resources
 * 'expr: type of expressions used in ML definitions
 * 'item: type of summary items
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

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
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item =
   Rewrite of ('term, 'proof) rewrite_info
 | CondRewrite of ('term, 'proof) cond_rewrite_info
 | Axiom of ('term, 'proof) axiom_info
 | Rule of ('term, 'meta_term, 'proof) rule_info
 | Opname of 'term opname_info
 | MLTerm of ('term, 'expr) mlterm_info
 | Condition of ('term, 'expr) mlterm_info
 | Parent of 'ctyp parent_info
 | Module of string * ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info
 | DForm of ('term, 'expr) dform_info
 | Prec of string
 | PrecRel of prec_rel_info
 | Id of int
 | Resource of 'ctyp resource_info
 | Infix of string
 | SummaryItem of 'item
 | ToploopItem of 'item
 | MagicBlock of 'item magic_info

(*
 * Proof is type unit in interface.
 *)
and ('term, 'proof) rewrite_info =
   { rw_name : string;
     rw_redex : 'term;
     rw_contractum : 'term;
     rw_proof : 'proof
   }

and ('term, 'proof) cond_rewrite_info =
   { crw_name : string;
     crw_params : 'term param list;
     crw_args : 'term list;
     crw_redex : 'term;
     crw_contractum : 'term;
     crw_proof : 'proof
   }

and ('term, 'proof) axiom_info =
   { axiom_name : string;
     axiom_stmt : 'term;
     axiom_proof : 'proof
   }

and ('term, 'meta_term, 'proof) rule_info =
   { rule_name : string;
     rule_params : 'term param list;
     rule_stmt : 'meta_term;
     rule_proof : 'proof
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

(*
 * An mlterm is a term that has an ML procedure for its rewrite.
 * The definition is not required in the interface.
 *)
and ('term, 'expr) mlterm_info =
   { mlterm_term : 'term;
     mlterm_contracta : 'term list;
     mlterm_def : ('expr * 'expr) option
   }

and 'term opname_info =
   { opname_name : string;
     opname_term : 'term
   }

(*
 * Dform descriptions.
 * The definition is not required in the interface.
 *)
and ('term, 'expr) dform_info =
   { dform_name : string;
     dform_modes : string list;
     dform_options : dform_option list;
     dform_redex : 'term;
     dform_def : ('term, 'expr) dform_def
   }

and dform_option =
   DFormInheritPrec
 | DFormPrec of string
 | DFormParens

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
     resource_data_type : 'ctyp
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
 * Pair it with a location.
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc =
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item * (int * int)

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

(************************************************************************
 * Interface								*
 ************************************************************************)

(* Creation *)
val find_sub_module : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   module_path ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

val new_module_info : unit -> ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

val info_items : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc list

(* Access *)
val find_axiom : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_rewrite : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_mlterm : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_condition : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_module : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_dform : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_prec : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_id : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info -> int

val get_resources : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   'ctyp resource_info list

val get_infixes : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string list

val get_proofs : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   (string * 'proof) list

val find : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc

val parents : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   module_path list

(* Update *)
val add_command : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

val set_command : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

(* Utilities *)
val summary_map :
   ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1,
    'term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) convert ->
   ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1) module_info ->
   ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) module_info

(*
 * Term conversion.
 *)
val term_of_meta_term : meta_term -> term
val meta_term_of_term : term -> meta_term

val term_of_rewrite :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'proof) rewrite_info ->
   term
val term_of_cond_rewrite :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'proof) cond_rewrite_info ->
   term
val term_of_axiom :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'proof) axiom_info ->
   term
val term_of_rule :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'meta_term, 'proof) rule_info ->
   term
val term_of_opname :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'term opname_info -> term
val term_of_mlterm :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'expr) mlterm_info ->
   term
val term_of_condition :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'expr) mlterm_info ->
   term
val term_of_parent :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'ctyp parent_info ->
   term
val term_of_dform :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'expr) dform_info ->
   term
val term_of_prec : string -> term
val term_of_prec_rel : prec_rel_info -> term
val term_of_id : int -> term
val term_of_resource :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'ctyp resource_info ->
   term
val term_of_infix : string -> term
val term_of_summary_item :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'item ->
   term
val term_of_magic_block :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'item magic_info ->
   term
val term_list :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term list
val of_term_list :
   (term, term, term, term, term, term, 'term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) convert ->
   term list ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

(*
 * Interface checking implem/interf/exception.
 *)
val check_implementation :
   (term, meta_term, 'proof1, 'ctyp1, 'expr1, 'item1) module_info ->
   (term, meta_term, 'proof2, 'ctyp2, 'expr2, 'item2) module_info ->
   unit

val copy_proofs :
   ('proof1 -> 'proof2 -> 'proof1) ->
   (term, meta_term, 'proof1, 'ctyp1, 'expr1, 'item1) module_info ->
   (term, meta_term, 'proof2, 'ctyp2, 'expr2, 'item2) module_info ->
   (term, meta_term, 'proof1, 'ctyp1, 'expr1, 'item1) module_info

(*
 * Debugging.
 *)
val eprint_command : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item -> unit
val eprint_info : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info -> unit

val debug_summary : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
