(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 * This is just like a FileBase, but we add more structure to it.
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
 *
 *)
open Lm_symbol
open Lm_string_set

open Opname
open Term_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy

open File_base_type

open Filter_type
open Filter_summary

(*
 * Abbreviations.
 *)
type module_name = string

type loc = MLast.loc

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * This module just provides subaddressing of its objects.
 *)
module type AddressSig =
sig
   type t
   val create : unit -> t
   val find_sub_module : t -> module_path -> t
end

(*
 * This is a module base that loads modules automatically
 * given a directory path to look for module files.
 * The proof type must be specified (so we know what kind
 * of proofs are in the file).
 *)
module type SummaryBaseSig =
sig
   (*
    * info: an abstract type for represeting modules in this base
    * t: the database type
    *)
   type cooked
   type arg
   type info
   type t

   (* Creation *)
   val create          : string list -> t
   val clear           : t -> unit
   val set_path        : t -> string list -> unit
   val set_magic       : t -> info -> int -> unit
   val create_info     : t -> select_type -> dir_name -> file_name -> info
   val remove_info     : t -> info -> unit

   (* Loading and saving *)
   val find            : t -> arg -> module_path -> select_type -> alt_suffix -> info
   val find_file       : t -> arg -> module_path -> select_type -> alt_suffix -> info
   val find_match      : t -> arg -> info -> select_type -> alt_suffix -> info
   val save            : t -> arg -> info -> alt_suffix -> unit
   val save_if_newer   : t -> arg -> info -> alt_suffix -> unit
   val save_if_missing : t -> arg -> info -> alt_suffix -> unit

   (* Info about a module *)
   val info            : t -> info -> cooked
   val set_info        : t -> info -> cooked -> unit
   val sub_info        : t -> info -> string -> info
   val name            : t -> info -> module_name
   val pathname        : t -> info -> module_path
   val root            : t -> info -> info
   val file_name       : t -> info -> file_name
   val type_of         : t -> info -> select_type
end

(*
 * This module just contains types to express sharing contraints.
 * We need all four type arguments to module_info.
 *)
module type MarshalSig =
sig
   (* These are the types used for module_info *)
   type proof
   type ctyp
   type expr
   type item
   type resource

   (* Type and id for this module *)
   val select : select_type

   (* Marshaling *)
   type cooked
   val marshal : (term, meta_term, proof, resource, ctyp, expr, item) module_info -> cooked
   val unmarshal : cooked -> (term, meta_term, proof, resource, ctyp, expr, item) module_info
end

(*
 * This is an enhanced SummaryBase.
 * It caches the following items:
 *    1. opnames in the current module
 *    2. infix operators in the current module
 *    3. resources in the current module
 *    4. grammar for the current module
 *    5. summary of the current module, recorded as a module_info
 *
 * The cache also takes care of recursively opening parent modules
 * when a module is opened.  All the opnames of the parents are
 * recorded in the current module.
 *
 * The summary cache operates on (proof, ctyp, expr, item) module_info, but
 * the signature type is allowed to differ from the type being constructed.
 * The sig_* types parameterize the signatures, and the str_* types
 * parameterize the module_info being constructed.
 *)
type parse_state = Filter_reflection.parse_state

type summary_mode =
   CompiledSummary
 | InteractiveSummary

module type SummaryCacheSig =
sig
   (*
    * proof, ctyp, expr, item: parameters to module_info
    * info: an abstract type for represeting modules in this base
    * t: the database type
    *)
   type sig_proof
   type sig_ctyp
   type sig_expr
   type sig_item
   type sig_elem = (term, meta_term, sig_proof, sig_ctyp resource_sig, sig_ctyp, sig_expr, sig_item) summary_item
   type sig_info = (term, meta_term, sig_proof, sig_ctyp resource_sig, sig_ctyp, sig_expr, sig_item) module_info

   type str_proof
   type str_ctyp
   type str_expr
   type str_item
   type str_resource
   type str_elem = (term, meta_term, str_proof, str_resource, str_ctyp, str_expr, str_item) summary_item
   type str_info = (term, meta_term, str_proof, str_resource, str_ctyp, str_expr, str_item) module_info

   type arg
   type info
   type t

   (* Creation *)
   val create         : string list -> t
   val clear          : t -> unit
   val clear_info     : info -> unit
   val set_path       : t -> string list -> unit

   (* Loading *)
   val create_cache   : t -> module_name -> select_type -> info
   val load           : t -> arg -> module_name -> select_type -> alt_suffix -> info
   val filename       : t -> info -> string

   (* Module operations *)
   val check          : info -> arg -> select_type -> sig_info
   val parse_comments : info -> (loc -> term -> term) -> unit
   val copy_proofs    : info -> arg -> (str_proof -> str_proof -> str_proof) -> unit
   val revert_proofs  : info -> arg -> unit
   val set_mode       : info -> summary_mode -> unit
   val save           : info -> arg -> alt_suffix -> unit

   (* Access *)
   val info           : info -> str_info
   val name           : info -> string
   val sig_info       : info -> arg -> select_type -> sig_info
   val sub_info       : info -> module_path -> sig_info

   (* Expand a partial path specification to a complete one *)
   val expand_path    : info -> module_path -> module_path

   (* Opname management *)
   val declare_typeclass    : info -> shape_class -> opname -> opname -> typeclass_parent -> unit
   val declare_type         : info -> shape_class -> ty_term -> opname -> unit
   val declare_term         : info -> shape_class -> ty_term -> unit
   val declare_type_rewrite : info -> term -> term -> unit

   val get_parsing_state : info -> parsing_state

   (* Inherited access for module_info *)
   val find           : info -> string -> (str_elem * loc)
   val find_axiom     : info -> string -> (str_elem * loc) option
   val find_rewrite   : info -> string -> (str_elem * loc) option
   val find_mlrewrite : info -> string -> (str_elem * loc) option
   val find_mlaxiom   : info -> string -> (str_elem * loc) option
   val find_dform     : info -> string -> (str_elem * loc) option
   val find_prec      : info -> string -> bool
   val resources      : info -> (module_path * string * str_ctyp resource_sig) list
   val parents        : info -> module_path list
   val proofs         : info -> (string * str_proof) list

   (* These are the resources and infixes for an included parent *)
   val sig_resources  : info -> module_path -> (string * sig_ctyp resource_sig) list
   val sig_infixes    : info -> module_path -> Infix.Set.t

   (* All infixes - own and inherited *)
   val all_infixes    : info -> Infix.Set.t

   (*
    * Update.
    *)
   val add_command    : info -> (str_elem * loc) -> unit
   val set_command    : info -> (str_elem * loc) -> unit
   val add_resource   : info -> string -> str_ctyp resource_sig -> unit
   val add_prec       : info -> string -> unit
   val hash           : info -> int

   (*
    * Debugging.
    *)
   val eprint_info    : info -> unit

   (*
    * An inlined module includes its opnames in the current
    * space.  This function recursively inlines all modules in
    * the hierarchy.
    *)
   val inline_module  : info -> arg -> module_path -> unit

   (* Grammar functions *)
   type precedence

   val load_sig_grammar  : info -> arg -> select_type -> unit

   val add_token         : info -> opname -> symbol -> string -> term option -> unit
   val add_token_pair    : info -> opname -> symbol -> string -> string -> term option -> unit
   val add_production    : info -> symbol -> term list -> term option -> term -> unit
   val add_iform         : info -> symbol -> term -> term -> unit
   val find_input_prec   : info -> term -> precedence
   val input_prec_lt     : info -> term -> Filter_grammar.assoc -> precedence
   val input_prec_gt     : info -> term -> Filter_grammar.assoc -> precedence
   val input_prec_new    : info -> Filter_grammar.assoc -> precedence
   val add_input_prec    : info -> precedence -> term -> unit
   val add_start         : info -> string -> term -> opname -> unit
   val get_start         : info -> shape StringTable.t
   val parse             : parse_state -> info -> Lexing.position -> shape -> string -> term
   val compile_parser    : info -> unit
end

(*
 * Proofs are:
 *   1. primitive terms,
 *   2. tactics.
 *   3. inferred from interactive proofs
 *)
type 'a proof_type =
   Primitive of term
 | Derived of MLast.expr
 | Incomplete
 | Interactive of 'a

module type CachesSig =
sig
   type t
   type cooked

   (*
    * The summary_cache for interfaces and implementations.
    *)
   module SigFilterCache :
      SummaryCacheSig
      with type sig_proof  = unit
      with type sig_ctyp   = MLast.ctyp
      with type sig_expr   = MLast.expr
      with type sig_item   = MLast.sig_item
      with type str_proof  = unit
      with type str_ctyp   = MLast.ctyp
      with type str_expr   = MLast.expr
      with type str_item   = MLast.sig_item
      with type str_resource = MLast.ctyp resource_sig
      with type arg        = t

   module StrFilterCache :
      SummaryCacheSig
      with type sig_proof  = unit
      with type sig_ctyp   = MLast.ctyp
      with type sig_expr   = MLast.expr
      with type sig_item   = MLast.sig_item
      with type str_proof  = cooked proof_type
      with type str_ctyp   = MLast.ctyp
      with type str_expr   = MLast.expr
      with type str_item   = MLast.str_item
      with type str_resource = (MLast.ctyp, MLast.expr) resource_str
      with type arg        = t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
