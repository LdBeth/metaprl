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

open Refiner.Refiner.TermType
open Opname

open File_base_type

open Filter_type
open Filter_summary

(*
 * Abbreviations.
 *)
type module_name = string

type loc = MLast.loc

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
    * select: a type of tags for typecase on the info
    * info: an abstract type for represeting modules in this base
    * t: the database type
    *)
   type cooked
   type select
   type arg
   type info
   type t

   (* Creation *)
   val create : string list -> t
   val set_path : t -> string list -> unit
   val set_magic : t -> info -> int -> unit
   val create_info : t -> select -> dir_name -> file_name -> info

   (* Loading and saving *)
   val find : t -> arg -> module_path -> select -> alt_suffix -> info
   val find_file : t -> arg -> module_path -> select -> alt_suffix -> info
   val find_match : t -> arg -> info -> select -> alt_suffix -> info
   val save : t -> arg -> info -> alt_suffix -> unit
   val save_if_newer : t -> arg -> info -> alt_suffix -> unit

   (* Info about a module *)
   val info : t -> info -> cooked
   val set_info : t -> info -> cooked -> unit
   val sub_info : t -> info -> string -> info
   val name : t -> info -> module_name
   val pathname : t -> info -> module_path
   val root : t -> info -> info
   val file_name : t -> info -> file_name
   val type_of : t -> info -> select
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

   (* Type and id for this module *)
   type select
   val select : select

   (* Marshaling *)
   type cooked
   val marshal : (term, meta_term, proof, ctyp, expr, item) module_info -> cooked
   val unmarshal : cooked -> (term, meta_term, proof, ctyp, expr, item) module_info
end

(*
 * This is an enhanced SummaryBase.
 * It caches the following items:
 *    1. opnames in the current module
 *    2. infix operators in the current module
 *    3. resources in the current module
 *    4. summary of the current module, recorded as a module_info
 *
 * The cache also takes care of recursively opening parent modules
 * when a module is opened.  All the opnames of the parents are
 * recorded in the current module.  A hook function is allowed that
 * can be called as the parents are opened.
 *
 * The summary cache operates on (proof, ctyp, expr, item) module_info, but
 * the signature type is allowed to differ from the type being constructed.
 * The sig_* types parameterize the signatures, and the str_* types
 * parameterize the module_info being constructed.
 *)
type summary_mode =
   CompiledSummary
 | InteractiveSummary

module type SummaryCacheSig =
sig
   (*
    * proof, ctyp, expr, item: parameters to module_info
    * select: a type of tags for typecase on the info
    * info: an abstract type for represeting modules in this base
    * t: the database type
    *)
   type sig_proof
   type sig_ctyp
   type sig_expr
   type sig_item
   type sig_elem = (term, meta_term, sig_proof, sig_ctyp, sig_expr, sig_item) summary_item
   type sig_info = (term, meta_term, sig_proof, sig_ctyp, sig_expr, sig_item) module_info

   type str_proof
   type str_ctyp
   type str_expr
   type str_item
   type str_elem = (term, meta_term, str_proof, str_ctyp, str_expr, str_item) summary_item
   type str_info = (term, meta_term, str_proof, str_ctyp, str_expr, str_item) module_info

   type arg
   type select
   type info
   type t

   (* Hook that is called whenever a module is loaded *)
   type 'a hook = info -> module_path * sig_info -> 'a -> 'a

   (* Creation *)
   val create : string list -> t
   val set_path : t -> string list -> unit

   (* Loading *)
   val create_cache : t -> module_name -> select -> select -> info
   val load : t -> arg -> module_name -> select -> select -> 'a hook -> 'a -> alt_suffix -> info * 'a
   val check : info -> arg -> select -> sig_info
   val parse_comments : info -> (loc -> term -> term) -> unit
   val copy_proofs : info -> arg -> (str_proof -> str_proof -> str_proof) -> unit
   val set_mode : info -> summary_mode -> unit
   val save : info -> arg -> alt_suffix -> unit

   (* Access *)
   val info : info -> str_info
   val name : info -> string
   val filename : t -> info -> string
   val sig_info : info -> arg -> select -> sig_info
   val sub_info : info -> module_path -> sig_info

   (* Expand a partial path specification to a complete one *)
   val expand_path : info -> module_path -> module_path

   (* Opname management *)
   val optable : info -> (string -> opname)
   val op_prefix : info -> opname
   val add_opname : info -> string -> opname -> unit
   val rm_opname : info -> string -> unit
   val mk_opname : info -> string list -> opname

   (* Inherited access for module_info *)
   val find : info -> string -> (str_elem * loc)
   val find_axiom : info -> string -> (str_elem * loc) option
   val find_rewrite : info -> string -> (str_elem * loc) option
   val find_mlrewrite : info -> string -> (str_elem * loc) option
   val find_mlaxiom : info -> string -> (str_elem * loc) option
   val find_dform : info -> string -> (str_elem * loc) option
   val find_prec : info -> string -> bool
   val resources : info -> (module_path * str_ctyp resource_info) list
   val parents : info -> module_path list
   val proofs : info -> (string * str_proof) list

   (* These are the resources for an included parent *)
   val sig_resources : info -> module_path -> str_ctyp resource_info list

   (*
    * Update.
    *)
   val add_command : info -> (str_elem * loc) -> unit
   val set_command : info -> (str_elem * loc) -> unit
   val add_resource : info -> str_ctyp resource_info -> unit
   val add_prec : info -> string -> unit

   (*
    * Debugging.
    *)
   val eprint_info : info -> unit

   (*
    * An inlined module includes its opnames in the current
    * space.  This function recursively inlines all modules in
    * the hierarchy, calling the hook on each.
    *)
   val inline_module : info -> arg -> module_path -> 'a hook -> 'a -> sig_info * 'a
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
