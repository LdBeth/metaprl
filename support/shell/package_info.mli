(*
 * The editor collects information about each theory and
 * summarizes it in a "package".  At start-up, the pre-loaded packages
 * are collected and presented as read-only theories.
 *
 * We can also create interactive packages, which are writable,
 * and may contain interactivly generated proofs.
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
open Opname
open Term_shape_sig
open Refiner.Refiner
open Refiner.Refiner.TermType

open Filter_type
open Filter_summary
open Filter_summary_type

open Tactic_type

open Shell_sig
open Proof_edit

(*
 * This is the database type.
 *)
type t

(*
 * This is the type of a package.
 *)
type package

(*
 * Proofs are abstract.
 *)
type proof

(*
 * Create a database.  The argument is the search path.
 *)
val create : string list -> t
val refresh : t -> string list -> unit

(*
 * Filesystem interface.
 * Loaded packages are initially read-only.
 *)
val create_package : t -> parse_arg -> string -> package
val load   : t -> parse_arg -> string -> package
val get    : t -> string -> package
val backup : parse_arg -> package -> unit
val save   : parse_arg -> package -> unit
val export : parse_arg -> package -> unit
val revert : package -> unit

(*
 * Access.
 *)
val name       : package -> string
val filename   : package -> parse_arg -> string

(*
 * Refiners and sentinals.
 *)
val get_refiner  : package -> Refine.refiner

(*
 * Navigation.
 *)
val packages : t -> package list
val groups : t -> (string * string) list
val group_exists : t -> string -> bool
val group_packages : t -> string -> string * string list (* May raise Not_found *)
val group_of_module : t -> string -> string (* May raise Not_found *)
val roots : t -> package list
val parents : t -> package -> package list
val children : t -> package -> package list

(*
 * Access to the status.
 *)
val status  : package -> package_status
val set_status : package -> package_status -> unit
val touch : package -> unit

(*
 * Access to the cache.
 *)
val mk_opname : package -> string list -> shape_param list -> int list -> opname

(*
 * Infixes/suffixes declared in the package
 *)
val get_infixes : package -> Infix.Set.t

(*
 * Quotation expander used by this module.
 *)
val get_grammar : package -> Filter_grammar.t

(*
 * Collection of objects in the module.
 *)
val info : package -> parse_arg -> (term, meta_term, proof proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) module_info
val sig_info : package -> parse_arg -> (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
val find : package -> parse_arg -> string -> (term, meta_term, proof proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) summary_item
val set : package -> parse_arg -> (term, meta_term, proof proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) summary_item -> unit

(*
 * This is the starting info for new proofs.
 *)
val arg_resource : package -> parse_arg -> string -> Mp_resource.global_resource
val new_proof : package -> parse_arg -> string -> term list -> term -> proof
val ped_of_proof : package -> parse_arg -> proof -> Refine.msequent -> ped
val status_of_proof : proof -> Proof.status
val node_count_of_proof : proof -> int * int

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
