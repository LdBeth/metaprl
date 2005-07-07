(*
 * Resource management.
 *
 * This interface is not meant to be used by end-users.
 * The only code that uses this interface is the code
 * created by filter.
 *
 * See doc/resources_spec.txt for more information.
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
 * Copyright (C) 2001 Aleksey Nogin, Cornell University
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
 * Author: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol
open Rewrite_sig
open Refiner.Refiner.Refine

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type bookmark = string * string (* theory name + local name *)

type ('input, 'intermediate, 'output) funct_processor = {
   fp_empty: 'intermediate;
   fp_add: 'intermediate -> 'input -> 'intermediate;
   fp_retr: 'intermediate -> 'output
}

type ('input, 'intermediate, 'output) imper_processor = {
   imp_create: unit -> 'intermediate;
   imp_add: 'intermediate -> 'input -> unit;
   imp_retr: 'intermediate -> 'output
}

type ('input, 'intermediate, 'output) resource_info =
   Imperative of ('input, 'intermediate, 'output) imper_processor
 | Functional of ('input, 'intermediate, 'output) funct_processor

type global_resource

type ('annotation, 'input) annotation_processor =
   string ->            (* Name of the new rule *)
   rewrite_args_spec -> (* Names of the context vars parameters *)
   term list ->         (* Term parameters *)
   meta_term ->         (* Rule statement *)
   'annotation ->       (* Extra arguments, will include Tactic.pre_tactic *)
   'input list

type ('annotation, 'input) rw_annotation_processor =
   string ->            (* Name of the new rewrite *)
   term ->              (* Redex *)
   term ->              (* Contractum *)
   term list ->         (* Assumptions *)
   rewrite_args_spec -> (* Names of the context vars parameters *)
   term list ->         (* Term arguments *)
   'annotation ->       (* Extra arguments, will include Refine.prim_rewrite *)
   'input list

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

val improve : string -> Obj.t -> unit
val improve_list : string -> Obj.t list -> unit
val bookmark : string -> unit
val extends_theory : string -> unit
val close_theory : string -> unit

(*
 * create_resource "name" info
 * will return a function that can be used to access global resource
 *)
val create_resource:
   string -> ('input, 'intermediate, 'output) resource_info ->
   global_resource -> 'output

(* Will raise Not_found if bookmark does not exist *)
val find : bookmark -> global_resource

(* Bookmark pointing to all the theories loaded *
 * last time recompute_top was called           *)
val top_bookmark : bookmark
val recompute_top : unit -> unit

val empty_bookmark : global_resource
val theory_bookmark : string -> bookmark

val debug_resource : bool ref

(* Finds the names of the _immediate_ parents *)
val get_parents : string -> string list

(* Forgets the results of calling fp_retr and imp_retr for all resources under specific bookmark *)
val clear_results : bookmark -> unit

(* Clear all the cached data *)
val clear : unit -> unit
