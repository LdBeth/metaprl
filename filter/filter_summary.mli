(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
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
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item =
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
      ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info) summary_item_type

(*
 * Pair it with a location.
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc =
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item * (int * int)

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
