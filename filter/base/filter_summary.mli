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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol

open Refiner_sig

open Filter_type

(************************************************************************
 * TYPES								*
 ************************************************************************)

(*
 * A module_base contains information about a collection of modules.
 * A module_info contains information about a specific module.
 *
 *)
type ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

type ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item =
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info) summary_item_type

(*
 * Pair it with a location.
 *)
type ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc =
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item * (int * int)

(************************************************************************
 * Interface								*
 ************************************************************************)

(* Creation *)
val find_sub_module : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   module_path ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

val new_module_info : unit -> ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

val info_items : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc list

(* Access *)
val find_axiom : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_rewrite : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_mlrewrite : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_mlaxiom : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_module : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_dform : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_prec : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc option

val find_id : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info -> int

(* Only returns resources from this module *)
val get_resources : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   (string * 'resource) list

val get_infixes : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string list

val get_proofs : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   (string * 'proof) list

val find : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc

val parents : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   module_path list

(* Update *)
val add_command : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

val set_command : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

(* Utilities *)
val summary_map :
   ('term1, 'meta_term1, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1,
    'term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) convert ->
   ('term1, 'meta_term1, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info ->
   ('term2, 'meta_term2, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) module_info

val filter :
   (('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item_loc -> bool) ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

val resource_op : Opname.opname

(*
 * Term conversion.
 *)
module FilterSummaryTerm (ToTerm : RefinerSig) :
sig
   open ToTerm.TermType

   val term_of_meta_term : meta_term -> term
   val meta_term_of_term : term -> meta_term

   val term_of_cond_rewrite :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      ('term, 'proof, 'expr) cond_rewrite_info ->
      term
   val term_of_rule :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      ('term, 'meta_term, 'proof, 'expr) rule_info ->
      term
   val term_of_mlrewrite :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      ('term, 'expr) mlterm_info ->
      term
   val term_of_mlaxiom :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      ('term, 'expr) mlterm_info ->
      term
   val term_of_parent :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      'ctyp parent_info ->
      term
   val term_of_dform :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      ('term, 'expr) dform_info ->
      term
   val term_list :
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item,
       term, term, term, term, term, term, term) convert ->
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info ->
      term list
   val of_term_list :
      (term, term, term, term, term, term, term, 'term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) convert ->
      term list ->
      ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info

   (*
    * Interface checking.
    *)
   val check_implementation :
      (term, meta_term, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info ->
      (term, meta_term, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) module_info ->
      unit

   val copy_proofs :
      ('proof1 -> 'proof2 -> 'proof1) ->
      (term, meta_term, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info ->
      (term, meta_term, 'proof2, 'resource2, 'ctyp2, 'expr2, 'item2) module_info ->
      (term, meta_term, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info

   val parse_comments :
      (int * int -> term -> term) ->
      (term, meta_term, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info ->
      (term, meta_term, 'proof1, 'resource1, 'ctyp1, 'expr1, 'item1) module_info
end

(*
 * Debugging.
 *)
val eprint_command : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) summary_item -> unit
val eprint_info : ('term, 'meta_term, 'proof, 'resource, 'ctyp, 'expr, 'item) module_info -> unit

val debug_summary : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
