(*
 * Conversion form filter_summary to program text.
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

open Refiner.Refiner.TermType
open Refiner.Refiner.Refine

open Filter_type
open Filter_summary_type
open Filter_summary
open Filter_cache

(*
 * Expression of a term.
 *)
val expr_of_term : loc -> term -> MLast.expr
val expr_of_contractum : loc -> int -> MLast.expr

(*
 * Signature for extract module.
 *)
module type ExtractSig =
sig
   type proof

   val extract_sig :
      (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (module_path * MLast.ctyp resource_info) list ->
      string -> (MLast.sig_item * (int * int)) list

   val extract_str :
      (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (term, meta_term, proof proof_type, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
      (module_path * MLast.ctyp resource_info) list ->
      string -> (MLast.str_item * (int * int)) list

   (*
    * Defining implementations.
    *)
   type t

   val prim_axiom : t -> loc -> (term, 'proof, 'expr) axiom_info -> term -> MLast.str_item list
   val derived_axiom : t -> loc -> (term, 'proof, 'expr) axiom_info -> MLast.expr -> MLast.str_item list

   val prim_rule : t -> loc -> (term, meta_term, 'proof, 'expr) rule_info -> term -> MLast.str_item list
   val derived_rule : t -> loc -> (term, meta_term, 'proof, 'expr) rule_info -> MLast.expr -> MLast.str_item list

   val prim_rewrite : t -> loc -> (term, 'proof, 'expr) rewrite_info -> MLast.str_item list
   val derived_rewrite : t -> loc -> (term, 'proof, 'expr) rewrite_info -> MLast.expr -> MLast.str_item list

   val prim_cond_rewrite : t -> loc -> (term, 'proof, 'expr) cond_rewrite_info -> MLast.str_item list
   val derived_cond_rewrite : t -> loc -> (term, 'proof, 'expr) cond_rewrite_info -> MLast.expr -> MLast.str_item list

   val define_dform : t -> loc -> (term, MLast.expr) dform_info -> term -> MLast.str_item list
   val define_prec : t -> loc -> string -> MLast.str_item list
   val define_prec_rel : t -> loc -> prec_rel_info -> MLast.str_item list
   val define_resource : t -> loc -> MLast.ctyp resource_info -> MLast.str_item list
   val define_parent : t -> loc -> MLast.ctyp parent_info -> MLast.str_item list
   val define_magic_block : t -> loc -> MLast.str_item magic_info -> MLast.str_item list

   val implem_prolog : t -> loc -> string -> MLast.str_item list
   val implem_postlog : t -> loc -> string -> MLast.str_item list
end

module MakeExtract (Convert : ConvertProofSig) :
   ExtractSig with type proof = Convert.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
