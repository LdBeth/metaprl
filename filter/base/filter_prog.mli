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
   type arg

   val extract_sig :
      arg ->
      (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (module_path * string * MLast.ctyp resource_sig) list ->
      string -> (MLast.sig_item * (int * int)) list

   val extract_str :
      arg ->
      (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
      (term, meta_term, proof proof_type, MLast.expr, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
      (module_path * string * MLast.ctyp resource_sig) list ->
      string -> (MLast.str_item * (int * int)) list
end

module MakeExtract (Convert : ConvertProofSig) :
   ExtractSig
   with type arg = Convert.t
   with type proof = Convert.cooked

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
