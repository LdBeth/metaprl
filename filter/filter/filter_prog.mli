(*
 * Conversion form filter_summary to program text.
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
 *)

open Refiner.Refiner.Refine

open Filter_type
open Filter_summary
open Filter_summary_type
open Proof_convert

val extract_sig :
   Convert.t ->
   (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
   (module_path * string * MLast.ctyp resource_sig) list ->
   string -> string -> string -> (MLast.sig_item * MLast.loc) list

val extract_str :
   Convert.t ->
   (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info ->
   (term, meta_term, Convert.cooked proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
   (module_path * string * MLast.ctyp resource_sig) list ->
   string -> string -> string -> (MLast.str_item * MLast.loc) list

val expr_of_loc : MLast.loc -> MLast.expr

module ProofCaches :
   CachesSig
   with type t = Convert.t
   with type cooked = Convert.cooked

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
