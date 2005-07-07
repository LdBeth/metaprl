(*
 * Commands for editing a rewrite.
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

open Refiner.Refiner.TermType
open Refiner.Refiner.TermTy

open Filter_type
open Filter_summary_type

open Shell_sig
open Package_info

(*
 * Make an editable rule/rewrite.
 *)
val create :
   package ->
   parse_arg ->
   display_fun ->
   string ->
   edit_object

val view_rule :
   package ->
   parse_arg ->
   display_fun ->
   (term, meta_term, proof proof_type, MLast.expr) rule_info ->
   edit_object

val view_rw :
   package ->
   parse_arg ->
   display_fun ->
   (term, proof proof_type, MLast.expr) rewrite_info ->
   edit_object

val view_crw :
   package ->
   parse_arg ->
   display_fun ->
   (term, proof proof_type, MLast.expr) cond_rewrite_info ->
   edit_object

val view_def :
   package ->
   parse_arg ->
   display_fun ->
   ty_term ->
   (term, MLast.expr) term_def ->
   edit_object

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
