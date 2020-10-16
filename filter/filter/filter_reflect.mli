(*
 * Reflect a theory.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Refiner.Refiner.TermType

open Filter_type
open Filter_summary
open Filter_summary_type
open Proof_convert

open Filter_prog.ProofCaches

(*
 * Name conversions.
 *)
val reflect_filename : string option -> string -> string * string

(*
 * Compiling.
 *)
val compile_sig :
   SigFilterCache.info ->
   string ->
   (term, meta_term, Convert.cooked proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
   unit

val compile_str :
   StrFilterCache.info ->
   string ->
   (term, meta_term, Convert.cooked proof_type, (MLast.ctyp, MLast.expr) resource_str, MLast.ctyp, MLast.expr, MLast.str_item) module_info ->
   unit

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
