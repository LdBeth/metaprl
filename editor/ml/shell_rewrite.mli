(*
 * Commands for editing a rewrite.
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

include Shell_type
include Package_info
include Package_df

open Refiner.Refiner.TermType

open Filter_cache
open Filter_summary

open Tacticals
open Shell_type
open Package_info

(*
 * Make an editable rewrite.
 *)
val create :
   Package.package ->
   string ->
   edit_object

val view_rw :
   Package.package ->
   (term, Package.proof proof_type) rewrite_info ->
   edit_object

val view_crw :
   Package.package ->
   (term, Package.proof proof_type) cond_rewrite_info ->
   edit_object

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
