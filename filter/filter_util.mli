(*
 * Common utilities for filtering modules.
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

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

(************************************************************************
 * UTILITIES								*
 ************************************************************************)

val context_vars_list : term list -> string list
val binding_vars_list : term list -> string list
val unzip_rewrite : string -> meta_term -> term list * term * term

val split_mfunction : meta_term -> term list * meta_term

(*
 * Module paths.
 *)
val string_of_path : module_path -> string
val output_path : out_channel -> module_path -> unit

(************************************************************************
 * OPNAMES								*
 ************************************************************************)

val string_of_opname_list : string list -> string
val translate_opname : opname -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
