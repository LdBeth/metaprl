(*
 * Display a particular theory.
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

include Base_theory

include Package_info

open Refiner.Refiner.TermType
open Rformat

open Filter_cache

open Filter_summary

open Package_info

(*
 * Display parameters.
 *)
val tabstop : int
val min_screen_width : int

(*
 * Summaries conversions.
 *)
val convert_intf : (term, meta_term, unit, MLast.ctyp, MLast.expr, MLast.sig_item,
                    term, term, term, term, term, term) convert
val convert_impl : (term, meta_term, Package_info.Package.proof proof_type, MLast.ctyp, MLast.expr, MLast.str_item,
                    term, term, term, term, term, term) convert

(*
 * Printers.
 *)
val format_interface : string -> buffer -> Package.package -> unit
val format_implementation : string -> buffer -> Package.package -> unit

val format_packages : buffer -> Package.t -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
