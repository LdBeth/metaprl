(*
 * The default implementation of the display
 * just shows it all as text.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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

include Package_sig

open Printf
open Mp_debug

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Rformat
open Dform_print

open Filter_type
open Filter_ocaml
open Filter_cache
open Filter_summary

open Tactic_type

open Shell_sig

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Display_textf%t" eflush

(*
 * Display the package DAG.
 *)
let term_of_root pack =
   let roots = Package.roots pack in
   let names = List.map Package.name roots in
   let terms = List.map mk_package_term names in
      mk_packages_term terms

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
