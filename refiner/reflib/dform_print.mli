(*
 * Add some features to the display form mechanism.
 * We want a default dform base for debugging purposes.
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
 *
 *)

open Rformat
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Dform

(*
 * We abstract the base a little to provide "modes" of
 * display forms.  Each mode has a string name.  The mode
 * "all" adds to all the modes at once.
 *)
type dform_mode_base

val null_mode_base : dform_mode_base

(*
 * Get a particular version of the base.
 *)
val get_mode_base : dform_mode_base -> string -> dform_base

(*
 * Join two bases.
 *)
val join_mode_base : dform_mode_base ref -> dform_mode_base -> unit

(*
 * Add a dform to the mode base, with a given mode.
 *)
val create_dform : dform_mode_base ref -> string list -> dform_info -> unit

(*
 * Destruction.
 *)
val is_null_mode_base : dform_mode_base -> bool
val equal_mode_bases : dform_mode_base -> dform_mode_base -> bool
val dest_mode_base : dform_mode_base -> dform_base * ((string * dform_base) list)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
