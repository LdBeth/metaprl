(*
 * Core shell functions that do not depend on ShellP4.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Opname
open Dform

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape

open Shell_sig
open Shell_util
open Shell_internal_sig

(*
 * Globals.
 *)
val get_resource : shell -> Mp_resource.global_resource

val packages : Package_info.t
val all_packages : unit -> Package_info.package list
val shell_package : Package_info.package -> bool
val get_current_package : shell -> Package_info.package

val get_db : shell -> dform_base
val default_mode_base : Mp_resource.bookmark
val get_display_mode : shell -> display_mode
val get_shortener : shell -> opname -> param list -> bound_term list -> string
val set_dfmode : shell -> string -> unit
val set_window_width : shell -> int -> unit

val touch : shell -> unit
val mk_dep_name : opname -> string

val parse_path : shell -> string -> string list
val string_of_path : string list -> string
val pwd : shell -> string

val get_ls_options : shell -> LsOptionSet.t
val get_view_options : shell -> string
val set_view_options : shell -> string -> unit
val clear_view_options : shell -> string -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
