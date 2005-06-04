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

open Refiner.Refiner.TermShape
open Tactic_type.Tactic

open Shell_sig
open Shell_util
open Shell_internal_sig

(*
 * Globals.
 *)
val get_resource : shell -> Mp_resource.global_resource

val packages : Package_info.t
val modified_packages : unit -> Package_info.package list
val all_theories : unit -> string list
val shell_package : Package_info.package -> bool
val get_current_package : shell -> Package_info.package

val get_db : shell -> dform_base
val default_base : dform_base
val get_display_mode : shell -> display_fun
val get_shortener : shell -> Dform.shortener
val set_dfmode : shell -> dform_mode -> unit
val set_dftype : shell -> display_type -> unit
val set_window_width : shell -> int -> unit

val touch : shell -> unit

val parse_path : shell -> string -> shell_dir
val string_of_dir : shell_dir -> string
val path_of_dir : shell_dir -> string list
val dir_of_path : string list -> shell_dir
val module_dir : string -> shell_dir
val proof_dir : string -> string -> shell_dir
val pwd : shell -> string
val relative_pwd : shell -> string
val fs_pwd : shell -> string

val get_ls_options : shell -> LsOptionSet.t
val get_view_options : shell -> string
val set_view_options : shell -> string -> unit
val clear_view_options : shell -> string -> unit

val view : parse_arg -> shell -> LsOptionSet.t -> unit
val chdir : parse_arg -> shell -> bool -> bool -> shell_dir -> unit
val cd : parse_arg -> shell -> string -> string
val root : parse_arg -> shell -> string
val refresh : parse_arg -> shell -> unit

(*
 * Apply_all maps a function over the items in a module
 * or over items in the root theory.
 *)
val apply_all : parse_arg -> shell -> item_fun -> bool -> clean_item_fun -> clean_module_fun -> unit

(*
 * Arguments to apply_all.
 *)
val dont_clean_item   : clean_item_fun
val dont_clean_module : clean_module_fun
val clean_resources   : clean_item_fun
val clean_and_abandon : clean_module_fun

(*
 * Proof modifications.
 *)
val set_goal : shell -> term -> unit
val set_redex : shell -> term -> unit
val set_contractum : shell -> term -> unit
val set_assumptions : shell -> term list -> unit
val set_params : shell -> term Filter_type.param list -> unit

val filename : parse_arg -> shell -> string option

val backup      : parse_arg -> shell -> unit
val backup_all  : parse_arg -> shell -> unit
val save        : parse_arg -> shell -> unit
val save_all    : parse_arg -> shell -> unit
val export      : parse_arg -> shell -> unit
val export_all  : parse_arg -> shell -> unit
val revert      : parse_arg -> shell -> unit
val revert_all  : parse_arg -> shell -> unit
val abandon     : parse_arg -> shell -> unit
val abandon_all : parse_arg -> shell -> unit

val create_pkg : parse_arg -> shell -> string -> unit
val create_ax_statement : parse_arg -> shell -> term -> string -> unit

val check : shell -> unit
val expand : shell -> unit
val expand_all : parse_arg -> shell -> unit
val interpret : shell -> proof_command -> unit
val refine : shell -> tactic -> unit

val print_theory : parse_arg -> shell -> string -> unit

val extract : parse_arg -> shell -> shell_dir -> unit -> Refiner.Refiner.Refine.extract
val term_of_extract : shell -> term list -> term

val edit_find : shell -> int -> string
val edit_is_enabled : shell -> method_name -> bool

val undo : shell -> unit
val redo : shell -> unit

(*
 * Name from an opname.
 *)
val mk_dep_name : opname -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
