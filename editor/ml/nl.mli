(*
 * This is the main file for Nuprl-Light.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

open Refiner.Refiner.TermType
open Refiner.Refiner.Refine

open Tacticals

(*
 * Navigation and display.
 *)
val cd : string -> string
val pwd : unit -> string
val set_window_width : int -> unit

(*
 * Module commands.
 *)
val load : string -> unit
val create_pkg : string -> unit
val set_writeable : unit -> unit
val save : unit -> unit
val save_all : unit -> unit

(*
 * The possible objects in a package.
 *)
val create_rw : string -> unit
val create_axiom : string -> unit
val create_thm : string -> unit
val create_tptp : string -> unit
val create_opname : string -> unit
val create_condition : string -> unit
val create_parent : string -> unit
val create_dform : string -> unit
val create_prec : string -> unit
val create_prec_rel : string -> string -> string -> unit
val create_resource : string -> unit
val create_infix : string -> unit
val create_ml : string -> unit

(*
 * View, close, check object.
 * An object is not installed until it is checked.
 *)
val view : string -> unit
val ls : unit -> unit

(*
 * Editing commands.
 *)
val set_goal : term -> unit
val set_redex : term -> unit
val set_contractum : term -> unit
val set_assumptions : term list -> unit
val set_params : term Filter_summary.param list -> unit
val check : unit -> unit
val expand : unit -> unit

(*
 * Proof editing.
 *)
val root : unit -> unit
val up : int -> unit
val down : int -> unit
val goal : unit -> Tactic_type.tactic_arg
val refine : tactic -> unit
val undo : unit -> unit
val fold : unit -> unit
val fold_all : unit -> unit

(*
 * Nuprl5.
 *)
val edit_list_modules : unit -> string list
val edit_list_module : string -> string list
val edit_create_thm : string -> string -> msequent -> unit
val edit_cd_thm : string -> string -> unit
val edit_refine : int list -> string -> msequent * msequent list * msequent list
val edit_node : int list -> string option * msequent * msequent list * msequent list
val edit_undo : unit -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
