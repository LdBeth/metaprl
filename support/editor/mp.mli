(*
 * This is the main file for MetaPRL.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
open Refiner.Refiner.Refine

(* open Tactic_type.Tacticals *)

open Shell_sig
(* open Proof_edit *)

(*
 * BUG JYH: reenable these?
(*
 * The possible objects in a package.
 *)
val create_rw : string -> unit
val create_axiom : string -> unit
val create_thm : string -> unit
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
 * Editing commands.
 *)
val set_goal : term -> unit
val set_redex : term -> unit
val set_contractum : term -> unit
val set_assumptions : term list -> unit
val set_params : term Filter_type.param list -> unit

(*
 * Proof editing.
 *)
val goal : unit -> tactic_arg
 *)

(*
 * Nuprl5.
 *)
val run_nuprl : int -> string -> string -> string -> unit
val run_nuprljp : int -> string -> string -> string -> unit
val edit_list_modules : unit -> string list
val edit_list_module_all : string -> string list
val edit_list_module : string -> string list * string list * string list * string list
val edit_list_module_rw : string -> string list
val edit_list_parents : string -> string list
val edit_list_dforms : string -> (string * Dform.dform_modes * term list * term * term) list
val edit_list_precs : string -> term list
val edit_list_prec_rels : string -> (string * term * term) list
val edit_cd_list_contents : string -> obj_contents list
val edit_create_thm : string -> string -> unit
val edit_create_rw : string -> string -> unit
val edit_cd_thm : string -> string -> unit
val edit_set_goal : string -> string -> term -> unit
val edit_set_redex : string -> string -> term -> unit
val edit_set_contractum : string -> string -> term -> unit
val edit_set_assumptions : string -> string -> term list -> unit
(* val edit_set_params : string -> string -> term Filter_type.param list -> unit *)
val edit_refine : int list -> string -> msequent * msequent list * msequent list
val edit_node : int list -> string option * msequent * msequent list * msequent list
val edit_save : string -> unit
val edit_undo : unit -> unit

(*
 * Need this for the standard toploop.
 *)
val shell_get_term : int -> term

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
