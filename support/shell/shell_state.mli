(*
 * Implement the global functions required by the shell.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_printf

open Refiner.Refiner.TermType

open Dform

open Filter_type

(*
 * Client functions called somewhere within a toploop.
 * Client functions are global, and they are only valid
 * within a toploop call.  These functions usually fail if the
 * toploop is not active.
 *)
val term_printer : term -> unit  (* uses Format library *)
val print_term_fp : out_channel -> term -> unit
val set_tactic : string -> MLast.expr -> unit
val get_term : int -> term

(****************************************
 * TOPLOOP FUNCTIONS
 *)

(* This is the opname function used when terms are built. *)
val set_mk_opname : opname_fun option -> unit

(* This is the set of grammar infix/suffix mods needed in this state *)
val set_infixes : Infix.Set.t option -> unit

(* Scan (lazily) terms for SO variable contexts and use that for SO var parsing *)
val set_so_var_context : term list option -> unit

(*
 * Set the current module.
 *)
val set_module : string -> unit

(*
 * Current display form base.
 *)
val set_dfbase : dform_base option -> unit
val get_dfbase : unit -> dform_base

(*
 * Get the tactic used in the last refinement.
 *)
val get_tactic : unit -> string * MLast.expr
val get_toploop : unit -> Mptop.top_table

(*
 * Printers.
 *)
val print_term : term -> unit

(*
 * Flag for whether currently in interactive mode.
 *)
val is_interactive : unit -> bool
val set_interactive : bool -> unit

val reset_terms : unit -> unit
val synchronize : ('a -> 'b) -> 'a -> 'b

(****************************************
 * FILE PARSING
 *)

val stream_of_channel : in_channel -> char Stream.t
val stream_of_string : string -> char Stream.t
val set_prompt : string -> unit
val set_prompt2 : string -> unit
val stdin_stream : unit -> char Stream.t * (unit -> unit)
val get_text : int * int -> string
val wrap : (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'a
val set_file : string -> unit

(****************************************
 * ADDITIONAL FUNCTIONS
 *)

(*
 * Include directory list.
 *)
val get_includes : unit -> string list
val get_input_files : unit -> string list

(*
 * We may start this as a web service.
 *)
val browser_flag : bool ref

val browser_port_name : string
val browser_port      : int ref

val browser_name     : string
val browser_string   : string option ref

val challenge_name   : string
val challenge_string : string option ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
