(*
 * This is the standard interface to the window system.
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
open Lm_string_set

open Refiner.Refiner.TermType

open Shell_util

(*
 * The output goes into the "current" buffer.
 *)
val set_main        : Lm_rformat.buffer -> term StringTable.t -> unit

(*
 * Add a prompt/directory to a specific buffer.
 *)
val add_prompt      : string -> unit
val add_directory   : string -> unit
val add_file        : string option -> unit
val add_edit        : string -> unit

(*
 * Get the output for display.
 *)
val format_main     : int -> Buffer.t -> unit
val format_message  : int -> Buffer.t -> unit

(*
 * Get the saved components.
 *)
val get_directories : unit -> string list
val get_history     : unit -> string list
val get_term        : string -> term
val get_files       : unit -> string list

(*
 * View options.
 *)
val get_view_options : unit -> LsOptionSet.t
val set_view_options : LsOptionSet.t -> unit
val add_view_options : string -> unit
val clear_view_options : string -> unit

(*
 * This function should be used while output is begin diverted.
 *)
val synchronize : ('a -> 'b) -> 'a -> 'b

(*
 * Perform a shell command and add the output to the message
 * window.
 *)
val add_command : Browser_syscall.t -> unit

(*
 * Complete edit operations.
 *)
val add_edit_point  : string -> int -> unit
val get_edit_point  : string -> int

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
