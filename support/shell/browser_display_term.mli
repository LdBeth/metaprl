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

(*
 * We allow the output to be multiplexed.
 *)
type t

val create : unit -> t

(*
 * The output goes into the "current" buffer.
 *)
val set_main        : Lm_rformat.buffer -> unit

(*
 * Add a prompt to a sepcific buffer.
 *)
val add_prompt      : t -> string -> unit

(*
 * Get the output for display.
 *)
val format_main     : t -> int -> Buffer.t -> unit
val format_message  : t -> int -> Buffer.t -> unit
val get_history     : t -> string StringTable.t -> Buffer.t * string StringTable.t

(*
 * This function should be used while output is begin diverted.
 *)
val synchronize : t -> ('a -> 'b) -> 'a -> 'b

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
