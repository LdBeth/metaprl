(*
 * This is the raw interface to the HTML window system.
 * Output is multiplexed over a common channel.
 * Channels are tracked with weak pointers, so that
 * they can be re-used if they are collected.
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

(*
 * This is the raw channel.
 *)
type t

(*
 * A session manages a collection of channels.
 *)
type session

(*
 * A channel is one of these types.
 *)
type channel

(*
 * Input uses a callback mechanism.
 *)
type callback = channel -> string list -> unit

(*
 * A proof has three channels.
 *)
type proof_channels =
   { goal_channel : channel;
     rule_channel : channel;
     subgoals_channel : channel
   }

(*
 * Output is multiplexed over a socket.
 * The second argument is the default URL.
 *)
val create : Unix.file_descr -> string option -> t

(*
 * Create a new session.
 *)
val create_session : t -> session
val new_session : session -> session
val id_of_session : session -> int
val url_of_channel : channel -> string option
val host_of_channel : channel -> string * int

(*
 * Channels.
 *)
val standard_menu : session -> channel
val new_menu : session -> channel
val new_proof : session -> proof_channels

(*
 * Callbacks.
 *)
val set_callback : channel -> callback -> unit

(*
 * Print a string on a channel.
 *)
val output_string : channel -> string -> unit

(*
 * Read a string off the default channel.
 *)
val input_line : t -> session * string list

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
