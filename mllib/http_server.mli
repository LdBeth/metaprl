(*
 * A basic web server.
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
 * Type of web servers.
 *)
type t

(*
 * Info about the local connection.
 *)
type http_info =
   { http_host : string;
     http_port : int;
     http_password : string
   }

(*
 * Helper function for decoding uri's that contain hex chars.
 *)
val decode_hex : string -> string

(*
 * Set the search path.
 *)
val set_path : string list -> unit

(*
 * Start a web server on the specified port.
 * The function argument handle client connections:
 * this function should be thread-safe, since the
 * server runs in a separate thread.
 *)
val start_http : (t -> Lm_inet.client -> unit) -> int option -> t

(*
 * Get the actual port number.
 *)
val http_info : t -> http_info

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
