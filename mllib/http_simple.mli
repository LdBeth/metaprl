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
open Http_server_type

(*
 * Type of web servers.
 *)
type t

(*
 * Info about the local connection.
 *)
type http_info =
   { http_host     : string;
     http_port     : int
   }

(*
 * Helper function for decoding uri's that contain hex chars.
 *)
val decode_hex : string -> string
val encode_hex : string -> string

(*
 * Decode the filename in the URI.
 *)
val decode_uri : string -> string list

(*
 * The body is a list of key/value pairs.
 *)
val parse_post_body : string -> (string * string) list

(*
 * Start a synchronous web server on the specified port.
 * The function argument handle client connections.  This
 * version is synchronous, and not threaded.
 *)
type 'a start_handler = t -> 'a -> 'a
type 'a connect_handler = t -> 'a -> out_channel -> in_channel -> string list -> request_header_entry list -> string -> 'a

val serve_http : 'a start_handler -> 'a connect_handler -> 'a -> int option -> unit

(*
 * Get the info for the server.
 *)
val http_info : t -> http_info

(*
 * Responses.
 *)
val print_success_page : out_channel -> response_code -> Buffer.t -> unit
val print_error_page : out_channel -> response_code -> unit
val print_redirect_page : out_channel -> response_code -> string -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
