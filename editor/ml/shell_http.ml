(*
 * This is the WWW service.
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

open Printf
open Mp_debug

open Shell_sig

let _ =
   show_loading "Loading Shell HTTP%t"

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "display HTTP services";
        debug_value = false
      }

(*
 * We may start this as a web service.
 *)
let http_port = Env_arg.int "port" None "start web service on this port" Env_arg.set_int_option_int

module ShellHTTP (Shell : ShellSig) =
struct
   (*
    * Serve a client.
    *)
   let serve_client fd port =
      try
         while true do
            let port, args = Mux_channel.input_line port in
               match args with
                  command :: _ ->
                     let command = Http_server.decode_hex command in
                     let port = Mux_channel.id_of_session port in
                        Shell.eval port command
                | [] ->
                     ()
         done;
      with
         Unix.Unix_error _
       | Sys_error _ ->
            eprintf "Shell: client closed connection%t" eflush;
            Shell.set_port None;
            Unix.close fd

   (*
    * Handle a connection.
    * We switch the mode to HTML.
    *)
   let http_connect server client =
      let { Http_server.http_host = host;
            Http_server.http_port = port
          } = Http_server.http_info server
      in
      let url = sprintf "http://%s:%d/" host port in
      let fd = Mp_inet.file_descr_of_client client in
      let _ =
         if !debug_http then
            eprintf "ShellHTTP: starting service on port %d%t" (Obj.magic fd) eflush
      in
      let port = Mux_channel.create fd (Some url) in
      let session = Mux_channel.create_session port in
      let _ = Shell.set_port (Some session) in
         ignore (Thread.create (serve_client fd) port)

   (*
    * Start the web server.
    *)
   let main () =
      let host = Http_server.start_http http_connect !http_port in
      let _ =
         try
            if Sys.getenv "TERM" = "xterm" then
               let { Http_server.http_host = host;
                     Http_server.http_port = port;
                     Http_server.http_password = pass
                   } = Http_server.http_info host
               in
                  printf "\027]0;MetaPRL http://%s:%d/%s\007%t" host port pass eflush
         with
            Not_found ->
               ()
      in
         Shell.main ()
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
