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
open Lm_debug

open Lm_printf
open Lm_threads

open Shell_sig

let _ =
   show_loading "Loading Shell HTTP%t"

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "HTTP server operations";
        debug_value = false
      }

(*
 * We may start this as a web service.
 *)
let http_port = Env_arg.int "port" None "start web service on this port" Env_arg.set_int_option_int
let http_enabled = Env_arg.bool "http" false "whether to start a web service" Env_arg.set_bool_bool

module ShellJava (Shell : ShellSig) =
struct
   (*
    * Serve a client.
    *)
   let serve_client fd port =
      try
         while true do
            let port, args = Java_mux_channel.input_line port in
               match args with
                  command :: _ ->
                     let command = Http_server.decode_hex command in
                     let port = Java_mux_channel.id_of_session port in
                        raise (Invalid_argument "Not implemented: ShellJava.serve_client")
(*
                     let shell = Shell.find_shell port in
                        ignore (Shell.eval_top shell command)
*)
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
      let fd = Lm_inet.file_descr_of_client client in
      let _ =
         if !debug_http then
            eprintf "ShellHTTP: starting service on port %d%t" (Obj.magic fd) eflush
      in
      let port = Java_mux_channel.create fd (Some url) in
      let session = Java_mux_channel.create_session port in
      let _ = Shell.set_port (Some session) in
         ignore (Thread.create (serve_client fd) port)

   (*
    * Start the web server.
    *)
   let main () =
      if !http_enabled then
         begin
            let host = Http_server.start_http http_connect !http_port in
               match Lm_terminfo.xterm_escape_begin (), Lm_terminfo.xterm_escape_begin () with
                  Some b, Some e ->
                     let { Http_server.http_host = host;
                           Http_server.http_port = port;
                           Http_server.http_password = pass
                         } = Http_server.http_info host
                     in
                        printf "%sMetaPRL http://%s:%d/%s%s%t" b host port pass e eflush
                | _ ->
                     ()
         end
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
