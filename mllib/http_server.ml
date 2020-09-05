(*
 * Implement an HTTP services for serving web pages.
 *
 * ------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright C 1999 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or at your option any later version.
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
open Lm_thread
open Printf

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "HTTP server operations";
        debug_value = false
      }

let eflush out =
   output_char out '\n';
   flush out

let rec print_string_list out sl =
   match sl with
      [s] ->
         output_string out s
    | s :: sl ->
         output_string out s;
         output_string out ", ";
         print_string_list out sl
    | [] ->
         ()

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The server info is just the socket.
 *)
type t = Lm_inet.server

(*
 * Info about the local connection.
 *)
type http_info =
   { http_host : string;
     http_port : int;
     http_password : string
   }

(************************************************************************
 * CONSTANTS                                                            *
 ************************************************************************)

(*
 * Root directory for serving web pages.
 *)
(*
 * string -> path commands
 *)
let set_path _ var path =
   let path' = Lm_string_util.split ":" path in
      var := path'

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let http_path = Env_arg.general "HTTP_PATH" ["."] "HTTP include directories" set_path set_path_arg

(*
 * HTTP response codes.
 *)
let ok_code                     = 200, "OK"
(* unused
let created_code                = 201, "Created"
let accepted_code               = 202, "Accepted"
let no_content_code             = 204, "No Content"
let moved_perm_code             = 301, "Moved Permanently"
let moved_temp_code             = 302, "Moved Temporarily"
let not_modified_code           = 303, "Not modified"
let bad_request_code            = 400, "Bad request"
let unauthorized_code           = 401, "Unauthorized"
let forbidden_code              = 402, "Forbidden"
*)
let not_found_code              = 404, "Not found"
(* unused
let server_error_code           = 500, "Internal server error"
let not_implemented_code        = 501, "Not implemented"
let bad_gateway_code            = 502, "Bad gateway"
let service_unavailable_code    = 503, "Service unavailable"
*)

(*
 * Message that is printed when the file is not found.
 *)
let not_found_msg = "
<html>
<head>
<title>File not found</title>
</head>
<body>
<h1>File is not found<h1>
</body>
</html>"

(*
 * The protocol we use when on 0.9
 *)
let http_protocol = "HTTP/1.0"

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Simple authentication scheme.  Generate a random password,
 * and allow connect requests only for that password.
 *)
let password =
   Lm_string_util.hexify (sprintf "%12.4f" (Unix.gettimeofday ()))

(*
 * Convert two hex chars into a new 8-bit char.
 *)
let unhex c1 c2 =
   let i1 = Lm_string_util.unhex c1 in
   let i2 = Lm_string_util.unhex c2 in
      Char.chr (i1 * 16 + i2)

(*
 * Decode hex characters in the URI.
 *)
let decode_hex uri =
   let len = String.length uri in
   let buf = Bytes.create len in
   let rec convert i j =
      if j = len then
         if i = len then
            buf
         else
            Bytes.sub buf 0 i
      else if uri.[j] = '%' & j < len - 2 then
         begin
            buf.[i] <- unhex uri.[j + 1] uri.[j + 2];
            convert (i + 1) (j + 3)
         end
      else
         begin
            buf.[i] <- uri.[j];
            convert (i + 1) (j + 1)
         end
   in
      Bytes.to_string (convert 0 0)

(*
 * Encode a string into hex.
 *)
(* unused
let hex code =
   if code < 10 then
      Char.chr (code + (Char.code '0'))
   else
      Char.chr (code - 10 + (Char.code 'a'))

let encode_hex uri =
   let len = String.length uri in
   let buf = String.create (3 * len) in
   let rec convert i j =
      if i = len then
         String.sub buf 0 j
      else
         match uri.[i] with
            ('0'..'9' | 'A'..'Z' | 'a'..'z') as c ->
               buf.[j] <- c;
               convert (succ i) (succ j)
          | c ->
               let code = Char.code c in
                  buf.[j] <- '%';
                  buf.[j + 1] <- hex ((code lsr 4) land 15);
                  buf.[j + 2] <- hex (code land 15);
                  convert (succ i) (j + 3)
   in
      convert 0 0
*)

let decode_uri uri =
   let simplified = Lm_filename_util.simplify_path (Lm_filename_util.split_path (decode_hex uri)) in
   let simplified =
      match simplified with
         "nocache" :: _ :: simplified | simplified ->
            simplified
   in
      String.concat "/" simplified

(*
 * Print the content size of the file, and close the connection.
 *)
let head out uri protocol =
   let filename = decode_uri uri in
   let _ =
      if !debug_http then
         eprintf "Editor_http.head %s%t" filename eflush
   in
   let rec search = function
      root :: roots ->
         begin
            try
               let filename = sprintf "%s/%s" root filename in
               let { Unix.st_size = st_size; _ } = Unix.stat filename in
                  if protocol then
                     let code, msg = ok_code in
                        fprintf out "%s %d %s\r\n" http_protocol code msg;
                        fprintf out "Content-Length: %d\r\n\r\n" st_size
            with
               Sys_error _
             | Unix.Unix_error _ ->
                  search roots
         end
    | [] ->
         if !debug_http then
            eprintf "Editor_http.get: failed%t" eflush;
         if protocol then
            let code, msg = not_found_code in
               fprintf out "%s %d %s\r\n" http_protocol code msg
   in
      search !http_path;
      Pervasives.flush out;
      close_out out

(*
 * Copy the input file to the output stream.
 *)
let copy_fd out fd =
   let buffer = String.create 8192 in
   let rec copy () =
      try
         let count = Unix.read fd buffer 0 8192 in
            if count > 0 then
               begin
                  output out buffer 0 count;
                  copy ()
               end
      with
         Unix.Unix_error _
       | Sys_error _ ->
            ()
   in
      copy ()

(*
 * Copy the file to the port.
 *)
let get out uri protocol =
   let filename = decode_uri uri in
   let _ =
      if !debug_http then
         eprintf "Editor_http.get %s%t" filename eflush;
   in
   let rec search = function
      root :: roots ->
         begin
            try
               let filename = sprintf "%s/%s" root filename in
               let { Unix.st_size = st_size; _ } = Unix.stat filename in
               let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
               let _ =
                  if protocol then
                     let code, msg = ok_code in
                        fprintf out "%s %d %s\r\n" http_protocol code msg;
                        fprintf out "Content-Length: %d\r\n\r\n" st_size
               in
                  copy_fd out fd;
                  Unix.close fd
            with
               Sys_error _
             | Unix.Unix_error _ ->
                  search roots
         end
    | [] ->
            (* The file does not exist *)
         let _ =
            if protocol then
               let code, msg = not_found_code in
                  fprintf out "%s %d %s\r\n" http_protocol code msg;
                  fprintf out "Content-Length: %d\r\n\r\n" (String.length not_found_msg)
         in
            fprintf out "%s\n" not_found_msg
   in
      search !http_path;
      Pervasives.flush out;
      close_out out

(*
 * Command line parsing is simple because there
 * is no quoting.
 *)
let parse_args line =
   Lm_string_util.split " \t" line

(*
 * Handle a connection to the server.
 *)
let handle server connect client =
   let fd = Lm_inet.file_descr_of_client client in
   let inx = Unix.in_channel_of_descr fd in
   let outx = Unix.out_channel_of_descr fd in
   let line = input_line inx in
   let _ =
      if !debug_http then
         eprintf "Editor_http.handle: %s%t" line eflush
   in
   let args =
      match parse_args line with
         command :: args ->
            String.lowercase command :: args
       | [] ->
            []
   in
      match args with
         ["get"; uri] ->
            get outx uri false
       | "get" :: uri :: _ :: _ ->
            get outx uri true
       | ["head"; uri] ->
            head outx uri false
       | "head" :: uri :: _ :: _ ->
            head outx uri true
       | "connect" :: pass :: _ ->
            if !debug_http then
               eprintf "Http_server.connect: %s/%s%t" pass password eflush;
            let pass =
               if String.length pass = 0 then
                  pass
               else if pass.[0] = '/' then
                  String.sub pass 1 (pred (String.length pass))
               else
                  pass
            in
               if pass = password then
                  connect server client
               else
                  begin
                     if !debug_http then
                        eprintf "Editor_http.handle: illegal connection with password %s%t" pass eflush;
                     Unix.close fd
                  end
       | _ ->
            if !debug_http then
               eprintf "Editor_http.handle: unknown command: %a%t" print_string_list args eflush;
            Unix.close fd

(*
 * Do the web service.
 *)
let serve connect fd =
   if !debug_http then
      eprintf "Editor_http: starting web services%t" eflush;
   try
      while true do
         let client = Lm_inet.accept fd in
         let fd' = Lm_inet.file_descr_of_client client in
            if !debug_http then
               eprintf "Editor_http: connection on fd=%d%t" (Obj.magic fd') eflush;

            (* Ignore errors when the connection is handled *)
            try handle fd connect client with
               Unix.Unix_error _
             | Sys_error _ ->
                  if !debug_http then
                     eprintf "Editor_http: stopping web services%t" eflush;
                  Unix.close fd'
      done
   with
      Unix.Unix_error _
    | Sys_error _ ->
         if !debug_http then
            eprintf "Editor_http: service closed%t" eflush

(*
 * Set the search path.
 *)
let set_path path =
   http_path := path

(*
 * Start the server on the specified port.
 *)
let start_http connect port =
   let inet = Lm_inet.serve port in
      ignore (Thread.create (serve connect) inet);
      inet

(*
 * Stop the server by closing its socket.
 *)
(* unused
let stop_http inet =
   Lm_inet.close_server inet
*)

(*
 * Server without threads.
 *)
let serve_http connect port =
   let inet = Lm_inet.serve port in
      serve connect inet

(*
 * Get the actual port number.
 *)
let http_info inet =
   let host, port = Lm_inet.get_server_host inet in
      { http_host = host;
        http_port = port;
        http_password = password
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
