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

open Http_server_type

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "HTTP server operations";
        debug_value = false
      }

let http_threads = Env_arg.int "threads" 4 "number of threads for the web service" Env_arg.set_int_int

(*
 * Allow the socket to be inherited from the parent process.
 *)
let socket_name = "socket"

let socket_fd = Env_arg.int socket_name None "socket descriptor (for restart)" Env_arg.set_int_option_int

let (_: unit ref) =
   let setter _ _ = Setup.sethostname in
      Env_arg.string Setup.hostname_var () "Hostname to use in browser URLs" setter

(*
 * IO.
 *)
let eflush out =
   output_char out '\n';
   flush out

(*
 * Localhost has this common address.
 *)
let localhost_addr =
   Unix.inet_addr_of_string "127.0.0.1"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The server info is just the socket.
 *)
type t = Lm_ssl.t

(*
 * The output is a SSL channel.
 *)
module Input =
struct
   type t = Lm_ssl.ssl_in
end

module Output =
struct
   type t = Lm_ssl.ssl_out

   let close = Lm_ssl.close_out
   let output_char = Lm_ssl.output_char
   let output_string = Lm_ssl.output_string
   let flush = Lm_ssl.flush
end

(*
 * Info about the local connection.
 *)
type http_info =
   { http_host     : string;
     http_port     : int
   }

(*
 * Handler types.
 *)
type 'a start_handler = t -> 'a -> 'a
type 'a connect_handler = t -> 'a -> Output.t -> Input.t -> string list -> request_header_entry list -> string -> 'a

(************************************************************************
 * CONSTANTS                                                            *
 ************************************************************************)

(*
 * HTTP response codes.
 *)
let continue_code               = 100, "Continue"
let ok_code                     = 200, "OK"
let created_code                = 201, "Created"
let accepted_code               = 202, "Accepted"
let no_content_code             = 204, "No Content"
let moved_perm_code             = 301, "Moved Permanently"
let moved_temp_code             = 302, "Moved Temporarily"
let see_other_code              = 303, "See Other"
let not_modified_code           = 304, "Not modified"
let bad_request_code            = 400, "Bad request"
let unauthorized_code           = 401, "Unauthorized"
let forbidden_code              = 402, "Forbidden"
let not_found_code              = 404, "Not found"
let server_error_code           = 500, "Internal server error"
let not_implemented_code        = 501, "Not implemented"
let bad_gateway_code            = 502, "Bad gateway"
let service_unavailable_code    = 503, "Service unavailable"

let get_code = function
   ContinueCode ->           continue_code
 | OkCode ->                 ok_code
 | CreatedCode ->            created_code
 | AcceptedCode ->           accepted_code
 | NoContentCode ->          no_content_code
 | MovedPermCode ->          moved_perm_code
 | MovedTempCode ->          moved_temp_code
 | SeeOtherCode ->           see_other_code
 | NotModifiedCode ->        not_modified_code
 | BadRequestCode ->         bad_request_code
 | UnauthorizedCode ->       unauthorized_code
 | ForbiddenCode ->          forbidden_code
 | NotFoundCode ->           not_found_code
 | ServerErrorCode ->        server_error_code
 | NotImplementedCode ->     not_implemented_code
 | BadGatewayCode ->         bad_gateway_code
 | ServiceUnavailableCode -> service_unavailable_code

(*
 * The protocol we use when on 0.9
 *)
let http_protocol = "HTTP/1.0"

(*
 * Print a success code.
 *)
let print_success_page_err out code buf =
   let code, msg = get_code code in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;
      Lm_ssl.fprintf out "Cache-Control: no-cache\r\n";
      Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n" (Buffer.length buf);
      Lm_ssl.output_buffer out buf

let print_success_header_err out code =
   let code, msg = get_code code in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;
      Lm_ssl.fprintf out "Cache-Control: no-cache\r\n\r\n"

let print_content_page_err out code content_type buf =
   let code, msg = get_code code in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;
      Lm_ssl.fprintf out "Cache-Control: no-cache\r\n";
      Lm_ssl.fprintf out "Content-Type: %s\r\n" content_type;
      Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n" (Buffer.length buf);
      Lm_ssl.output_buffer out buf

(*
 * Print a file.
 *)
let copy outx inx =
   let buffer = Bytes.create 8192 in
   let rec copy () =
      let count = input inx buffer 0 8192 in
         if count <> 0 then
            begin
               Lm_ssl.output outx (Bytes.to_string buffer) 0 count;
               copy ()
            end
   in
      copy ()

let print_gmtime outx time =
   let { Unix.tm_sec = sec;
         Unix.tm_min = min;
         Unix.tm_hour = hour;
         Unix.tm_mday = mday;
         Unix.tm_mon  = mon;
         Unix.tm_year = year;
         Unix.tm_wday = wday;
         _
       } = Unix.gmtime time
   in
   let mon_names = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
   let day_names = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|] in
      Lm_ssl.fprintf outx "%s, %02d %s %d %02d:%02d:%02d GMT" day_names.(wday) mday mon_names.(mon) (1900+year) hour min sec

let print_success_channel_err out code inx =
   let code, msg = get_code code in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;

      (* This silly exception block is because Win32 doesn't support fstat *)
      (try
          let fd = Unix.descr_of_in_channel inx in
          let { Unix.st_size = st_size;
                Unix.st_mtime = st_mtime;
                _
              } = Unix.fstat fd
          in
             Lm_ssl.fprintf out "Last-Modified: %a\r\n" print_gmtime st_mtime;
             Lm_ssl.fprintf out "Cache-Control: public\r\n";
             Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n" st_size
       with
          Unix.Unix_error _
        | Invalid_argument _ ->
             Lm_ssl.fprintf out "\r\n");

      copy out inx

let print_content_channel_err out code content_type inx =
   let code, msg = get_code code in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;

      (* This silly exception block is because Win32 doesn't support fstat *)
      (try
          let fd = Unix.descr_of_in_channel inx in
          let { Unix.st_size = st_size;
                Unix.st_mtime = st_mtime;
                _
              } = Unix.fstat fd
          in
             Lm_ssl.fprintf out "Last-Modified: %a\r\n" print_gmtime st_mtime;
             Lm_ssl.fprintf out "Cache-Control: public\r\n";
             Lm_ssl.fprintf out "Content-Type: %s\r\n" content_type;
             Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n" st_size
       with
          Unix.Unix_error _
        | Invalid_argument _ ->
             Lm_ssl.fprintf out "\r\n");

      copy out inx

(*
 * For errors, construct a message body.
 *)
let print_error_page_err out code =
   let code, msg = get_code code in
   let buf = sprintf "<html>
<head>
<title>%s</title>
</head>
<body>
<h1>%s</h1>
</body>
</html>" msg msg in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;
      Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n%s" (String.length buf) buf

(*
 * Redirect.
 *)
let print_redirect_page_err out code where =
   let code, msg = get_code code in
   let buf = sprintf "<html>
<head>
<title>%s</title>
</head>
<body>
You are being redirected to <a href=\"%s\"><tt>%s</tt></a>
</body>
</html>" msg where where
   in
      Lm_ssl.fprintf out "%s %d %s\r\n" http_protocol code msg;
      Lm_ssl.fprintf out "Location: %s\r\n" where;
      Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n%s" (String.length buf) buf

(*
 * Catch sigpipe.
 *)
exception SigPipe = Lm_ssl.SSLSigPipe

let print_success_header out code =
   try print_success_header_err out code with
      SigPipe ->
         ()

let print_success_page out code buf =
   try print_success_page_err out code buf with
      SigPipe ->
         ()

let print_content_page out code content_type buf =
   try print_content_page_err out code content_type buf with
      SigPipe ->
         ()

let print_success_channel out code inx =
   try print_success_channel_err out code inx with
      SigPipe ->
         ()

let print_content_channel out code content_type inx =
   try print_content_channel_err out code content_type inx with
      SigPipe ->
         ()

let print_error_page out code =
   try print_error_page_err out code with
      SigPipe ->
         ()

let print_redirect_page out code where =
   try print_redirect_page_err out code where with
      SigPipe ->
         ()

let catch_sigpipe () =
   if Sys.os_type <> "Win32" then
      let handle_sigpipe _ =
         if !debug_http then
            eprintf "Raised sigpipe@.";
         raise SigPipe
      in
         Sys.set_signal Sys.sigpipe (Sys.Signal_handle handle_sigpipe)

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

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
      else if uri.[j] = '+' then
         begin
            buf.[i] <- ' ';
            convert (i + 1) (j + 1)
         end
      else if uri.[j] = '%' && j < len - 2 then
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
let hex code =
   if code < 10 then
      Char.chr (code + (Char.code '0'))
   else
      Char.chr (code - 10 + (Char.code 'a'))

let encode_hex uri =
   let len = String.length uri in
   let buf = Bytes.create (3 * len) in
   let rec convert i j =
      if i = len then
         Bytes.sub buf 0 j
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
      Bytes.to_string (convert 0 0)

let decode_uri uri =
   Lm_filename_util.simplify_path (Lm_filename_util.split_path (decode_hex uri))

(*
 * Command line parsing is simple because there
 * is no quoting.
 *)
let parse_args line =
   Lm_string_util.split " \t" line

(************************************************************************
 * Input handling.
 *)

(*
 * Form handler.
 *)
let colon = ':'

let header_line inx =
   let line = Lm_ssl.input_line inx in
      try
         let index = String.index line colon in
         let length = String.length line in
            if index < length then
               let tag = String.lowercase (String.sub line 0 index) in
               let field = String.sub line (succ index) (length - index - 1) in
                  Some (tag, Lm_string_util.trim field)
            else
               None
      with
         Not_found ->
            None

let rec read_header inx lines =
   try
      match header_line inx with
         Some (tag, field) ->
            if !debug_http then
               eprintf "Http_simple.header line: %s: %s%t" tag field eflush;
            read_header inx ((tag, field) :: lines)
       | None ->
            lines
   with
      End_of_file ->
         lines

(*
 * Get the next header line from the string.
 *)
type string_header =
   StringEntry of string * string * int
 | StringOffset of int
 | StringNone

let rec find_colon s i len =
   if i = len then
      raise Not_found
   else if s.[i] = colon then
      i
   else
      find_colon s (succ i) len

let string_header_line line i =
   try
      let length = String.index_from line i '\n' in
         try
            let index = find_colon line i length in
            let tag = String.lowercase (String.sub line i (index - i)) in
            let field = String.sub line (succ index) (length - index - 1) in
               StringEntry (tag, Lm_string_util.trim field, succ length)
         with
            Not_found ->
               StringOffset (succ length)
   with
      Not_found ->
         StringNone

let string_read_header part =
   let rec split lines i =
      match string_header_line part i with
         StringEntry (tag, field, i) ->
            if !debug_http then
               eprintf "Http_simple.string_header line: %s: %s%t" tag field eflush;
            split ((tag, field) :: lines) i
       | StringOffset i ->
            i, lines
       | StringNone ->
            i, lines
   in
      split [] 0

(*
 * Header parsing.
 *)
let parse_int s =
   try int_of_string s with
      Failure _ ->
         0

let parse_host s =
   match Lm_string_util.parse_args s with
      [host] ->
         host, None
    | host :: port :: _ ->
         host, Some (parse_int port)
    | _ ->
         "unknown-host", None

let parse_if_match s =
   match Lm_string_util.parse_args s with
      ["*"] ->
         None
    | args ->
         Some args

let parse_request_cache_control field =
   match Lm_string_util.parse_args (String.lowercase field) with
      ["no-cache"] ->
         CacheRequestNoCache
    | ["no-store"] ->
         CacheRequestNoStore
    | ["max-age"; field] ->
         CacheRequestMaxAge (parse_int field)
    | ["max-stale"; field] ->
         CacheRequestMaxStale (parse_int field)
    | ["min-fresh"; field] ->
         CacheRequestMinFresh (parse_int field)
    | ["no-transform"] ->
         CacheRequestNoTransform
    | ["only-if-cached"] ->
         CacheRequestOnlyIfCached
    | [token] ->
         CacheRequestExtension1 token
    | [token1; token2] ->
         CacheRequestExtension2 (token1, token2)
    | _ ->
         CacheRequestExtension0 field

type uri_state =
   StateProto
 | StateHost
 | StatePort
 | StateFile

let colon = ':'

let parse_uri s =
   let len = String.length s in
   let proto = Buffer.create 7 in
   let host = Buffer.create 64 in
   let port = Buffer.create 6 in
   let file = Buffer.create 128 in
   let rec search state j =
      if j <> len then
         let c = s.[j] in
         let j' = succ j in
            match state with
               StateProto ->
                  if c = colon then
                     search StateHost j'
                  else
                     (Buffer.add_char proto c; search StateProto j')
             | StateHost ->
                  if c = colon then
                     search StatePort j'
                  else if c = '/' then
                     search StateFile j'
                  else
                     (Buffer.add_char host c; search StateHost j')
             | StatePort ->
                  if c = '/' then
                     search StateFile j'
                  else
                     (Buffer.add_char port c; search StatePort j')
             | StateFile ->
                  Buffer.add_char file c;
                  search StateFile j'
   in
   let _ = search StateProto 0 in
      { uri_proto = Buffer.contents proto;
        uri_host = Buffer.contents host;
        uri_port =
           (let i = parse_int (Buffer.contents port) in
               if i = 0 then
                  None
               else
                  Some i);
        uri_path = Buffer.contents file
      }

let split_eq_val arg =
   let arg = Lm_string_util.trim arg in
      try
         let index = String.index arg '=' in
         let length = String.length arg in
            if index < length then
               let tag = String.sub arg 0 index in
               let tag = decode_hex tag in
               let field = String.sub arg (succ index) (length - index - 1) in
               let field = decode_hex field in
                  if !debug_http then
                     eprintf "Http_simple.post_body: %s=%s%t" tag (String.escaped field) eflush;
                  tag, field
            else
               arg, ""
      with
         Not_found ->
            arg, ""

let parse_eq_list sep body =
      if !debug_http then
         eprintf "parse_post_body: \"%s\"%t" (String.escaped body) eflush;
      List.map split_eq_val (Lm_string_util.split sep body)

let parse_cookies body =
   parse_eq_list ";" body

(*
 * Content-type.
 *)
let parse_content_type body =
   let parts = Lm_string_util.split ";" body in
      match parts with
         info :: params ->
            let main, sub =
               match Lm_string_util.split "/" info with
                  main :: sub :: _ ->
                     main, sub
                | [main] ->
                     main, ""
                | [] ->
                     "", ""
            in
            let params =
               List.map (fun arg ->
                     let key, x = split_eq_val arg in
                        String.lowercase key, x) params
            in
               { content_type_main   = String.lowercase main;
                 content_type_sub    = String.lowercase sub;
                 content_type_params = params
               }
       | [] ->
            { content_type_main = "";
              content_type_sub  = "";
              content_type_params = []
            }

(*
 * Content-disposition.
 *)
let parse_content_disposition body =
   let parts = Lm_string_util.split ";" body in
      match parts with
         info :: params ->
            let params =
               List.map (fun arg ->
                     let key, x = split_eq_val arg in
                        String.lowercase key, x) params
            in
               { content_disposition_type = String.lowercase info;
                 content_disposition_params = params
               }
       | [] ->
            { content_disposition_type = "";
              content_disposition_params = []
            }

(*
 * HTTP header.
 *)
let parse_header_entry (tag, field) =
   match tag with
      "accept" ->
         RequestAccept field
    | "accept-charset" ->
         RequestAcceptCharset field
    | "accept-encoding" ->
         RequestAcceptEncoding field
    | "accept-language" ->
         RequestAcceptLanguage field
    | "authorization" ->
         RequestAuthorization field
    | "cache-control" ->
         RequestCacheControl (parse_request_cache_control field)
    | "connection" ->
         RequestConnection field
    | "content-type" ->
         RequestContentType (parse_content_type field)
    | "content-length" ->
         RequestContentLength (parse_int field)
    | "content-disposition" ->
         RequestContentDisposition (parse_content_disposition field)
    | "cookie" ->
         RequestCookies (parse_cookies field)
    | "date" ->
         RequestDate (float_of_int (Ctime.parse_time field))
    | "expect" ->
         RequestExpect
    | "from" ->
         RequestFrom field
    | "host" ->
         let host, port = parse_host field in
            RequestHost (host, port)
    | "if-match" ->
         RequestIfMatch (parse_if_match field)
    | "if-modified-since" ->
         RequestIfModifiedSince (float_of_int (Ctime.parse_time field))
    | "if-none-match" ->
         RequestIfNoneMatch field
    | "if-range" ->
         RequestIfRange field
    | "if-unmodified-since" ->
         RequestIfUnmodifiedSince (float_of_int (Ctime.parse_time field))
    | "max-forwards" ->
         RequestMaxForwards (parse_int field)
    | "pragma" ->
         RequestCacheControl CacheRequestNoCache
    | "proxy-authorization" ->
         RequestProxyAuthorization field
    | "referer" ->
         RequestReferer (parse_uri field)
    | "te" ->
         RequestTE field
    | "trailer" ->
         RequestTrailer field
    | "transfer-encoding" ->
         RequestTransferEncoding
    | "upgrade" ->
         RequestUpgrade (Lm_string_util.parse_args field)
    | "user-agent" ->
         RequestUserAgent field
    | "via" ->
         RequestVia field
    | "warning" ->
         RequestWarning field
    | _ ->
         RequestExtension (tag, field)

let input_header inx =
   List.map parse_header_entry (read_header inx [])

(*
 * Each part has a header and a value.
 *)
let rec get_content_disposition header =
   match header with
      RequestContentDisposition disp :: _ ->
         disp
    | _ :: rest ->
         get_content_disposition rest
    | [] ->
         raise Not_found

let parse_post_body part i =
   let len = String.length part in
   let len =
      if len - i >= 2 && part.[len - 2] = '\r' && part.[len - 1] = '\n' then
         len - 2
      else
         len
   in
      String.sub part i (len - i)

let parse_post_part part =
   if !debug_http then
      eprintf "Http_simple.parse_post_part: %s%t" (String.escaped part) eflush;
   let i, header = string_read_header part in
   let header = List.map parse_header_entry header in
      match get_content_disposition header with
         { content_disposition_type = "form-data";
           content_disposition_params = params
         } ->
            let name =
               try List.assoc "name" params with
                  Not_found ->
                     raise (Failure "parse_post_body: form-data has no name")
            in
            let name = Lm_string_util.unescape name in
            let body = parse_post_body part i in
               if !debug_http then
                  eprintf "Http_simple.parse_post_part: %s = %s%t" name (String.escaped body) eflush;
               name, body
       | { content_disposition_type = dtype; _ } ->
            raise (Failure ("parse_post_body: bad Content-Disposition: " ^ dtype))

let parse_post_multipart boundary body =
   if !debug_http then
      eprintf "Http_simple.parse_post_multipart: %s%t" (String.escaped body) eflush;
   let parts = Lm_string_util.split_mime_string boundary body in
      List.map parse_post_part parts

let parse_post_body content_type body =
   match content_type with
      { content_type_main = "application";
        content_type_sub  = "x-www-form-urlencoded";
        _
      } ->
         parse_eq_list "&" body

    | { content_type_main = "multipart";
        content_type_sub  = "form-data";
        content_type_params = params
      } ->
         let boundary =
            try List.assoc "boundary" params with
               Not_found ->
                  raise (Failure "parse_post_body: boundary is not defined")
         in
            if !debug_http then
               begin
                  List.iter (fun (name, x) ->
                        eprintf "parse_post_body: %s=%s%t" name x eflush) params;
                  eprintf "parse_post_body.boundary = %s%t" boundary eflush
               end;
            parse_post_multipart boundary body

    | { content_type_main = main;
        content_type_sub = sub;
        _
      } ->
         raise (Failure ("parse_post_body: unknown content type: " ^ main ^ "/" ^ sub))

(*
 * POST body.
 *)
let rec find_content_length = function
   h :: t ->
      (match h with
          RequestContentLength i ->
             i
        | _ ->
             find_content_length t)
 | [] ->
      raise Not_found

let read_body inx header =
   try
      let length = find_content_length header in
      let body = Bytes.create length in
         if !debug_http then
            eprintf "Http_simple.read_body: trying to read %d chars%t" length eflush;
         Lm_ssl.really_input inx body 0 length;
         let str = Bytes.to_string body in
            if !debug_http then
               eprintf "Http_simple.read_body: %d, %s%t" length
                  (String.escaped str) eflush;
         str
   with
      Not_found
    | Failure _
    | End_of_file ->
         ""

(************************************************************************
 * Server main.
 *)

(*
 * Save the server filedescriptor in the environment,
 * so that we will re-use it on restart.
 *)
let save_http server =
   let _, port = Lm_ssl.getsockname server in
      Env_arg.putenv socket_name (string_of_int (Lm_ssl.fd server));
      port

let close_http server =
   Lm_ssl.close server

(*
 * Handle a connection to the server.
 *)
let handle server client connect info =
   let _ =
      if !debug_http then
         eprintf "Http_simple.handle: begin%t" eflush
   in
   let inx  = Lm_ssl.in_channel_of_ssl client in
   let outx = Lm_ssl.out_channel_of_ssl client in
   let line = Lm_ssl.input_line inx in
   let _ =
      if !debug_http then
         eprintf "Http_simple.handle: %s%t" line eflush
   in
   let args =
      match parse_args line with
         command :: args ->
            String.lowercase command :: args
       | [] ->
            []
   in
   let header = input_header inx in
   let body = read_body inx header in
   let _ =
      if !debug_http then
         eprintf "Http_simple.handle: %s%t" line eflush
   in
   let state = connect server info outx inx args header body in
      Lm_ssl.flush outx;
      state

(*
 * Do the web service.
 *)
let serve connect server info =
   if !debug_http then
      eprintf "Http_simple: starting web services%t" eflush;

   (* Catch sigpipe *)
   let () = catch_sigpipe () in

   (* Serve as a secure connection *)
   let rec serve info =
      let client =
         try Some (Lm_ssl.accept server) with
            Unix.Unix_error _
          | Sys_error _
          | Failure _
          | SigPipe as exn ->
               eprintf "Http_simple: %s: accept failed%t" (Printexc.to_string exn) eflush;
               None
      in
      let info =
         match client with
            None ->
               info
          | Some client ->
               let info =
                  (* Ignore errors when the connection is handled *)
                  try handle server client connect info with
                     Sys_error _
                   | Failure _
                   | End_of_file
                   | SigPipe as exn ->
                        eprintf "Http_simple: %s: error during web service%t" (Printexc.to_string exn) eflush;
                        info
                   | Unix.Unix_error (errno, funinfo, arginfo) ->
                        eprintf "Http_simple: Unix error: %s: %s(%s)%t" (Unix.error_message errno) funinfo arginfo eflush;
                        info
               in
               let () =
                  try Lm_ssl.shutdown client with
                     Failure _
                   | SigPipe as exn ->
                        eprintf "Http_simple: %s: shutdown failed%t" (Printexc.to_string exn) eflush
               and () =
                  try Lm_ssl.close client with
                      Failure _
                    | SigPipe as exn ->
                         eprintf "Http_simple: %s: close failed%t" (Printexc.to_string exn) eflush
               in
                  info
      in
         serve info
   in
      try serve info with
         exn ->
            eprintf "Http_simple: %s: service closed%t" (Printexc.to_string exn) eflush;
            raise exn

(*
 * Server without threads.
 *)
let serve_http start connect info port =
   let passwd_name = Setup.server_pem () in
   let dh_name = Setup.dh_pem () in
   let inet_addr =
      if Lm_ssl.enabled then
         Unix.inet_addr_any
      else
         localhost_addr
   in
   let ssl =
      match !socket_fd with
         Some fd ->
            Lm_ssl.serve fd passwd_name dh_name
       | None ->
            let ssl = Lm_ssl.socket passwd_name in
               Lm_ssl.bind ssl inet_addr port;
               Lm_ssl.listen ssl dh_name 10;
               ssl
   in
   let info = start ssl info in
      if Thread.enabled then
         for _i = 2 to !http_threads do
            ignore (Thread.create (serve connect ssl) info)
         done;
      try
         serve connect ssl info
      with exn ->
         close_http ssl;
         raise exn

(*
 * Get the string name of an addr.
 *)
let string_of_inet_addr addr =
   if addr = Unix.inet_addr_any then
      Setup.gethostname ()
   else if addr = localhost_addr then
      "localhost"
   else
      raise (Failure "Http_simple.string_of_inet_addr: internal error")

(*
 * Get the actual port number.
 *)
let http_info ssl =
   let host, port = Lm_ssl.getsockname ssl in
      { http_host = string_of_inet_addr host;
        http_port = port
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
