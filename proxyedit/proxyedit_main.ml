(*
 * Proxy editor.  This takes a file from the browser,
 * invokes an external editor, then uses a "put" operation
 * to upload the file to the server.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Printf
open Proxyedit_lex

let eflush out =
   output_char out '\n';
   flush out

(*
 * Divert input and output channels.
 *)
let () =
   if Sys.os_type = "Win32" then
      eprintf "Don't worry about this window.\nIt is an artifact of Win32 that displays error messages.\nIt will go away when you finish editing your file.%t" eflush
   else
      let fd = Unix.openfile "/tmp/medit.out" [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
         Unix.dup2 fd Unix.stdout;
         Unix.dup2 fd Unix.stderr;
         Unix.close Unix.stdin;
         eprintf "Hello%t" eflush

exception SigPipe = Lm_ssl.SSLSigPipe

let catch_sigpipe () =
   if Sys.os_type <> "Win32" then
      let handle_sigpipe _ =
         raise SigPipe
      in
         Sys.set_signal Sys.sigpipe (Sys.Signal_handle handle_sigpipe)

(*
 * Info about the file being edited.
 *)
type info =
   { info_host     : string;
     info_port     : int;
     info_name     : string;
     info_filename : string;
     info_passwd   : string;
     info_keyfile  : string;
     info_content  : string
   }

let print_info out info =
   let { info_host     = host;
         info_port     = port;
         info_name     = name;
         info_filename = filename;
         info_passwd   = passwd;
         info_keyfile  = keyfile;
         info_content  = content
       } = info
   in
      fprintf out "host = %s\n" host;
      fprintf out "port = %d\n" port;
      fprintf out "name = %s\n" name;
      fprintf out "filename = %s\n" filename;
      fprintf out "passwd = %s\n" passwd;
      fprintf out "keyfile = %s\n" keyfile;
      fprintf out "content = %s\n" content

(*
 * Connect to the server.
 *)
let connect info keyfile =
   let { info_host = machine;
         info_port = port;
       } = info
   in
   let fd = Lm_ssl.socket keyfile in
   let () =
      try
         let addr = Unix.inet_addr_of_string machine in
            Lm_ssl.connect fd addr port
      with
         Failure _ ->
            let addrs = (Unix.gethostbyname info.info_host).Unix.h_addr_list in
            let len = Array.length addrs in
            let rec search exn i =
               if i = len then
                  raise exn
               else
                  try
                     let addr = addrs.(i) in
                        Lm_ssl.connect fd addr port
                  with
                     exn ->
                        search exn (succ i)
            in
            let exn = Unix.Unix_error (Unix.EAFNOSUPPORT, "connect", machine) in
               search exn 0
   in
      fd

(*
 * Send the file back to the server.
 *)
let put_file info =
   let { info_name = name;
         info_filename = filename;
         info_keyfile = key;
         info_passwd = passwd
       } = info
   in

   (* Save the password into a temporary file *)
   let keyfile, outx = Filename.open_temp_file "proxyedit" ".pem" in
   let () =
      output_string outx key;
      close_out outx
   in
   let fd = connect info keyfile in
   let () =
      try Unix.unlink keyfile with
         Unix.Unix_error _ ->
            ()
   in
   let out = Lm_ssl.out_channel_of_ssl fd in
   let in_length = (Unix.stat filename).Unix.st_size in
   let in_file = open_in_bin filename in

   (* Send the request *)
   let () =
      Lm_ssl.fprintf out "put %s please\r\n" name;
      Lm_ssl.fprintf out "Cookie: MetaPRL.response=%s\r\n" passwd;
      Lm_ssl.fprintf out "Content-Length: %d\r\n\r\n" in_length;
   in

   (* Copy the remaining text to the server *)
   let rec copy () =
      Lm_ssl.output_char out (input_char in_file);
      copy ()
   in
   let () =
      try copy () with
         End_of_file ->
            ()
   in
   let () =
      close_in in_file;
      Lm_ssl.flush out
   in

   (* Read the result *)
   let inx = Lm_ssl.in_channel_of_ssl fd in
   let line = Lm_ssl.input_line inx in
   let code = Scanf.sscanf line "%s %d" (fun _ code -> code) in
   let () =
      Lm_ssl.shutdown fd;
      Lm_ssl.close fd
   in
      if code < 200 || code >= 300 then
         raise (Failure "put failed")

(*
 * Edit a file.
 *)
let edit_file info =
   let { info_name = name;
         info_filename = filename;
         info_content = content
       } = info
   in

   (* Save the file *)
   let () =
      let outx = open_out filename in
         output_string outx content;
         close_out outx
   in
   let digest = Digest.file filename in

   (* Edit it until we can save it *)
   let editor = Setup.editor ()
   let command = sprintf "%s %s" editor filename in
   let exit_ok =
      match Unix.system command with
         Unix.WEXITED 0 ->
            Digest.file filename <> digest
       | _ ->
            false
   in
      if exit_ok then
         try put_file info with
            exn ->
               eprintf "Put_file: %s failed%t" name eflush;

               (* Create an error file *)
               let errorfile = filename ^ ".txt" in
               let outx = open_out errorfile in
                  fprintf outx "!!! Unable to upload the file to MetaPRL !!!\n";
                  fprintf outx "!!! %s !!!\n" (Printexc.to_string exn);
                  fprintf outx "!!! Your file has been saved !!!\n%s\n" filename;
                  close_out outx;
                  ignore (Unix.system (sprintf "%s %s" editor errorfile))

(*
 * Parse arguments.
 *)
let () =
   catch_sigpipe ();
   match Sys.argv with
      [|_; filename|] ->
         let args = parse_file filename in
(*
         let () =
            try Unix.unlink filename with
               Unix.Unix_error _ ->
                  ()
         in
*)

         (* Choose a filename *)
         let name = List.assoc "name" args in
         let basename = Filename.basename name in
         let root, suf =
               try
                  let index = String.rindex basename '.' in
                  let root = String.sub basename 0 index in
                  let suf = String.sub basename index (String.length basename - index) in
                     root, suf
               with
                  Not_found ->
                     basename, ".txt"
         in
         let filename =
            try
               let tmp = Filename.concat Setup.home "tmp" in
               let () =
                  try Unix.mkdir tmp 0o777 with
                     Unix.Unix_error _ ->
                        ()
               in
                  Filename.concat tmp basename
            with
               Not_found ->
                  Filename.temp_file root suf
         in
         let info =
            { info_host     = List.assoc "host" args;
              info_port     = int_of_string (List.assoc "port" args);
              info_name     = name;
              info_filename = filename;
              info_passwd   = List.assoc "passwd" args;
              info_keyfile  = List.assoc "keyfile" args;
              info_content  = List.assoc "content" args
            }
         in
            edit_file info
    | _ ->
         eprintf "usage: medit <filename>\n";
         exit 1

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
