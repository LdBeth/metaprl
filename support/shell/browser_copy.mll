(*
 * Copy the text into a buffer, replacing variables of the form %%name%%
 *
 * ----------------------------------------------------------------
 *
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

{
open Format

open Lm_symbol
open Lm_thread

open Http_server_type
open Http_simple

open Browser_sig

(*
 * Build the table of values used in the HTML.
 *)
let title_sym       = Lm_symbol.add "TITLE"
let buttons_sym     = Lm_symbol.add "BUTTONS"
let location_sym    = Lm_symbol.add "LOCATION"
let body_sym        = Lm_symbol.add "BODY"
let host_sym        = Lm_symbol.add "HOST"
let port_sym        = Lm_symbol.add "PORT"
let challenge_sym   = Lm_symbol.add "CHALLENGE"
let response_sym    = Lm_symbol.add "RESPONSE"
let message_sym     = Lm_symbol.add "MESSAGE"
let style_sym       = Lm_symbol.add "STYLE"
let history_sym     = Lm_symbol.add "HISTORY"
let menu_sym        = Lm_symbol.add "MENU"
let session_sym     = Lm_symbol.add "SESSION"
let menu_macros_sym = Lm_symbol.add "MENUMACROS"
let rulebox_sym     = Lm_symbol.add "RULEBOX"
let buttons_macros_sym = Lm_symbol.add "BUTTONSMACROS"
let file_sym        = Lm_symbol.add "FILE"
let content_sym     = Lm_symbol.add "CONTENT"
let basename_sym    = Lm_symbol.add "BASENAME"
let command_sym     = Lm_symbol.add "COMMAND"

(*
 * Open the file as a channel.
 *)
let in_channel_of_file dir name =
   let name = Filename.concat dir name in
      try
         if (Unix.stat name).Unix.st_kind = Unix.S_REG then
            Some (open_in name)
         else
            None
      with
         Sys_error _
       | Unix.Unix_error _ ->
            None

let out_channel_of_file dir name =
   let name = Filename.concat dir name in
      try Some (open_out name) with
         Sys_error _ ->
            None

(*
 * Get an escaped string from a file.
 *)
let string_of_file dir filename =
   let buf = Buffer.create 1024 in
      match in_channel_of_file dir filename with
         Some inx ->
            let rec copy () =
               Buffer.add_char buf (input_char inx);
               copy ()
            in
            let () =
               try copy () with
                  End_of_file ->
                     ()
            in
               close_in inx;
               Buffer.contents buf
       | None ->
           ""

let string_of_lib_file = string_of_file Setup.lib
let string_of_root_file = string_of_file Setup.root

(*
 * Strip DOS-style line-endings.
 *)
let unix_of_dos s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec copy i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
            if c = '\r' && i + 1 < len && s.[i + 1] = '\n' then
               begin
                  Buffer.add_char buf '\n';
                  copy (i + 2)
               end
            else
               begin
                  Buffer.add_char buf c;
                  copy (i + 1)
               end
   in
      copy 0

(*
 * Replace the file with the string.
 *)
let save_root_file filename contents =
   let contents = unix_of_dos contents in
      match out_channel_of_file Setup.root filename with
         Some out ->
            eprintf "save_root_file: saving file %s@." filename;
            output_string out contents;
            close_out out;
            true
       | None ->
            false

(*
 * Output functions.
 *)
type info =
   { info_add_char   : char -> unit;
     info_add_string : string -> unit;
     info_add_buffer : Buffer.t -> unit;
     info_add_fun    : (Buffer.t -> unit) -> unit;
     info_flush      : unit -> unit
   }

(*
 * Buffered output.
 *)
let buffer_info buf =
   { info_add_char   = Buffer.add_char buf;
     info_add_string = Buffer.add_string buf;
     info_add_buffer = Buffer.add_buffer buf;
     info_add_fun    = (fun f -> f buf);
     info_flush      = (fun () -> ())
   }

(*
 * HTTP output.
 *)
let http_info out =
   let add_char c =
      Http_simple.Output.output_char out c;
      if c = '\n' then
         Http_simple.Output.flush out
   in
   let add_string s =
      Http_simple.Output.output_string out s
   in
   let add_buffer buf =
      Http_simple.Output.output_string out (Buffer.contents buf)
   in
   let add_fun f =
      let s =
         let buf = Buffer.create 16 in
            f buf;
            Buffer.contents buf
      in
         Http_simple.Output.output_string out s
   in
   let flush () =
      Http_simple.Output.flush out
   in
      { info_add_char   = add_char;
        info_add_string = add_string;
        info_add_buffer = add_buffer;
        info_add_fun    = add_fun;
        info_flush      = flush
      }

(*
 * Browser table.
 *)
module BrowserTable =
struct
   type t = (info -> unit) SymbolTable.t

   (*
    * Table of buffer functions.
    *)
   let html_table_defaults =
      []

   let empty =
      List.fold_left (fun table (v, s) ->
            SymbolTable.add table v (fun info -> info.info_add_string s)) SymbolTable.empty html_table_defaults

   (*
    * Adding to the table.
    *)
   let add_string table v s =
      SymbolTable.add table v (fun info -> info.info_add_string s)

   let add_buffer table v buffer =
      SymbolTable.add table v (fun info -> info.info_add_buffer buffer)

   let add_file table v filename =
      SymbolTable.add table v (fun info ->
         match in_channel_of_file Setup.root filename with
            Some inx ->
               let rec copy () =
                  info.info_add_char (input_char inx);
                  copy ()
               in
               let () =
                  try copy () with
                     End_of_file ->
                        ()
               in
                  close_in inx
          | None ->
              ())

   let add_fun table v f =
      SymbolTable.add table v (fun info ->
         info.info_add_fun f)

   (*
    * Find and append the value to a buffer.
    *)
   let append_to_buffer info table v =
      (SymbolTable.find table v) info
end

(*
 * OCaml 3.07 allows lexer functions to have arguments.
 * In the meantime, code this imperatively (yuck).
 *)
type lex_info =
   { mutable lex_buffer : info;
     mutable lex_table  : BrowserTable.t
   }

let lex_entry =
   let default =
      { lex_buffer = buffer_info (Buffer.create 1);
        lex_table = BrowserTable.empty
      }
   in
   let fork lex =
      { lex with lex_table = lex.lex_table }
   in
      State.private_val "Browser_copy.lex" default fork

let with_buffer table buf f x =
   State.write lex_entry (fun lex ->
         lex.lex_buffer <- buf;
         lex.lex_table <- table;
         f x)
}

(*
 * Tokens.
 *)
let name = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
let other = [^ '%']+

(*
 * Lexer definition.
 *)
rule main = parse
   "%%" name "%%"
   { let s = Lexing.lexeme lexbuf in
     let name = String.sub s 2 (String.length s - 4) in
     let v = Lm_symbol.add name in
     let lex = State.get lex_entry in
     let () =
        try BrowserTable.append_to_buffer lex.lex_buffer lex.lex_table v with
           Not_found ->
              (* Ignore unbound symbols *)
              ()
     in
        main lexbuf
   }
 | other
   { let s = Lexing.lexeme lexbuf in
     let lex = State.get lex_entry in
        lex.lex_buffer.info_add_string s;
        main lexbuf
   }
 | _
   { let s = Lexing.lexeme lexbuf in
     let lex = State.get lex_entry in
        lex.lex_buffer.info_add_string s;
        main lexbuf
   }
 | eof
   { () }

{
(*
 * Main function.
 *)
let parse table buf inx =
   with_buffer table buf (fun inx ->
      main (Lexing.from_channel inx)) inx

(*
 * Print the contents of a file, with replacement.
 *)
let print_raw_file_to_http out name =
   match in_channel_of_file Setup.lib name with
      Some inx ->
         print_success_channel out OkCode inx;
         close_in inx
    | None ->
         print_error_page out NotFoundCode

let print_metaprl_file_to_http out name =
   match in_channel_of_file Setup.root name with
      Some inx ->
         print_success_channel out OkCode inx;
         close_in inx
    | None ->
         print_error_page out NotFoundCode

(*
 * Print the contents of a file, with replacement.
 *)
let print_translated_file print_success_page print_error_page out table name =
   match in_channel_of_file Setup.lib name with
      Some inx ->
         let buf = Buffer.create 1024 in
         let info = buffer_info buf in
            parse table info inx;
            close_in inx;
            print_success_page out OkCode buf
    | None ->
         print_error_page out NotFoundCode

let print_translated_file_to_http out table name =
   print_translated_file print_success_page print_error_page out table name

let print_translated_file_to_channel out table name =
   let print_success_page out code buf =
      Buffer.output_buffer out buf
   in
   let print_error_page out code =
      raise (Invalid_argument ("File not found: " ^ name))
   in
      print_translated_file print_success_page print_error_page out table name

(*
 * Execute a command and send it to a window.
 *)
let brbuf =
   (* This is a silly hack to get the browser to wake up *)
   let s = String.make 1024 ' ' in
      s.[0] <- '<';
      s.[1] <- 'b';
      s.[2] <- 'r';
      s.[3] <- '>';
      s.[1022] <- '\r';
      s.[1023] <- '\n';
      s

let html_escape_char info col c =
   match c with
      '<' ->
	info.info_add_string "&lt;";
	succ col
    | '>' ->
	info.info_add_string "&gt;";
	succ col
    | '&' ->
	info.info_add_string "&amp;";
	succ col
    | ' ' ->
	info.info_add_string "&nbsp;";
	succ col
    | '\r'
    | '\n' ->
	 info.info_add_string brbuf;
         info.info_flush ();
         0
    | '\t' ->
         let col' = (col + 8) land (lnot 7) in
            for i = col to pred col' do
               info.info_add_string "&nbsp;"
            done;
            col'
    | _ ->
	info.info_add_char c;
	succ col

let print_translated_io_buffer_to_http out table name io =
   match in_channel_of_file Setup.lib name with
      Some inx ->
         let info = http_info out in
         let inp = Browser_syscall.open_in io in
         let print_process buf =
            let rec copy col =
               copy (html_escape_char info col (Browser_syscall.get_char inp))
            in
               try ignore (copy 0) with
                  End_of_file ->
                     ()
         in
         let table = BrowserTable.add_fun table content_sym print_process in
             print_success_header out OkCode;
             parse table info inx;
             close_in inx
    | None ->
         print_error_page out NotFoundCode
}

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
