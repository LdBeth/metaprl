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
let buttons_macros_sym = Lm_symbol.add "BUTTONSMACROS"

(*
 * Browser table.
 *)
module BrowserTable : BrowserTableSig =
struct
   (*
    * Table of buffer functions.
    *)
   type t = (Buffer.t -> unit) SymbolTable.t


   let html_table_defaults =
      []

   let empty =
      List.fold_left (fun table (v, s) ->
            SymbolTable.add table v (fun buf -> Buffer.add_string buf s)) SymbolTable.empty html_table_defaults

   (*
    * Adding to the table.
    *)
   let add_string table v s =
      SymbolTable.add table v (fun buf -> Buffer.add_string buf s)

   let add_buffer table v buffer =
      SymbolTable.add table v (fun buf -> Buffer.add_buffer buf buffer)

   let add_fun = SymbolTable.add

   (*
    * Find and append the value to a buffer.
    *)
   let append_to_buffer buf table v =
      (SymbolTable.find table v) buf
end

(*
 * OCaml 3.07 allows lexer functions to have arguments.
 * In the meantime, code this imperatively.
 *)
let output_buffer = ref (Buffer.create 1)
let symbol_table = ref BrowserTable.empty

let with_buffer table buf f x =
   let old_buf = !output_buffer in
   let old_table = !symbol_table in
      symbol_table := table;
      output_buffer := buf;
      f x;
      output_buffer := old_buf;
      symbol_table := old_table
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
     let v = Lm_symbol.add (String.sub s 2 (String.length s - 4)) in
     let () =
        try BrowserTable.append_to_buffer !output_buffer !symbol_table v with
           Not_found ->
              (* Ignore unbound symbols *)
              ()
     in
        main lexbuf
   }
 | other
   { let s = Lexing.lexeme lexbuf in
        Buffer.add_string !output_buffer s;
        main lexbuf
   }
 | _
   { let s = Lexing.lexeme lexbuf in
        Buffer.add_string !output_buffer s;
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
 * Open the file as a channel.
 *)
let mplib =
   try Some (Sys.getenv "MPLIB") with
      Not_found ->
         eprintf "Shell_browser.print_file: the MPLIB environment variable is not defined@.";
         None

let channel_of_file name =
   match mplib with
      None ->
         None
    | Some mplib ->
         let name = Filename.concat mplib name in
            try
               if (Unix.stat name).Unix.st_kind = Unix.S_REG then
                  Some (open_in name)
               else
                  None
            with
               Sys_error _ | Unix.Unix_error _ ->
                  None

(*
 * Print the contents of a file, with replacement.
 *)
let print_raw_file_to_http out name =
   match channel_of_file name with
      Some inx ->
         print_success_channel out OkCode inx
    | None ->
         print_error_page out NotFoundCode

(*
 * Print the contents of a file, with replacement.
 *)
let print_translated_file print_success_page print_error_page out table name =
   match channel_of_file name with
      Some inx ->
         let buf = Buffer.create 1024 in
            parse table buf inx;
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
}

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
