(*
 * Read a session file.
 * Sessions have the following format:
 *    [name]
 *    text
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
open Printf

open Lm_thread
open Lm_string_set

open Browser_sig

(*
 * Names of the fields.
 *)
let junk_sym        = "_junk"
let options_sym     = "options"
let directory_sym   = "directory"
let history_sym     = "history"
let edit_sym        = "edit"
let directories_sym = "directories"

(*
 * State has a current name and a table of values.
 *)
type state =
   { mutable state_name  : string;
     mutable state_table : string StringMTable.t
   }

let current_entry =
   let default =
      { state_name  = junk_sym;
        state_table = StringMTable.empty
      }
   in
   let fork state =
      { state with state_name = state.state_name }
   in
      State.private_val "Browser_io.current_entry" default fork
}

(*
 * Tokens.
 *)
let eol     = [' ' '\t' '\r']* '\n'
let comment = '#' [^ '\n']* '\n'
let name    = '[' ['a'-'z' 'A'-'Z' '_' '0'-'9']+ ']' eol
let line    = [^ '\n']+ '\n'

(*
 * Lexer definition.
 *)
rule main = parse
   eol
 | comment
   { main lexbuf }
 | name
   { let name = Lm_string_util.trim (Lexing.lexeme lexbuf) in
     let length = String.length name in
     let name =
        if length > 2 then
           String.sub name 1 (length - 2)
        else
           name
     in
     let state = State.get current_entry in
        state.state_name <- name
   }
 | line
   { let line = Lm_string_util.trim (Lexing.lexeme lexbuf) in
     let state = State.get current_entry in
        state.state_table <- StringMTable.add state.state_table state.state_name line
   }
 | _
   { eprintf "Lexing error\n" }
 | eof
   { () }

{
(*
 * Main function.
 *)
let parse filename =
   let inx = open_in_bin filename in
   let lexbuf = Lexing.from_channel inx in
   let table =
      State.write current_entry (fun state ->
         state.state_name <- junk_sym;
         state.state_table <- StringMTable.empty;
         main lexbuf;
         let table = state.state_table in
            state.state_table <- StringMTable.empty;
            table)
   in
      close_in inx;
      table

(*
 * Read the session info.
 *)
let read_session filename =
   let table = parse filename in
   let dir =
      try StringMTable.find table directory_sym with
         Not_found ->
             "/"
   in
   let options =
      try StringMTable.find table options_sym with
         Not_found ->
            "prR"
   in
   let history =
      try List.rev (StringMTable.find_all table history_sym) with
         Not_found ->
            []
   in
   let edit =
      try List.rev (StringMTable.find_all table edit_sym) with
         Not_found ->
            []
   in
   let directories =
      try List.rev (StringMTable.find_all table directories_sym) with
         Not_found ->
            []
   in
      { session_dir     = dir;
 	session_options = options;
	session_history = history;
	session_edit    = edit;
	session_dirs    = directories
      }

(*
 * Write the session info to a file.
 *)
let write_session filename session =
   let { session_dir     = dir;
	 session_options = options;
         session_history = history;
         session_edit    = edit;
         session_dirs    = dirs
       } = session
   in
   let outx = open_out_bin filename in
      fprintf outx "# Session version 1.0\r\n";

      (* Directory *)
      fprintf outx "[%s]\r\n%s\r\n\r\n" directory_sym dir;

      (* Options *)
      fprintf outx "[%s]\r\n%s\r\n" options_sym options;

      (* Edit *)
      fprintf outx "[%s]\r\n" edit_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) edit;
      fprintf outx "\r\n";

      (* Directories *)
      fprintf outx "[%s]\r\n" directories_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) dirs;
      fprintf outx "\r\n";

      (* History *)
      fprintf outx "[%s]\r\n" history_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) history;
      fprintf outx "\r\n";

      close_out outx
}

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
