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

open Session_sig

(*
 * Flushing.
 *)
let eflush out =
   output_char out '\n';
   flush out

(*
 * Names of the fields.
 *)
let junk_sym        = "_junk"
let options_sym     = "options"
let directory_sym   = "directory"
let history_sym     = "history"
let edit_sym        = "edit"
let directories_sym = "directories"
let messages_sym    = "messages"

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

(*
 * Parse failures.
 *)
exception ParseError
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
        state.state_name <- name;
     main lexbuf
   }
 | line
   { let line = Lm_string_util.trim (Lexing.lexeme lexbuf) in
     let state = State.get current_entry in
        state.state_table <- StringMTable.add state.state_table state.state_name line;
        main lexbuf
   }
 | _
   { eprintf "Syntax error in session file%t" eflush;
     raise ParseError
   }
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
         try
            main lexbuf;
            let table = state.state_table in
               state.state_table <- StringMTable.empty;
               table
         with
            exn ->
               close_in inx;
               raise exn)
   in
      close_in inx;
      table

(*
 * Make a directory, but don't complain if it already exists.
 *)
let mkdir name =
   try Unix.mkdir name 0o700 with
      Unix.Unix_error _ ->
         ()

(*
 * Session directory name.
 *)
let shared_filename, session_dir =
   let home =
       try Sys.getenv "HOME" with
          Not_found ->
             eprintf "HOME environment variable not defined, using .%t" eflush;
             "."
   in
   let metaprl_dir = Filename.concat home ".metaprl" in
   let session_dir = Filename.concat metaprl_dir "sessions" in
   let shared_name = Filename.concat metaprl_dir "shared" in
      mkdir metaprl_dir;
      mkdir session_dir;
      shared_name, session_dir

(*
 * Filename for a session.
 *)
let filename_of_id id =
   Filename.concat session_dir id

(*
 * Session filenames in a directory.
 *)
let session_ids () =
   let dir = Unix.opendir session_dir in
   let rec collect ids =
      let name =
         try Some (Unix.readdir dir) with
            End_of_file ->
               None
      in
         match name with
            Some name ->
              collect (name :: ids)
          | None ->
              ids
    in
    let ids = collect [] in
       Unix.closedir dir;
       ids

(*
 * Read the session info.
 *)
let read_shared () =
   let table =
	try Some (parse shared_filename) with
	   Sys_error _ ->
              None
   in
      match table with
         Some table ->
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
               { shared_info_files    = edit;
                 shared_info_dirs     = directories
               }
       | None ->
            { shared_info_files = [];
              shared_info_dirs  = []
            }

(*
 * Write the session info to a file.
 *)
let write_shared session =
   let { shared_info_files    = edit;
         shared_info_dirs     = dirs
       } = session
   in
   let outx = open_out_bin shared_filename in
      fprintf outx "# Session version 1.0\r\n";

      (* Edit *)
      fprintf outx "[%s]\r\n" edit_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) edit;
      fprintf outx "\r\n";

      (* Directories *)
      fprintf outx "[%s]\r\n" directories_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) dirs;
      fprintf outx "\r\n";

      close_out outx

(*
 * Read the session info.
 *)
let read_session id =
   let table = parse (filename_of_id id) in
   let dir =
      try List.rev (StringMTable.find_all table directory_sym) with
         Not_found ->
             []
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
   let messages =
      try List.rev (StringMTable.find_all table messages_sym) with
         Not_found ->
            []
   in
      { session_info_id       = id;
        session_info_dir      = dir;
        session_info_options  = options;
        session_info_history  = history;
        session_info_messages = messages
      }

(*
 * Read all the sessions in the directory.
 *)
let read_sessions () =
   let rec collect sessions ids =
      match ids with
         id :: ids ->
            let sessions =
               try read_session id :: sessions with
                  Unix.Unix_error _
                | Sys_error _
                | ParseError ->
                      sessions
            in
               collect sessions ids
       | [] ->
             sessions
   in
        collect [] (session_ids ())

(*
 * Write the session info to a file.
 *)
let write_session session =
   let { session_info_id       = id;
         session_info_dir      = dir;
         session_info_options  = options;
         session_info_history  = history;
         session_info_messages = messages
       } = session
   in
   let outx = open_out_bin (filename_of_id id) in
      fprintf outx "# Session version 1.0\r\n";
      fprintf outx "# Session %s\r\n\r\n" id;

      (* Options *)
      fprintf outx "[%s]\r\n%s\r\n\r\n" options_sym options;

      (* Directory *)
      fprintf outx "[%s]\r\n" directory_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) dir;
      fprintf outx "\r\n";

      (* History *)
      fprintf outx "[%s]\r\n" history_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) history;
      fprintf outx "\r\n";

      (* Messages *)
      fprintf outx "[%s]\r\n" messages_sym;
      List.iter (fun s -> fprintf outx "%s\r\n" s) messages;
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
