(*
 * This is the standard interface to the window system.
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
open Lm_string_set
open Lm_rformat
open Lm_format
open Lm_rformat_html
open Lm_thread

open Line_buffer

open Shell_util
open Session_sig
open Session_current

(*
 * Ls options.
 *)
let get_view_options () =
   State.read session_entry (fun session ->
         session.session_options)

let set_view_options options =
   State.write session_entry (fun session ->
         session.session_options <- options)

let add_view_options options =
   State.write session_entry (fun session ->
         session.session_options <- ls_options_add session.session_options options)

let clear_view_options options =
   State.write session_entry (fun session ->
         session.session_options <- ls_options_clear session.session_options options)

(*
 * Get the HTML tagger from the current state.
 *)
let get_tagger session =
   let tagger =
      if LsOptionSet.mem session.session_options LsHandles then
         (fun s -> Printf.sprintf "<span class=\"slot\" id=\"%s\">&#8227;" s)
      else
         (fun s -> Printf.sprintf "<span class=\"slot\" id=\"%s\">" s)
   in
      Some { html_tag_begin = FunTagger tagger;
             html_tag_end = StringTagger "</span>"
      }

(*
 * Simplify invis strings.
 *)
let format_invis buf s =
   format_izone buf;
   format_string buf s;
   format_ezone buf

(*
 * Add the prompt to the output box.
 *)
let add_prompt str =
   State.write session_entry (fun session ->
         let buffer = new_buffer () in
            format_invis buffer "<b># ";
            format_string buffer str;
            format_invis buffer "</b><br>\n";
            LineBuffer.add session.session_messages buffer;

            (* Add to the history only if the command is different *)
            match LineBuffer.last session.session_history with
               Some line ->
                  if str <> line then
                     LineBuffer.add session.session_history str
             | None ->
                  LineBuffer.add session.session_history str)

(*
 * Add a file.
 *)
let absname name =
   Filename.concat (Setup.root ()) name

let rec resolve_symlink name =
   let absname = absname name in
   let stat = Unix.lstat absname in
      if stat.Unix.st_kind = Unix.S_LNK then
         resolve_symlink (Unix.readlink absname)
      else if stat.Unix.st_kind = Unix.S_REG then
         absname
      else
         raise Not_found

let resolve_symlink filename =
   try Some (resolve_symlink filename) with
      Unix.Unix_error _
    | Not_found ->
         None

let strip_root filename =
   match filename with
      Some filename ->
          let root = Setup.root () ^ "/" in
          let root_len = String.length root in
          let file_len = String.length filename in
          let convert c =
             if c = '\\' then
                '/'
             else
                c
          in
          let rec matches i =
             if i = root_len then
                true
             else
                convert root.[i] = convert filename.[i] && matches (succ i)
          in
          let file =
             if root_len < file_len && matches 0 then
                String.sub filename root_len (file_len - root_len)
             else
                filename
          in
             Some file
    | None ->
         None

let add_edit_point_internal shared name point =
   shared.shared_files <- LineTable.add shared.shared_files name point

let add_edit_internal shared name =
   add_edit_point_internal shared name 0

let add_filename_internal shared name =
   match name with
      Some name ->
         add_edit_internal shared name
    | None ->
         ()

let add_file_internal shared file =
   match strip_root file with
      Some name ->
         let basename =
            try String.sub name 0 (String.rindex name '.') with
               Not_found ->
                  name
         in
         let ml_name = strip_root (resolve_symlink (basename ^ ".ml")) in
         let mli_name = strip_root (resolve_symlink (basename ^ ".mli")) in
            add_filename_internal shared ml_name;
            add_filename_internal shared mli_name
    | None ->
         ()

let add_edit_point name point =
   State.write shared_entry (fun shared ->
         add_edit_point_internal shared name point)

let add_edit name =
   State.write shared_entry (fun shared ->
         add_edit_internal shared name)

let add_file file =
   State.write shared_entry (fun shared ->
         add_file_internal shared file)

(*
 * Add a directory.  For the buffer, remember the main part of the directory
 * as well as the current location.
 *)
let add_directory str =
   State.write shared_entry (fun shared ->
         (* Parse the filename *)
         let path =
            match Lm_string_util.split "/" str with
               [""; ""] ->
                  []
             | "" :: path ->
                  path
             | path ->
                  path
         in
            match path with
               [] ->
                  ()
             | [dir] ->
                  shared.shared_directories <- LineTable.add shared.shared_directories dir ""
             | group :: md :: subdir ->
                  let dir = group ^ "/" ^ md in
                     if (subdir <> []) || not (LineTable.mem shared.shared_directories dir) then
                        shared.shared_directories <- LineTable.add shared.shared_directories dir (String.concat "/" subdir))

(*
 * Capture output channels.
 *)
let add_channel message color =
   let font = Printf.sprintf "<span class=\"%s\">" color in
      (fun buf ->
            if not (Lm_rformat.buffer_is_empty buf) then
               let buffer = new_buffer () in
                  format_invis buffer font;
                  format_buffer buffer buf;
                  format_invis buffer "</span>";
                  LineBuffer.add message buffer)

(*
 * Format it.
 *)
let format_message width buf =
   State.read session_entry (fun session ->
         let tagger = get_tagger session in
            LineBuffer.iter (fun buffer ->
                  Lm_rformat_html.print_html_buffer width tagger buffer buf) session.session_messages)

(*
 * Get the history.
 *)
let get_history () =
   State.read session_entry (fun session ->
         let history =
            LineBuffer.fold (fun lines line ->
                  line :: lines) [] session.session_history
         in
            List.rev history)

(*
 * Get the files.
 *)
let get_files () =
   State.read shared_entry (fun shared ->
         let files =
            LineTable.fold (fun files file _ ->
                  file :: files) [] shared.shared_files
         in
            List.rev files)

(*
 * Get the info.
 *)
let get_edit_point name =
   State.read shared_entry (fun shared ->
         try LineTable.find shared.shared_files name with
            Not_found ->
               0)

(*
 * Get the directories.
 *)
let get_directories () =
   State.read shared_entry (fun shared ->
         let dirs =
            LineTable.fold (fun lines dir subdir ->
                  if subdir = "" then
                     ("/" ^ dir) :: lines
                  else
                     (sprintf "/%s/%s" dir subdir) :: ("/" ^ dir) :: lines) [".."; "~"; "/"] shared.shared_directories
         in
            List.rev dirs)

(*
 * Get the term matched by the id.
 *)
let get_term id =
   State.read session_entry (fun session ->
         StringTable.find session.session_content_table id)

(*
 * Display a term in the window.
 *)
let set_main buf terms =
   let session = State.get session_entry in
      session.session_content_buffer <- buf;
      session.session_content_table <- terms

let format_main width buf =
   State.read session_entry (fun session ->
         let tagger = get_tagger session in
            Lm_rformat_html.print_html_buffer width tagger session.session_content_buffer buf)

(*
 * Divert output during this call.
 *)
let synchronize f x =
   State.write session_entry (fun session ->
         Lm_format.divert std_formatter (Some (add_channel session.session_messages "stdout"));
         Lm_format.divert err_formatter (Some (add_channel session.session_messages "stderr"));
         let result =
            try f x with
               exn ->
                  Lm_format.divert std_formatter None;
                  Lm_format.divert err_formatter None;
                  raise exn
         in
            Lm_format.divert std_formatter None;
            Lm_format.divert err_formatter None;
            result)

(*
 * Perform the command, and add the output to the message window.
 *)
let add_command io =
   State.write session_entry (fun session ->
         let s =
            Browser_syscall.flush io;
            Browser_syscall.contents io
         in
         let buffer = new_buffer () in
            format_invis buffer "<span class=\"system\">\n";
            format_invis buffer (Lm_string_util.html_escaped s);
            format_invis buffer "\n</span>\n";
            LineBuffer.add session.session_messages buffer)

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
