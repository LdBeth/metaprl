(*
 * This takes care of creating shells, and load/save of sessions.
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
open Lm_thread
open Lm_rprintf

open Line_buffer

open Dform

open Shell_sig
open Shell_util
open Shell_core
open Session_io
open Session_sig
open Shell_internal_sig
open Session_current

(************************************************************************
 * Formatting.
 *)

(*
 * Formatting of string buffers.
 *)
let strings_of_linebuffer queue =
   let strings =
      LineBuffer.fold (fun strings s ->
            String.escaped s :: strings) [] queue
   in
      List.rev strings

(*
 * Format buffers.
 *)
let strings_of_linebuffer_buffers queue =
   let buffers =
      List.rev (LineBuffer.fold (fun buffers buf ->
                      buf :: buffers) [] queue)
   in
   let s = Lm_rformat.marshal_buffers buffers in
   let len = String.length s in
   let rec collect strings off =
      let amount = min (len - off) 32 in
         if amount = 0 then
            List.rev strings
         else
            let s = Lm_string_util.hexify_sub s off amount in
               collect (s :: strings) (off + amount)
   in
      collect [] 0

(*
 * Add the contents of a file to a (string LineBuffer.t)
 *)
let add_linebuffer_elements queue elems =
   List.iter (LineBuffer.add queue) elems

(*
 * Unescape the strings.
 *)
let add_linebuffer_strings queue strings =
   List.iter (fun s -> LineBuffer.add queue (Lm_string_util.unescape s)) strings

(*
 * Unmarshal the buffers.
 *)
let add_linebuffer_buffers queue strings =
   (* Reconstruct the string *)
   let s =
      let buf = Buffer.create 32 in
         List.iter (fun s ->
               Buffer.add_string buf (Lm_string_util.unhexify s)) strings;
         Buffer.contents buf
   in
      try add_linebuffer_elements queue (Lm_rformat.unmarshal_buffers s) with
         Failure _
       | Invalid_argument _ ->
            ()

(*
 * Save the contents of a (string LineBuffer.t) to a file in the user's
 * home directory.
 *)
let strings_of_linetable queue =
   let strings =
      LineTable.fold (fun strings dir subdir ->
            Printf.sprintf "%s#%s" dir subdir :: strings) [] queue
   in
      List.rev strings

(*
 * Add the contents of a file to a (string LineBuffer.t)
 *)
let linetable_of_strings strings =
   List.fold_left (fun table line ->
         let dir, subdir =
            try
               let index = String.index line '#' in
               let dir = String.sub line 0 index in
               let subdir = String.sub line (succ index) (String.length line - index - 1) in
                  dir, subdir
            with
               Not_found ->
                  line, ""
         in
            LineTable.add table dir subdir) LineTable.empty strings

(************************************************************************
 * Current shell.
 *)

(*
 * Shell entry.
 *)
let default_shell =
   let port = None in
   let dfmode = "prl" in
   let display_mode = DisplayText (default_mode_base, dfmode) in
   let proof = Shell_root.create packages display_mode in
      { shell_id             = 1;
        shell_label          = "current";
        shell_width          = 80;
        shell_df_mode        = dfmode;
        shell_dir            = DirRoot;
        shell_package        = None;
        shell_proof          = proof;
        shell_needs_refresh  = false
      }

let fork_shell shell =
   let { shell_proof = old_proof } = shell in
      { shell with shell_label = "subsession";
                   shell_proof = old_proof.edit_copy ();
                   shell_needs_refresh = true;
                   shell_id = new_session_id ()
      }

let shell_entry = State.private_val "Shell_current.shell_entry" default_shell fork_shell

(*
 * Set the current entry from a session.
 *)
let set_current_session session_info =
   State.write shell_entry (fun shell ->
   State.write session_entry (fun session ->
         let { session_info_id       = id;
               session_info_dir      = dir;
               session_info_options  = options;
               session_info_history  = history;
               session_info_edit     = edit;
               session_info_dirs     = dirs;
               session_info_messages = messages
             } = session_info
         in
            shell.shell_id              <- id;
            shell.shell_dir             <- dir_of_path dir;
            shell.shell_needs_refresh   <- true;
            add_linebuffer_strings session.session_history history;
            add_linebuffer_buffers session.session_messages messages;
            session.session_directories <- linetable_of_strings dirs;
            session.session_files       <- linetable_of_strings edit;
            session.session_options     <- ls_options_of_string options))

(*
 * Load all the shells.
 *)
let () =
   (* Load all the previous sessions *)
   let sessions = read_sessions () in

   (* Find the session to assign to the current state *)
   let rec search_current rest sessions =
      match sessions with
         session :: sessions ->
            if session.session_info_label = "current" then
               session, List.rev_append rest sessions
            else
               search_current (session :: rest) sessions
       | [] ->
            match rest with
               session :: sessions ->
                  session, sessions
             | [] ->
                  let session =
                     { session_info_id       = Lm_thread_shell.get_pid ();
                       session_info_label    = "current";
                       session_info_dir      = [];
                       session_info_options  = string_of_ls_options ls_options_default;
                       session_info_history  = [];
                       session_info_edit     = [];
                       session_info_dirs     = [];
                       session_info_messages = []
                     }
                  in
                     session, rest
   in
   let session, sessions = search_current [] sessions in
      (* Assign the current session *)
      set_current_session session;

      (* For the rest, create new states *)
      List.iter (fun session ->
            let pid = Lm_thread_shell.create false in
               Lm_thread_shell.with_pid pid (fun () ->
                     set_current_session session) ()) sessions

(*
 * Create the session and save it.
 *)
let flush () =
   State.read shell_entry (fun shell ->
   State.read session_entry (fun session ->
         let { shell_id          = id;
               shell_label       = label;
               shell_dir         = dir
             } = shell
         in
         let { session_history     = history;
               session_directories = dirs;
               session_files       = files;
               session_options     = options;
               session_messages    = messages
             } = session
         in
         let session =
            { session_info_id       = id;
              session_info_dir      = path_of_dir dir;
              session_info_label    = label;
              session_info_options  = string_of_ls_options options;
              session_info_history  = strings_of_linebuffer history;
              session_info_edit     = strings_of_linetable files;
              session_info_dirs     = strings_of_linetable dirs;
              session_info_messages = strings_of_linebuffer_buffers messages
            }
         in
            write_session session))

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
