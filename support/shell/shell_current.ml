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

open Refiner.Refiner.RefineError

open Shell_sig
open Shell_util
open Shell_core
open Session_io
open Session_sig
open Shell_internal_sig
open Session_current

(************************************************************************
 * Process identifiers.
 *)

(*
 * Format strings.
 *)
let string_of_pid pid =
   let id, i = Lm_thread_shell.dest_pid pid in
      sprintf "%s.%d" id i

let pid_of_string s =
   try
      let index = String.rindex s '.' in
      let id = String.sub s 0 index in
      let i = int_of_string (String.sub s (succ index) (String.length s - index - 1)) in
         id, i
   with
      Failure _
    | Not_found ->
         raise (RefineError ("Shell_current.pid_of_string", StringStringError ("illegal process identifier", s)))

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
   let s = Lm_rformat.marshal_buffers LineBuffer.version
           buffers in
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
      try add_linebuffer_elements queue
          (Lm_rformat.unmarshal_buffers LineBuffer.version s) with
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

(*
 * Save the contents of a (string LineBuffer.t) to a file in the user's
 * home directory.
 *)
let strings_of_file_info queue =
   let strings =
      LineTable.fold (fun strings dir point ->
            Printf.sprintf "%s#%d" dir point :: strings) [] queue
   in
      List.rev strings

(*
 * Add the contents of a file to a (string LineBuffer.t)
 *)
let file_info_of_strings strings =
   List.fold_left (fun table line ->
         let dir, point =
            match String.split_on_char '#' line with
               dir :: point :: _ ->
                  let point =
                     try int_of_string point with
                        Failure _ ->
                           0
                  in
                     dir, point
             | [dir] ->
                  dir, 0
             | [] ->
                  line, 0
         in
            LineTable.add table dir point) LineTable.empty strings

(************************************************************************
 * Current shell.
 *)

(*
 * Shell entry.
 *)
let default_shell =
   let df =
      { df_mode  = "prl";
        df_base  = default_base;
        df_width = 80;
        df_type  = DisplayText;
      }
   in
   let shell =
      { shell_debug          = "root";
        shell_fs             = DirRoot;
        shell_subdir         = [];
        shell_package        = None;
        shell_proof          = Shell_root.create packages (fun _ -> df);
        shell_needs_refresh  = false;
        shell_df_method      = df
      }
   in shell.shell_proof <- Shell_root.create packages
                           (fun _ -> shell.shell_df_method);
      shell

let index_entry = State.shared_val "Shell_current.shell_debug" (ref 0)

let fork_shell shell =
   let index =
      State.write index_entry (fun indexp ->
            let index = !indexp in
               indexp := succ index;
               index)
   in
      { shell with shell_debug = Printf.sprintf "%s.%d" shell.shell_debug index;
                   shell_proof = shell.shell_proof#edit_copy;
                   shell_needs_refresh = true
      }

let shell_entry = State.private_val "Shell_current.shell_entry" default_shell fork_shell

(*
 * Set the current entry from a session.
 *)
let set_current_session session_info =
   State.write shell_entry (fun shell ->
   State.write session_entry (fun session ->
      let fs, subdir =
         try dir_of_path session_info.session_info_dir with
            Not_found
          | Failure _ ->
               DirRoot, []
      in
         shell.shell_fs              <- fs;
         shell.shell_subdir          <- subdir;
         shell.shell_needs_refresh   <- true;
         add_linebuffer_strings session.session_history session_info.session_info_history;
         add_linebuffer_buffers session.session_messages session_info.session_info_messages;
         session.session_options     <- ls_options_of_string session_info.session_info_options))

let set_shared shared_info =
   State.write shared_entry (fun shared ->
         shared.shared_directories <- linetable_of_strings shared_info.shared_info_dirs;
         shared.shared_files       <- file_info_of_strings shared_info.shared_info_files)

(*
 * Load the shared data.
 *)
let () =
   set_shared (read_shared ())

(*
 * Load all the shells.
 *)
let restore_sessions () =
   (* Load all the previous sessions *)
   let sessions = read_sessions () in
      List.iter (fun session ->
            try
               let id, i = pid_of_string session.session_info_id in
               let pid = Lm_thread_shell.create_or_find id i Lm_thread_shell.VisibleJob in
                  Lm_thread_shell.with_pid pid (fun () ->
                        set_current_session session) ()
            with
               RefineError _ ->
                  ()) sessions

(*
 * Create the session and save it.
 *)
let flush () =
   State.read shell_entry (fun shell ->
   State.read session_entry (fun session ->
   State.read shared_entry (fun shared ->
      let pid = string_of_pid (Lm_thread_shell.get_pid ()) in
      let shared =
         { shared_info_files    = strings_of_file_info shared.shared_files;
           shared_info_dirs     = strings_of_linetable shared.shared_directories
         }
      in
      let session =
         { session_info_id       = pid;
           session_info_dir      = path_of_dir (shell.shell_fs, shell.shell_subdir);
           session_info_options  = string_of_ls_options session.session_options;
           session_info_history  = strings_of_linebuffer session.session_history;
           session_info_messages = strings_of_linebuffer_buffers session.session_messages;
         }
      in
         write_shared shared;
         write_session session)))

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
