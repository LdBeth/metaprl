(*
 * Typical external commands.
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
extends Mptop

open Lm_format

open Shell_syscall_sig

(*
 * The state is shared across all sessions.
 *)
type state =
   { mutable shell_handler : syscall -> int;
     shell_root : string
   }

(*
 * Default system call handler.
 *)
let handle_syscall command =
   let perform_command command =
      Shell_command.backup_all ();
      eprintf "+ %s@." command;
      try
         match Unix.system command with
            Unix.WEXITED code ->
               code
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _ ->
               -1
      with
         Unix.Unix_error _ ->
            -1
   in
      match command with
         SyscallRestart ->
            Shell_current.flush ();
            (try Unix.execv Sys.argv.(0) Sys.argv with
                Unix.Unix_error (errno, funinfo, arginfo) ->
                   eprintf "Execv failed: %s %s(%s)@." (Unix.error_message errno) funinfo arginfo;
                   -1)
       | SyscallOMake target ->
            perform_command (sprintf "omake %s" target)
       | SyscallGit (cwd, command) ->
            perform_command (sprintf "cd %s && git %s" cwd command)
       | SyscallEdit (root, target) ->
            perform_command (sprintf "%s %s" (Setup.editor ()) (Filename.concat root target))
       | SyscallShell s ->
            perform_command s

let state =
   { shell_handler = handle_syscall;
     shell_root = Setup.root ()
   }

let set_syscall_handler f =
   state.shell_handler <- f

(*
 * Perform a system call.
 *)
let exec command =
   state.shell_handler command

(*
 * Get the filename relative to the root.
 *)
let rec prune_relname path =
   match path with
      ".." :: path ->
         prune_relname path
    | _ ->
         path

(*
 * Get the filename relative to the current FS directory.
 *)
let rootname s =
   let s =
      if String.length s >= 3 && String.sub s 0 3 = "/fs" then
         String.sub s 3 (String.length s - 3)
      else
         s
   in
   let s = Lm_filename_util.split_path s in
   let s = Lm_filename_util.simplify_path s in
   let s = prune_relname s in
      Lm_filename_util.concat_path s

(*
 * Get the filename relative to the working directory.
 *)
let edit_of_root s =
   Filename.concat state.shell_root s

let editname s =
   edit_of_root (rootname s)

(*
 * Make a directory.
 *)
let deref_mkdir () s =
   let filename = editname s in
      try Unix.mkdir filename 0o777; 0 with
         Unix.Unix_error (error, msg, code) ->
            if code = "" then
               eprintf "%s: %s@." msg (Unix.error_message error)
            else
               eprintf "%s(%s): %s@." msg code (Unix.error_message error);
            -1

(*
 * Remove a file.
 *)
let deref_rm () s =
   let filename = editname s in
      try Unix.unlink filename; 0 with
         Unix.Unix_error (error, msg, code) ->
            if code = "" then
               eprintf "%s: %s@." msg (Unix.error_message error)
            else
               eprintf "%s(%s): %s@." msg code (Unix.error_message error);
            -1

(*
 * Edit a file.
 *)
let deref_edit () s =
   let filename = rootname s in
      exec (SyscallEdit (state.shell_root, filename))

(*
 * Rebuild MetaPRL.
 * The target is either mp.top or mp.opt.
 *)
let deref_omake () =
   let target1 = Filename.basename Sys.argv.(0) in
   let target2 =
      match Sys.backend_type with
         Sys.Bytecode | Sys.Other _ ->
            "mp.top"
       | Sys.Native ->
            "mp.opt"
   in
      if target1 <> target2 then begin
         eprintf "@[<v 3>Unexpected name of the MetaPRL executable:@ Command line: %s@ Expected: %s@]@." target1 target2;
         raise (Invalid_argument "deref_omake")
      end else
         exec (SyscallOMake target1)

(*
 * Restart metaprl.
 * XXX BUG: how do we close all file descriptors?
 *)
let deref_restart () =
   exec SyscallRestart

(*
 * Subversion commands.
 *)
let deref_git () command =
   exec (SyscallGit (editname ".", command))

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
