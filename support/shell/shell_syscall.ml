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

open Lm_config
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
 * Compiler target.
 *)
let target =
   let target1 = Filename.basename Sys.argv.(0) in
   let target2 =
      match Lm_config.code with
         ByteCode ->
            "mp.top"
       | NativeCode ->
            "mp.opt"
   in
      if target1 <> target2 then
         eprintf "@[<v 3>Current command targets differ:@ Command line: %s@ Expected: %s@]@." target1 target2;
      target1

(*
 * Default system call handler.
 *)
let handle_syscall command =
   let perform_command command =
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
            Shell.backup_all ();
            (try Unix.execv Sys.argv.(0) Sys.argv; -1 with
                Unix.Unix_error (errno, funinfo, arginfo) ->
                   eprintf "Execv failed: %s %s(%s)@." (Unix.error_message errno) funinfo arginfo;
                   -1)
       | SyscallOMake target ->
            perform_command (sprintf "omake %s" target)
       | SyscallCVS (cwd, command) ->
            perform_command (sprintf "cd %s && cvs %s" cwd command)
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

let absname s =
   let cwd = Shell.relative_pwd () in
   let s = Filename.concat cwd s in
   let s = Lm_filename_util.split_path s in
   let s = Lm_filename_util.simplify_path s in
   let s = prune_relname s in
      Lm_filename_util.concat_path s

(*
 * Get the filename relative to the working directory.
 *)
let relname s =
   Filename.concat state.shell_root s

(*
 * Make a directory.
 *)
let deref_mkdir () s =
   let filename = relname s in
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
   let filename = relname s in
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
   let filename = absname s in
      exec (SyscallEdit (state.shell_root, filename))

(*
 * Rebuild MetaPRL.
 * The target is either mp.top or mp.opt.
 *)
let deref_omake () =
   exec (SyscallOMake target)

(*
 * Restart metaprl.
 * BUG: how do we close all file descriptors?
 *)
let deref_restart () =
   exec SyscallRestart

(*
 * CVS commands.
 *)
let deref_cvs () command =
   match command with
      "up"
    | "update"
    | "co"
    | "commit" ->
         exec (SyscallCVS (absname ".", command))
    | _ ->
         eprintf "CVS command '%s' not allowed@." command;
         -1

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
