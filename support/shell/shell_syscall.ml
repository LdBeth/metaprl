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
     mutable shell_dir : string;
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
            let code =
               try Unix.execv Sys.argv.(0) Sys.argv; -1 with
                  Unix.Unix_error _ ->
                     -1
            in
               eprintf "System restart failed@.";
               code
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
     shell_root = Setup.root ();
     shell_dir = "."
   }

let set_syscall_handler f =
   state.shell_handler <- f

(*
 * Perform a system call.
 *)
let exec command =
   state.shell_handler command

(*
 * Get the current path.
 *)
let cwd () =
   Filename.concat state.shell_root state.shell_dir

(*
 * Filname simplification.
 *)
let rec prune_relname path =
   match path with
      ".." :: path ->
         prune_relname path
    | _ ->
         path

let relname s =
   let s = Filename.concat state.shell_dir s in
   let s = Lm_filename_util.split_path s in
   let s = Lm_filename_util.simplify_path s in
   let s = prune_relname s in
      Lm_filename_util.concat_path s

let filenames s =
   let relname = relname s in
   let absname = Filename.concat state.shell_root relname in
      absname, relname

(*
 * List the current directory.
 *)
let deref_ls () =
   exec (SyscallShell (sprintf "ls -ACF %s" (cwd ())))

(*
 * Change the current directory.
 *)
let deref_cd () s =
   let dirname, s = filenames s in
      try
         let stat = Unix.stat dirname in
            if stat.Unix.st_kind = Unix.S_DIR then
               begin
                  state.shell_dir <- s;
                  0
               end
            else
               begin
                  eprintf "Directory %s does not exist@." s;
                  -1
               end
      with
         Unix.Unix_error _ ->
            eprintf "Directory %s does not exist@." s;
            -1

(*
 * Get the current directory.
 *)
let deref_pwd () =
   state.shell_dir

(*
 * Make a directory.
 *)
let deref_mkdir () s =
   let filename, _ = filenames s in
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
   let filename, _ = filenames s in
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
   let filename = relname s in
      exec (SyscallEdit (state.shell_root, filename))

(*
 * Rebuild MetaPRL.
 * The target is either mp.top or mp.opt.
 * We may have to detect native code here...
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
         exec (SyscallCVS (cwd (), command))
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
