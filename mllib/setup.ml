(*
 * Values for MetaPRL environment variables.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2004 MetaPRL Group, Caltech
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
 * Authors: Jason Hickey <jyh@cs.caltech.edu>
 *          Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Unix
open Lm_printf
open Lm_thread

(*
 * Environment variables are prefixed with this string.
 *)
let environ_prefix = "MP"

let confname = "metaprl"

let delay name f =
   let handle = State.shared_val name (ref None) in
   let writer = function
      { contents = Some name } -> name
    | ref ->
         let res = f () in
            ref := Some res;
            res
   in
      fun () -> State.write handle writer

(* $(MP_ROOT) *)
let root =
    let writer () =
      let name = environ_prefix ^ "_ROOT" in
         try Sys.getenv name with
            Not_found ->
               raise(Invalid_argument ("Setup: the environment variable " ^ name ^ " is not defined"))
   in
      delay "Setup.root" writer

(* $(MPLIB) *)
let lib =
   let writer () =
      let name = environ_prefix ^ "LIB" in
         try Sys.getenv name with
            Not_found ->
               begin
                  (* Fall back to $(MP_ROOT)/lib, but only if directory exists and is accessible *)
                  let lib = Filename.concat (root ()) "lib" in
                  try
                     closedir(opendir lib);
                     lib
                  with
                     _ ->
                        raise (Invalid_argument ("Setup: " ^ name ^ " environment variable must be defined"))
               end
   in
      delay "Setup.lib" writer

(* $(HOME)/.metaprl *)
let home =
   let writer () =
      let home =
         try Filename.concat (Sys.getenv "HOME") ("." ^ confname) with
            Not_found ->
               let home =
                  if Sys.os_type = "Win32" then
                     Filename.concat "C:" confname
                  else
                     sprintf "/tmp/%s-%i" confname (getuid ())
               in
                  eprintf "@[<v 3>WARNING!   Please set the HOME environment variable to point@WARNING!   to your home directory.@ WARNING!   Using %s in place of $(HOME)/.%s for now.@]@." home confname;
                  home
      in
      let new_setup =
         (* Does "home" dir exist already? *)
         try access home [F_OK]; false with
            Unix_error _ ->
               begin
                  eprintf "@[<v 3>WARNING!  MetaPRL state directory %s does not exist, creating.@]@." home;
                  try mkdir home 0o700 with
                     Unix_error _ -> ()
               end;
               true
      in
         begin (* Make sure home is read/write *)
            try access home [F_OK;R_OK;W_OK]; closedir(opendir home); with
               Unix_error _ ->
                  raise (Invalid_argument ("Setup: Please make sure that " ^ home ^ " is a directory for which MetaPRL has read and write access"))
         end;
         if new_setup then begin
            (*
             * XXX TODO (nogin): Here we should add code that would ask all kind of questions
             * about user's preferences.
             *)
            ()
         end;
         home
   in
      delay "Setup.home" writer

let editor =
   let writer () =
      let file = Filename.concat (home ()) "editor" in
         try
            let inx = open_in file in
            let editor = input_line inx in
               close_in inx;
               editor
         with
            Sys_error _
          | End_of_file ->
               if Sys.os_type = "Win32" then
                  "notepad.exe"
               else
                  let editor =
                     try Sys.getenv "EDITOR" with
                        Not_found ->
                           "vi"
                  in
                     sprintf "xterm -e %s" editor
   in
      delay "Setup.editor" writer

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
