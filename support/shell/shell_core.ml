(*
 * Basic shell functions that do not depend on a toploop.
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
open Lm_debug
open Lm_rprintf

open Opname
open Precedence
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape
open Refiner.Refiner.RefineError
open Dform

open Shell_sig
open Shell_util
open Shell_internal_sig

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

(*
 * Turn a path to an absolute string.
 *)
let rec string_of_path = function
   [n] ->
      "/" ^ n
 | h::t ->
      "/" ^ h ^ string_of_path t
 | [] ->
      "/"

let mk_dep_name opname =
   "/" ^ String.concat "/" (List.rev (dest_opname opname))

(*
 * We should skip the packages that do not have basic shell commands in them.
 *)
let shell_package pkg =
   let name = Package_info.name pkg in
      try Mptop.mem (Mptop.get_toploop_resource (Mp_resource.find (Mp_resource.theory_bookmark name)) []) "cd" with
         Not_found ->
            false

(*
 * All loaded modules.
 *)
let packages = Package_info.create (Shell_state.get_includes ())

let all_packages () =
   List.filter shell_package (Package_info.packages packages)

let default_mode_base = Mp_resource.theory_bookmark "summary"

(*
 * Get the current "prl" printing base.
 *)
let get_dfbase info =
   match info.shell_package with
      Some mod_info ->
         if !debug_shell then
            eprintf "Selecting display forms from %s%t" (Package_info.name mod_info) eflush;
         Mp_resource.theory_bookmark (Package_info.name mod_info)
    | None ->
         if !debug_shell then
            eprintf "Restoring default display forms%t" eflush;
         default_mode_base

let get_display_mode info =
   let dfbase = get_dfbase info in
      match info.shell_df_mode with
         "tex" ->
            DisplayTex dfbase
       | "html" ->
            DisplayBrowser dfbase
       | mode ->
            DisplayText (dfbase, mode)

let get_db info =
   let dfbase = get_dfbase info in
      get_mode_base dfbase info.shell_df_mode

let set_dfmode info mode =
   info.shell_df_mode <- mode

(*
 * Get the resource collection.
 *)
let get_resource info =
   match info.shell_package with
      Some mod_info ->
         Mp_resource.find (Mp_resource.theory_bookmark (Package_info.name mod_info))
    | None ->
         raise Not_found

(*
 * Get the current package.
 *)
let get_current_package shell =
   match shell.shell_package with
      Some pack ->
         pack
    | None ->
         raise (RefineError ("Shell.get_current_package", StringError "no current package"))

(*
 * Update the timestamp.
 *)
let touch shell =
   let pack = get_current_package shell in
      try Package_info.touch pack with
         Failure _ ->
            begin
               (* Change the status so that we can write to the file. *)
               Package_info.set_status pack PackModified;
               Package_info.touch pack
            end

(************************************************************************
 * VIEWING                                                              *
 ************************************************************************)

(*
 * Turn a string into a path, relative to info.dir.
 * The string is "/"-separated; "." means current directory, ".." its
 * parent, "..." its grandparent etc.
 * Also, "~" refers to the second level from top, e.g. cd "~" goes to
 * the current module.
 *)
let parse_path info name =
   let home =
      match info.shell_dir with
         [] ->
            []
       | modname :: _ ->
            [modname]
   in
   let rec aux dir names =
      match names with
         [] -> dir
       | ""::ns -> aux dir ns
       | "~"::ns -> aux home ns
       | n::ns when (Lm_string_util.for_all (fun c -> c = '.') n) ->
            (* Remove |n| elements from dir's tail *)
            let head = try (fst (Lm_list_util.split_list ((List.length dir) - (String.length n) + 1) dir))
                       with Failure "split_list" -> []
            in aux head ns
       | n::ns -> aux (dir @ [n]) ns
   in
      aux (if String.length name <> 0 & name.[0] = '/' then [] else info.shell_dir)
      (Lm_string_util.split "/" name)

(*
 * Window width.
 *)
let set_window_width shell i =
   shell.shell_width <- max !Mp_term.min_screen_width i

(************************************************************************
 * PROCESS CONTROL                                                      *
 ************************************************************************)

(*
 * Show current process id.
 *)
let pid shell =
   Lm_thread_shell.get_pid ()

(*
 * Show all process names.
 *)
let jobs shell =
   let pids = Lm_thread_shell.get_pids () in
   let buf = Buffer.create 32 in
      ignore (List.fold_left (fun first i ->
                    if not first then
                       Buffer.add_string buf " ";
                    Buffer.add_string buf (string_of_int i);
                    false) true pids);
      Buffer.contents buf

(*
 * Switch jobs.
 *)
let fg shell pid =
   Lm_thread_shell.set_pid pid

(*
 * Interface to the HTTP shell.
 *)
let get_ls_options shell =
   Session.get_view_options ()

let get_view_options shell =
   string_of_ls_options (get_ls_options ())

let set_view_options shell s =
   Session.add_view_options s

let clear_view_options shell s =
   Session.clear_view_options s

let get_shortener shell =
   match shell.shell_package with
      Some pkg ->
         let mk_opname = Package_info.mk_opname pkg in
         let shortener opname params bterms =
            match Opname.dest_opname opname with
               h :: _ ->
                  let params = List.map param_type params in
                  let arities = List.map (fun bterm -> List.length (dest_bterm bterm).bvars) bterms in
                  let opname' = mk_opname [h] params arities in
                     if Opname.eq opname' opname then
                        h
                     else
                        Opname.string_of_opname opname
             | [] ->
                  "$"
         in
            shortener
    | None ->
         let shortener opname _ _ =
            Opname.string_of_opname opname
         in
            shortener

let pwd shell =
   string_of_path shell.shell_dir

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
