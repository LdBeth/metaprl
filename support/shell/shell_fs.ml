(*
 * Create an ediable rewrite object.
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
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
extends Shell_sig
extends Package_info
extends Summary

open Lm_printf

open Refiner.Refiner.RefineError
open Dform

open Summary
open Shell_sig
open Shell_util

let eflush out =
   output_char out '\n';
   flush out

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * This is the actual editable object.
 *)
type entry =
   EntryFile of string * string
 | EntryUnreadable

type dir =
   { mutable dir_root    : string;
     mutable dir_subdir  : string;
     mutable dir_entries : entry list;
     mutable dir_ignore  : Str.regexp option
   }

(************************************************************************
 * Listing.
 *)

(*
 * Get all the names in the directory.
 *)
let strings_of_dir dirname =
   let inx = Unix.opendir dirname in
   let rec read files =
      try read (Unix.readdir inx :: files) with
         End_of_file ->
            files
   in
   let files = read [] in
      Unix.closedir inx;
      files

let entries_of_dir dirname =
   let names = strings_of_dir dirname in
   let names = List.sort (fun s1 s2 -> Pervasives.compare s1 s2) names in
      List.map (fun name ->
            let modifier =
               try
                  let name = Filename.concat dirname name in
                  let stat = Unix.lstat name in
                     match stat.Unix.st_kind with
                        Unix.S_REG ->
                           if (stat.Unix.st_perm land 0o111) <> 0 then
                              "*"
                           else
                              ""
                      | Unix.S_DIR ->
                           "/"
                      | Unix.S_CHR ->
                           "%"
                      | Unix.S_BLK ->
                           "#"
                      | Unix.S_LNK ->
                           "@"
                      | Unix.S_FIFO ->
                           "|"
                      | Unix.S_SOCK ->
                           "="
               with
                  Unix.Unix_error _ ->
                     "!"
            in
               EntryFile (name, modifier)) names

(*
 * Convert a shell regular expression to normal regular expression.
 *)
let add_shell_pattern buf s =
   let len = String.length s in
      Buffer.add_string buf "\\|^";
      for i = 0 to pred len do
         let c = s.[i] in
            match s.[i] with
               '*' ->
                  Buffer.add_string buf ".*"
             | '?' ->
                  Buffer.add_string buf "."
             | '.'
             | '+'
             | '^'
             | '$' ->
                  Buffer.add_string buf "\\";
                  Buffer.add_char buf c
             | _ ->
                  Buffer.add_char buf c
      done;
      Buffer.add_char buf '$'

(*
 * These are the files that CVS ignores by default.
 * https://www.cvshome.org/docs/manual/cvs-1.11.16/cvs_18.html#IDX266
 *)
let default_patterns =
   ["RCS";
    "SCCS";
    "CVS";
    "CVS.adm";
    "RCSLOG";
    "cvslog.*";
    "tags";
    "TAGS";
    ".make.state";
    ".nse_depinfo";
    "*~";
    "#*";
    ".#*";
    ",*";
    "_$*";
    "*$";
    "*.old";
    "*.bak";
    "*.BAK";
    "*.orig";
    "*.rej";
    ".del-*";
    "*.a";
    "*.olb";
    "*.o";
    "*.obj";
    "*.so";
    "*.exe";
    "*.Z";
    "*.elc";
    "*.ln";
    "core";
    "core.*"]

(*
 * Load the ignore expression from .cvsignore.
 *)
let load_cvsignore dirname =
   let filename = Filename.concat dirname ".cvsignore" in

   (* Get the patterns from the file *)
   let inx = open_in filename in
   let rec collect patterns =
      try collect (Lm_string_util.tokens_std (input_line inx) @ patterns) with
         End_of_file ->
            patterns
   in
   let patterns = collect [] in
   let () = close_in inx in

   (* Concatenate them into a large regular expression *)
   let buf = Buffer.create 256 in
      Buffer.add_string buf "^\\.cvsignore$";
      List.iter (add_shell_pattern buf) default_patterns;
      List.iter (add_shell_pattern buf) patterns;
      Str.regexp (Buffer.contents buf)

let load_cvsignore dirname =
   try Some (load_cvsignore dirname) with
      Sys_error _ ->
         None

let dirname_of_subdir dir subdir =
   Filename.concat dir.dir_root subdir

let load_dir_entries dir subdir =
   let dirname = dirname_of_subdir dir subdir in
      dir.dir_entries <- entries_of_dir dirname;
      dir.dir_ignore <- load_cvsignore dirname;
      dir.dir_subdir <- subdir

let refresh_dir_entries dir =
   try load_dir_entries dir dir.dir_subdir with
      Unix.Unix_error _ ->
         dir.dir_entries <- [EntryUnreadable];
         dir.dir_ignore <- None

(************************************************************************
 * SHELL INTERFACE                                                      *
 ************************************************************************)

(*
 * Display the listing.
 *)
let edit_display get_dfm dir options =
   let nametest =
      match dir.dir_ignore with
         Some ignore ->
            if LsOptionSet.mem options LsFileAll then
               (fun _ -> true)
            else
               (fun name -> not (Str.string_match ignore name 0))
       | None ->
            (fun _ -> true)
   in
   let modname =
      if LsOptionSet.mem options LsFileModifiers then
         (fun modifier -> modifier)
      else
         (fun modifier -> "")
   in
   let terms =
      List.fold_left (fun terms entry ->
            match entry with
               EntryFile (name, modifier) ->
                  if nametest name then
                     mk_file_term name (modname modifier) :: terms
                  else
                     terms
             | EntryUnreadable ->
                  mk_unreadable_term :: terms) [] dir.dir_entries
   in
   let term = mk_listing_term dir.dir_subdir (List.rev terms) in
      Proof_edit.display_term (get_dfm ()) term

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_fs", StringError s))

(*
 * Build the shell interface.
 *)
let rec edit get_dfm dir =
   let edit_check_addr addr =
      let path = Lm_filename_util.simplify_path addr in
      let path = Lm_filename_util.concat_path path in
         if dir.dir_subdir <> path then
            load_dir_entries dir path
   in
   let edit_display addr options =
      edit_check_addr addr;
      edit_display get_dfm dir options
   in
   let edit_copy () =
      edit get_dfm { dir with dir_root = dir.dir_root }
   in
   let not_a_rule _ =
      raise_edit_error "this is not a rule or rewrite"
   in
   let edit_save () =
      raise_edit_error "list of files can't be saved"
   in
   let edit_check _ =
      raise_edit_error "files can't be checked"
   in
   let edit_undo addr =
      addr
   in
   let edit_redo addr =
      addr
   in
   let edit_info addr =
      raise_edit_error "no info for the files"
   in
   let edit_interpret command =
      raise_edit_error "this is not a proof"
   in
   let edit_get_contents addr =
      raise_edit_error "can only retrieve contents of an individual item, not of a file"
   in
      { edit_display = edit_display;
        edit_get_contents = edit_get_contents;
        edit_get_terms = not_a_rule;
        edit_copy = edit_copy;
        edit_set_goal = not_a_rule;
        edit_set_redex = not_a_rule;
        edit_set_contractum = not_a_rule;
        edit_set_assumptions = not_a_rule;
        edit_set_params = not_a_rule;
        edit_get_extract = not_a_rule;
        edit_save = edit_save;
        edit_check = edit_check;
        edit_check_addr = edit_check_addr;
        edit_info = edit_info;
        edit_undo = edit_undo;
        edit_redo = edit_redo;
        edit_interpret = edit_interpret;
        edit_find = not_a_rule;
      }

let create get_dfm =
   let dir =
      { dir_root    = Setup.root ();
        dir_subdir  = ".";
        dir_entries = [];
        dir_ignore  = None
      }
   in
      refresh_dir_entries dir;
      edit get_dfm dir

let view = create

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
