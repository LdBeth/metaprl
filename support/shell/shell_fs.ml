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
 * A window is either a text window or an HTML window.
 *)
type text_window =
   { df_base : dform_mode_base;
     df_mode : string;
     mutable df_width : int
   }

type window =
   TextWindow of text_window
 | TexWindow of text_window
 | BrowserWindow of text_window

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
 * WINDOWS                                                              *
 ************************************************************************)

(*
 * Create a window from the description.
 *)
let create_window = function
   DisplayText (base, mode) ->
      TextWindow { df_base = base; df_mode = mode; df_width = 80 }
 | DisplayTex base ->
      TexWindow { df_base = base; df_mode = "tex"; df_width = 80 }
 | DisplayBrowser base ->
      BrowserWindow { df_base = base; df_mode = "html"; df_width = 80 }

(*
 * Update the width based on the terminal.
 *)
let update_terminal_width window =
   match window with
      TextWindow info ->
         info.df_width <- Mp_term.term_width Pervasives.stdout info.df_width;
         window
    | TexWindow _
    | BrowserWindow _ ->
         window

(*
 * Copy the window.
 *)
let new_window = function
   TextWindow _
 | TexWindow _
 | BrowserWindow _ as window ->
      window

(*
 * Display a term in the window.
 *)
let display_term window term =
   match update_terminal_width window with
      TextWindow { df_base = base; df_mode = mode; df_width = width } ->
         let df = get_mode_base base mode in
         let buf = Lm_rformat.new_buffer () in
            Dform.format_term df buf term;
            Lm_rformat_text.print_text_channel width buf Pervasives.stdout;
            eflush stdout
    | TexWindow { df_base = base; df_mode = mode; df_width = width } ->
         let df = get_mode_base base mode in
         let buf = Lm_rformat.new_buffer () in
            Dform.format_term df buf term;
            Lm_rformat_tex.print_tex_channel width buf Pervasives.stdout;
            eflush stdout
    | BrowserWindow { df_base = base; df_mode = mode } ->
         let buf = Lm_rformat.new_buffer () in
         let df = get_mode_base base mode in
         let df = save_slot_terms df in
         let () = Dform.format_term df buf term in
         let terms = get_slot_terms df in
            Session.set_main buf terms

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
let edit_display window dir options =
   let { dir_root    = root;
         dir_subdir  = subdir;
         dir_entries = entries;
         dir_ignore  = ignore
       } = dir
   in
   let nametest =
      match ignore with
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
                  mk_unreadable_term :: terms) [] entries
   in
   let term = mk_listing_term subdir (List.rev terms) in
      display_term window term

(*
 * Set a new address.
 *)
let edit_addr dir addr =
   let path = Lm_filename_util.simplify_path addr in
   let path = Lm_filename_util.concat_path path in
      load_dir_entries dir path

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_fs", StringError s))

(*
 * Build the shell interface.
 *)
let rec edit window dir =
   let edit_display options =
      edit_display window dir options
   in
   let edit_addr addr =
      edit_addr dir addr
   in
   let edit_fs_cwd () =
      dir.dir_subdir
   in
   let edit_int_addr addr =
      edit_addr (List.map string_of_int addr)
   in
   let edit_copy () =
      edit (new_window window) { dir with dir_root = dir.dir_root }
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
   let edit_undo () =
      ()
   in
   let edit_redo () =
      ()
   in
   let edit_info () =
      raise_edit_error "no info for the files"
   in
   let edit_refine _ _ _ =
      raise_edit_error "can't refine files"
   in
   let edit_interpret command =
      raise_edit_error "this is not a proof"
   in
   let edit_get_contents () =
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
        edit_addr = edit_addr;
        edit_int_addr = edit_int_addr;
        edit_info = edit_info;
        edit_refine = edit_refine;
        edit_undo = edit_undo;
        edit_redo = edit_redo;
        edit_interpret = edit_interpret;
        edit_find = not_a_rule;
        edit_fs_cwd = edit_fs_cwd
      }

let create window =
   let dir =
      { dir_root    = Setup.root ();
        dir_subdir  = ".";
        dir_entries = [];
        dir_ignore  = None
      }
   in
      refresh_dir_entries dir;
      edit (create_window window) dir

let view = create

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)