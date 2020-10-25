(*
 * Create an ediable rewrite object.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
extends Mpsymbols
extends Base_dform
extends Summary

open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

open Shell_sig
open Shell_util

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Info for a directory.
 *)
type dir_info =
   { dir_entries : (string * string) list;
     dir_ignore  : Str.regexp option
   }

(*
 * The element can be a directory or a file.
 *)
type info_data =
   File of string list
 | Directory of dir_info

(*
 * Info for a specific file.
 *)
type info =
   { mutable info_root    : string;
     mutable info_subdir  : string;
     mutable info_data    : info_data
   }

(************************************************************************
 * Terms and display forms
 *)

(* Directories and files *)
declare "direntry"[name:s, modifier:s] : Dform
declare "dirlisting"[name:s]{'fl} : Dform
declare "fileline"[number:n, contents:s] : Dform
declare "filelisting"[name:s]{'l} : Dform

(*
 * Packages.
 *)
declare listing_df{'t : Dform} : Dform

dform dirlisting_df : dirlisting[name:s]{'listing} =
   hzone listing_df{'listing} ezone

dform filelisting_df : filelisting[name:s]{'listing} =
   hzone monospaced_begin listing_df{'listing} monospaced_end ezone

dform listing_df1 : listing_df{xcons{'e1; xcons{'e2; 'next}}} =
   'e1 hspace listing_df{xcons{'e2; 'next}}

dform listing_df2 : listing_df{xcons{'e; xnil}} =
   'e

dform listing_df3 : listing_df{xnil} =
   `""

dform direntry_df : direntry[name:s, modifier:s] =
   cd_begin[name] slot[name:s] cd_end slot[modifier:s]

dform fileline_df : fileline[number:n, line:s] =
   slot[line:s]

(*
 * Constructors
 *)
let mk_direntry_term s modifier = <:con< "direntry"[$s$:s, $modifier$:s] >>
let mk_dirlisting_term name files = <:con< "dirlisting"[$name$:s]{$mk_xlist_term files$} >>
let mk_fileline_term n s = <:con< "fileline"[$n$:n, $s$:s] >>
let mk_filelisting_term name files = <:con< "filelisting"[$name$:s]{$mk_xlist_term files$} >>

(************************************************************************
 * Read a file into memory.
 *)
let lines_of_file filename =
   let inx = open_in filename in
   let rec collect lines =
      let line =
         try Some (input_line inx) with
            End_of_file ->
               None
      in
         match line with
            Some line ->
               collect (line :: lines)
          | None ->
               List.rev lines
   in
   let lines = collect [] in
      close_in inx;
      lines

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
   let names = List.sort (fun s1 s2 -> Stdlib.compare s1 s2) names in
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
               name, modifier) names

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
    ".svn";
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

let dirname_of_subdir info subdir =
   Filename.concat info.info_root subdir

let load_dir_entries_exn info subdir =
   let filename = dirname_of_subdir info subdir in
   let data =
      match (Unix.stat filename).Unix.st_kind with
         Unix.S_DIR ->
            let dir_info =
               { dir_entries = entries_of_dir filename;
                 dir_ignore = load_cvsignore filename
               }
            in
               Directory dir_info
       | Unix.S_REG ->
            Session.add_edit subdir;
            File (lines_of_file filename)
       | _ ->
            raise Not_found
   in
      info.info_subdir <- subdir;
      info.info_data <- data

let load_dir_entries info subdir =
   try load_dir_entries_exn info subdir with
      Unix.Unix_error _
    | Sys_error _
    | Failure _ ->
         raise (RefineError ("Shell_fs.load_dir_entries", StringStringError (subdir, "unreadable")))

let refresh_dir_entries info =
   load_dir_entries info info.info_subdir

(************************************************************************
 * SHELL INTERFACE                                                      *
 ************************************************************************)

(*
 * Display the entries in a directory.
 *)
let term_of_dir info options subdir dir_info =
   let { dir_entries = entries;
         dir_ignore = ignore
       } = dir_info
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
      List.fold_left (fun terms (name, modifier) ->
            if nametest name then
               mk_direntry_term name (modname modifier) :: terms
            else
               terms) [] entries
   in
      mk_dirlisting_term subdir (List.rev terms)

(*
 * File lines.
 *)
let term_of_file info options subdir lines =
   let lines, _ =
      List.fold_left (fun (lines, i) line ->
            let line = mk_fileline_term (Lm_num.num_of_int i) line in
               line :: lines, succ i) ([], 1) lines
   in
      mk_filelisting_term subdir (List.rev lines)

(*
 * Display the listing.
 *)
let edit_display get_dfm info options =
   let { info_root    = root;
         info_subdir  = subdir;
         info_data    = data
       } = info
   in
   let term =
      match data with
         Directory dir_info ->
            term_of_dir info options subdir dir_info
       | File file_info ->
            term_of_file info options subdir file_info
   in
      Proof_edit.display_term (get_dfm ()) term

(*
 * Error handler.
 *)
let raise_edit_error s =
   raise (RefineError ("Shell_fs", StringError s))

(*
 * Build the shell interface.
 *)
let rec edit get_dfm info =
   let edit_check_addr addr =
      let path = Lm_filename_util.simplify_path addr in
      let path = Lm_filename_util.concat_path path in
         if info.info_subdir <> path then
            load_dir_entries info path
   in
   let edit_display addr options =
      edit_check_addr addr;
      edit_display get_dfm info options
   in
   let edit_copy () =
      edit get_dfm { info with info_root = info.info_root }
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

   (*
    * This function always returns false.
    * However, it is wise to keep it because
    * we may add more methods.
    *)
   let edit_is_enabled _ = function
      MethodRefine
    | MethodPaste _
    | MethodUndo
    | MethodRedo
    | MethodApplyAll
    | MethodExpand ->
         false
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
        edit_is_enabled = edit_is_enabled
      }

let create get_dfm =
   let info =
      { info_root    = Setup.root ();
        info_subdir  = ".";
        info_data    = Directory { dir_entries = []; dir_ignore = None }
      }
   in
      refresh_dir_entries info;
      edit get_dfm info

let view = create

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
