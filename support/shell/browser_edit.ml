(*
 * File editing commands.
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
open Printf


(*
 * Complete edit info.
 *)
type edit_info =
   { edit_point    : int;
     edit_modified : bool;
     edit_new      : bool;
     edit_rootname : string
   }

(*
 * Edit info.
 *)
let backup_suffix = ".bak"

let absname name =
   Filename.concat (Setup.root ()) name

let editname = absname

let backupname name =
   name ^ backup_suffix

let make_edit_info point name =
   let backupname = backupname name in
   let isnew = not (Sys.file_exists (absname name)) in
      if Sys.file_exists (absname backupname) then
         { edit_point = point;
           edit_modified = true;
           edit_new = isnew;
           edit_rootname = backupname
         }
      else
         { edit_point = point;
           edit_modified = false;
           edit_new = isnew;
           edit_rootname = name
         }

let get_edit_info name =
   make_edit_info (Session.get_edit_point name) name

(*
 * File names.
 *)
let proxyedit_of_filename name =
   name ^ "/" ^ "info.prl"

let filename_of_proxyedit name =
   if Filename.basename name = "info.prl" then
      Filename.dirname name
   else
      name

(*
 * Open a file.
 *)
let out_channel_of_file name =
   try Some (open_out name) with
      Sys_error _ ->
         None

(*
 * Skip a number: this should be exactly 9 characters.
 *)
let skip_number s off =
   let len = String.length s in
   let rec check_decimal i =
      if i = 9 then
         true
      else
         match s.[off + i] with
            ' '
          | '0'..'9' ->
               check_decimal (succ i)
          | _ ->
               false
   in
      if len - off >= 9 && check_decimal 0 then
          off + 9
      else
          off

(*
 * Add a specific number of space characters.
 *)
let add_spaces buf spaces =
   for i = 1 to spaces do
      Buffer.add_char buf ' '
   done

(*
 * Strip DOS-style line-endings.
 *)
let unix_of_dos skip_lines s =
   let len = String.length s in
   let buf = Buffer.create len in
   let skip_number =
      if skip_lines then
         skip_number
      else
         (fun _ i -> i)
   in
   let rec copy spaces i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
            if c = '\r' && i + 1 < len && s.[i + 1] = '\n' then
               begin
                  Buffer.add_char buf '\n';
                  copy 0 (skip_number s (i + 2))
               end
            else if c = '\n' then
               begin
                  Buffer.add_char buf '\n';
                  copy 0 (skip_number s (i + 1))
               end
            else if c = ' ' then
               copy (succ spaces) (succ i)
            else
               begin
                  add_spaces buf spaces;
                  Buffer.add_char buf c;
                  copy 0 (i + 1)
               end
   in
      copy 0 (skip_number s 0)

(*
 * Replace the file with the string.
 *)
let save_file name skip_lines point contents =
   let filename = absname name in
   let backupname = backupname filename in
   let contents = unix_of_dos skip_lines contents in
   let result =
      match out_channel_of_file filename with
         Some out ->
            eprintf "save_file: saving file %s@." filename;
            output_string out contents;
            close_out out;
            true
       | None ->
            false
   in
      (try Unix.unlink backupname with
          Unix.Unix_error _ ->
             ());
      Session.add_edit_point name point;
      result

(*
 * Just back it up.
 *)
let backup_file name skip_lines point contents =
   let backupname = absname (backupname name) in
   let contents = unix_of_dos skip_lines contents in
   let result =
      match out_channel_of_file backupname with
         Some out ->
            eprintf "backup_file: saving file %s@." backupname;
            output_string out contents;
            close_out out;
            true
       | None ->
            false
   in
      Session.add_edit_point name point;
      result

(*
 * Cancel the edit.
 *)
let cancel_file name _ point contents =
   let backupname = absname (backupname name) in
      (try Unix.unlink backupname with
          Unix.Unix_error _ ->
             ());
      true

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
