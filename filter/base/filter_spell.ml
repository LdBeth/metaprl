(*
 * Define a specll-checker.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *)

open Mp_debug
open Printf

(*
 * The dictionary is compiled from /usr/dict/words and
 * $HOME/.ispell_english.  Both file just contain words
 * that are correctly spelled.  The dictionary is saved
 * as a hashtable in /tmp/metaprl-spell.dat.
 *)
let words_filename = "/usr/dict/words"

let tmp_magic = 0x2557f3ed

let tmp_filename =
   let user =
      try Sys.getenv "USER" with
         Not_found ->
            "generic"
   in
      "/tmp/metaprl-spell-" ^ user ^ ".dat"

let ispell_filename =
   ".ispell_english"

let home_filename =
   let home =
      try Sys.getenv "HOME" with
         Not_found ->
            "/etc"
   in
      home ^ "/" ^ ispell_filename

(*
 * The loaded dictionary.
 *)
let dict = ref None

(*
 * Check if the database is out-of-date.
 *)
let check_magic () =
   try
      let inx = open_in_bin tmp_filename in
         try
            let i = input_binary_int inx in
               close_in inx;
               i = tmp_magic
         with
            _ ->
               close_in inx;
               false
   with
      _ ->
         false

let check_dict () =
   if check_magic () then
      let tmp_stat = Unix.stat tmp_filename in
         (try
             let words_stat = Unix.stat words_filename in
                words_stat.Unix.st_mtime > tmp_stat.Unix.st_mtime
          with
             Unix.Unix_error _ ->
                false) ||
         (try
             let home_stat = Unix.stat home_filename in
                home_stat.Unix.st_mtime > tmp_stat.Unix.st_mtime
          with
             Unix.Unix_error _ ->
                false)
   else
      false

(*
 * Make the dictionary.
 *)
let add_file table filename =
   try
      let inx = open_in filename in
         try
            while true do
               let word = input_line inx in
               let key = String.lowercase word in
               let key =
                  if key = word then
                     word
                  else
                     key
               in
                  Hashtbl.add table key word
            done
         with
            _ ->
               close_in inx
   with
      _ ->
         ()

let make_dict () =
   eprintf "Building spelling dictionary %s...%t" tmp_filename flush;
   let table = Hashtbl.create 1037 in
      add_file table words_filename;
      add_file table home_filename;
      dict := Some table;

      let out = open_out_bin tmp_filename in
         output_binary_int out tmp_magic;
         Marshal.to_channel out table [];
         close_out out;
         add_file table ispell_filename;
         eprintf "[done]%t" eflush

(*
 * Load the dict.
 *)
let load_dict () =
   try
      let inx = open_in_bin tmp_filename in
         try
            let magic = input_binary_int inx in
               if magic <> tmp_magic then
                  raise Not_found;
               let table = Marshal.from_channel inx in
                  close_in inx;
                  add_file table ispell_filename;
                  dict := Some table
         with
            _ ->
               close_in inx;
               make_dict ()
   with
      _ ->
         make_dict ()

(*
 * Initialize.
 *)
let init () =
   match !dict with
      None ->
         if check_dict () then
            make_dict ()
         else
            load_dict ()
    | Some _ ->
         ()

(*
 * Check a word.
 * Words are "correctly" spelled if the module is
 * not initialized.
 *)
let check s =
   match !dict with
      Some dict ->
         if String.length s >= 2 then
            match s.[0] with
               'A'..'Z'
             | 'a'..'z' ->
                  let key = String.lowercase s in
                     (try
                         let words = Hashtbl.find_all dict key in
                            List.mem s words || List.mem key words
                      with
                         Not_found ->
                            false)
             | _ ->
                  true
         else
            true
    | None ->
         true

(*
 * Add a word to the table.
 *)
let add word =
   match !dict with
      Some table ->
         let key = String.lowercase word in
         let key =
            if key = word then
               word
            else
               key
         in
            Hashtbl.add table key word
    | None ->
         ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
