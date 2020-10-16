(*
 * Define a spell-checker.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2006 MetaPRL Group, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

(*
 * The dictionary is compiled from /usr/dict/words and
 * $(MPLIB)/words.  Both file just contain words
 * that are correctly spelled.  The dictionary is saved
 * as a hashtable in /tmp/metaprl-spell.dat.
 *)
let dat_magic = 0x2557f3ef

(*
 * Defer requesting MP_ROOT to init()
 *)
let lib = ref ""
let dat_filename = ref ""
let words_filenames = ref []

(*
 * The loaded dictionary.
 *)
let dict = ref None

(*
 * Check if the database is out-of-date.
 *)
let check_magic () =
   try
      let inx = open_in_bin !dat_filename in
         try
            let i = input_binary_int inx in
               close_in inx;
               i = dat_magic
         with
            _ ->
               close_in inx;
               false
   with
      _ ->
         false


let check_dict () =
   if check_magic () then
      let tmp_stat = Unix.stat !dat_filename in
      let check_magic' file =
         (try
             let file_stat = Unix.stat file in
                file_stat.Unix.st_mtime > tmp_stat.Unix.st_mtime
          with
             Unix.Unix_error _ ->
                false)
      in
         List.exists check_magic' !words_filenames
   else
      true

(*
 * Make the dictionary.
 *)
let add_file table filename =
   try
      let inx = open_in filename in
         try
            while true do
               Hashtbl.add table (input_line inx) ()
            done
         with
            _ ->
               close_in inx
   with
      _ ->
         ()

(*
 * Borrowing from the Filename library, using the "lib" directory instead of the tmp one
 *)
let make_dict () =
   let prng = Random.State.make_self_init () in
   let lib = !lib in
   let dat_filename = !dat_filename in
   let rec try_name counter =
      if counter >= 1000 then
         invalid_arg "Filter_spell.open_temp_file: lib directory nonexistent or full";
      let rnd = (Random.State.bits prng) land 0xFFFFFF in
      let name = Filename.concat lib (Printf.sprintf "english_dictionary%06x.dat" rnd) in
      try
         (name,
          open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 name)
      with Sys_error _ ->
         try_name (counter + 1)
   in
   let table = Hashtbl.create 1037 in
      List.iter (add_file table) !words_filenames;
      dict := Some table;
      let tmp, out = try_name 0 in
      let out = Pervasives.open_out_bin tmp in
         Pervasives.output_binary_int out dat_magic;
         Marshal.to_channel out table [];
         Pervasives.close_out out;
         try
            Unix.rename tmp dat_filename
         with exn ->
            if Sys.file_exists dat_filename then
               Unix.unlink tmp
            else
               raise exn

(*
 * Load the dict.
 *)
let load_dict () =
   try
      let inx = open_in_bin !dat_filename in
         try
            let magic = input_binary_int inx in
               if magic <> dat_magic then
                  raise Not_found;
               let table = Marshal.from_channel inx in
                  close_in inx;
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
         lib := Setup.lib();
         dat_filename := Filename.concat !lib "english_dictionary.dat";
         words_filenames := [Filename.concat !lib "words.metaprl";
                             Filename.concat !lib "words.linux";
                            ];
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
let is_num = function
   '0'..'9' -> true
 | _ -> false

let check s =
   match !dict with
      Some dict ->
         if String.length s >= 2 then
            match s.[0] with
               'A'..'Z' ->
                  Hashtbl.mem dict s || (Hashtbl.mem dict (String.lowercase s))
             | '0'..'9' when Lm_string_util.for_all is_num s ->
                  true
             | _ ->
                  Hashtbl.mem dict s
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
         Hashtbl.add table word ()
    | None ->
         ()

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
