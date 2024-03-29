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
 * The dictionary is compiled from $MP_ROOT/filter/words
 * contain sorted words that are correctly spelled.
 * The dictionary is saved in $MP_ROOT/lib/english_dictionary.dat
 *)
let dat_magic = 0x2557f3ef

(*
 * Check if the database is out-of-date.
 *)
let check_dict dat words =
   let tmp_stat = Unix.stat dat in
   let file_stat = Unix.stat words in
      file_stat.Unix.st_mtime > tmp_stat.Unix.st_mtime

(*
 * Borrowing from the Filename library, using the "lib" directory instead of the tmp one
 *)
let make_dict lib dat_filename words =
   let prng = Random.State.make_self_init () in
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
   let table = Lm_spell.make_dict words in
   let tmp, out = try_name 0 in
   let out = Stdlib.open_out_bin tmp in
      Stdlib.output_binary_int out dat_magic;
      Lm_spell.to_channel out table;
      Stdlib.close_out out;
      begin
         try
            Unix.rename tmp dat_filename with exn ->
               if Sys.file_exists dat_filename then
                  Unix.unlink tmp
               else
                  raise exn
      end;
      table

(*
 * Load the dict.
 *)
let load_dict lib dat words =
   if check_dict dat words then raise Not_found;
   let inx = open_in_bin dat in
      try
         let magic = input_binary_int inx in
            if magic <> dat_magic then
               raise Not_found;
            let table = Lm_spell.from_channel inx in
               close_in inx;
               table
      with
         exn ->
            close_in inx;
            raise exn

(*
 * The loaded dictionary.
 *)
let dict = lazy (let lib = Setup.lib() in
                 let dat = Filename.concat lib "english_dictionary.dat"
                 and words = Filename.concat lib "words.dict" in
                    try load_dict lib dat words
                    with
                       _ -> make_dict lib dat words)

(*
 * Initialize.
 *)
let init () = ignore (Lazy.force dict)

(*
 * Check a word.
 * Words are "correctly" spelled if the module is
 * not initialized.
 *)
let is_num = function
   '0'..'9' -> true
 | _ -> false

let check s =
   let dict = Lazy.force_val dict in
      if String.length s >= 2 then
         match s.[0] with
            'A'..'Z' ->
               Lm_spell.check dict s || (Lm_spell.check dict (String.lowercase_ascii s))
          | '0'..'9' when Lm_string_util.for_all is_num s ->
               true
          | _ ->
               Lm_spell.check dict s
      else
         true

let do_you_mean s = Lm_spell.anagram (Lazy.force_val dict) s false

(*
 * Add a word to the table.
 *)
let add w = Lm_spell.add_word (Lazy.force_val dict) w

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
