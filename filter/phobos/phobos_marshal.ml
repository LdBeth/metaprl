(*
 * Save compiled grammar.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Phobos_type
open Phobos_header

let marshal_write channel data options =
   let s = Marshal.to_string data options in
   let digest = Digest.string s in
      output_binary_int channel (String.length s);
      Digest.output channel digest;
      Marshal.to_channel channel data options

let marshal_read channel =
   let size = input_binary_int channel in
   let digest = Digest.input channel in
   let buf = String.create size in
      really_input channel buf 0 size;
   let digest_check = Digest.string buf in
      if digest <> digest_check then
         raise (Invalid_argument "checksum does not match");
      Marshal.from_string buf 0

let save_grammar header gst lenv penv ptable fname =
   let outx = open_out_bin fname in
      marshal_write outx header [];
      marshal_write outx gst [];
      marshal_write outx lenv [];
      marshal_write outx penv [];
      marshal_write outx ptable [];
      close_out outx

let load_grammar fname =
   let inx = open_in_bin fname in
   let (header: header) = marshal_read inx in
   let (gst: grammar_state) = marshal_read inx in
   let (lenv: lexer_env) = marshal_read inx in
   let (penv: parser_env) = marshal_read inx in
   let (ptable: parsing_table) = marshal_read inx in
      header, (gst, lenv, penv, ptable)
