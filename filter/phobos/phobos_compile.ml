(*
 * Process Phobos files.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2001 Adam Granicz, Caltech
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
open Phobos_constants
open Phobos_exn
open Phobos_rewrite
open Phobos_header
open Phobos_marshal

(*
 * Parse Phobos file.
 *)
let parse_phobos_file s =
   if s = "" then
      raise (Invalid_argument "Must specify a language description file");
   (* Open source file and parse it *)
   let inx = open_in s in
   let pos = s, 0, 0, 0, 0 in
   Phobos_parse_state.set_current_position pos;
   let lex = Lexing.from_channel inx in
      Phobos_parser.main Phobos_lexer.main lex

(*
 * Compile a source program with a source grammar.
 *)
let compile_with_source_grammar paths gfile str =
   let cgfile = Phobos_util.filename_of_compiled_grammar gfile in
   let parser_struct = parse_phobos_file gfile in
   let gst, module_name, lenv, parsable = Phobos_grammar.compile paths parser_struct in
   let clenv = Phobos_tokenizer.create_clenv lenv in
   if parsable then begin
      let penv = Phobos_main.create_penv module_name gst in
      let ptable = Phobos_main.create_parsing_table gfile gst penv in
      if !Phobos_state.save_grammar then
         begin
            let digest = create_digest (string_of_file gfile) in
            let stats = Unix.stat gfile in
            let header = create_header digest stats.Unix.st_mtime stats.Unix.st_size in
               Phobos_marshal.save_grammar header gst lenv penv ptable cgfile
         end;
      let tokens = Phobos_tokenizer.tokenize_string str clenv in
         Phobos_main.parse_source gst clenv penv ptable tokens,
         gst.grammar_post_rewrites,
         gst.grammar_inline_forms,
         gst.grammar_local_rewrites
   end else
      raise (PhobosError "no start production is given")

let compile_with_saved_grammar paths gfile str =
   let fname = Phobos_util.find_file paths gfile in
   let _, (gst, lenv, penv, ptable) = Phobos_marshal.load_grammar fname in
   if gst.grammar_start_symbol = Phobos_constants.bogus_symbol then
      raise (PhobosError "no start production is defined in compiled grammar");
   let clenv = Phobos_tokenizer.create_clenv lenv in
   let tokens = Phobos_tokenizer.tokenize_string str clenv in
      Phobos_main.parse_source gst clenv penv ptable tokens,
      gst.grammar_post_rewrites,
      gst.grammar_inline_forms,
      gst.grammar_local_rewrites

let term_of_string paths gfile s =
   let cgfile = Phobos_util.filename_of_compiled_grammar gfile in
   let term, post_rws, inline_forms, local_rws =
      match Sys.file_exists gfile, Sys.file_exists cgfile with
      (* If both the source and binary grammar file exists, use the
         more recent one. *)
         true, true ->
            (try
               let header, _ = load_grammar cgfile in
               let time1 = timestamp_of gfile in
               let size1 = size_of gfile in
               let time2 = timestamp_of_header header in
               let size2 = sizestamp_of_header header in
                  (* Both are of the same size and date: use binary grammar *)
                  if time1 = time2 && size1 = size2 then
                     compile_with_saved_grammar paths cgfile s
                  (* The source file is more recent, use it only if it has really changed *)
                  else if time1 > time2 then
                     begin
                        let digest1 = create_digest (string_of_file gfile) in
                        let digest2 = digest_of_header header in
                           if digest1 = digest2 then
                              compile_with_saved_grammar paths cgfile s
                           else
                              compile_with_source_grammar [] gfile s
                     end
                  (* Otherwise, use the binary grammar *)
                  else
                     compile_with_saved_grammar paths cgfile s
            with
               File_type_base.Bad_version _
             | End_of_file ->
                  compile_with_source_grammar [] gfile s)
      (* Only the .pho file exists, so use it *)
       | true, false ->
            compile_with_source_grammar [] gfile s
      (* Only the .cph file exists, so we use that *)
       | false, true ->
            compile_with_saved_grammar paths cgfile s
      (* Neither file exists, raise error *)
       | false, false ->
            raise (PhobosError ("Can not find " ^ gfile ^ " or its binary form"))
   in
   let post_rws =
      List.map (fun iforms ->
         local_rws @ iforms) post_rws
   in
      apply_post_rewrites term post_rws
   
