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
open Phobos_exn

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
   let parser_struct = parse_phobos_file gfile in
   let gst, module_name, lenv, parsable = Phobos_grammar.compile paths parser_struct in
   let clenv = Phobos_tokenizer.create_clenv lenv in
   if parsable then begin
      let penv = Phobos_main.create_penv module_name gst in
      let ptable = Phobos_main.create_parsing_table gfile gst penv in
      if !Phobos_state.save_grammar then
         Phobos_marshal.save_grammar gst lenv penv ptable (Phobos_util.filename_of_compiled_grammar gfile);
      let tokens = Phobos_tokenizer.tokenize_string str clenv in
         Phobos_main.parse_source gst clenv penv ptable tokens,
         gst.grammar_post_rewrites,
         gst.grammar_inline_forms,
         gst.grammar_local_rewrites
   end else
      raise (PhobosError "no start production is given")

let compile_with_saved_grammar paths gfile str =
   let fname = Phobos_util.find_file paths gfile in
   let gst, lenv, penv, ptable = Phobos_marshal.load_grammar fname in
   if gst.grammar_start_symbol = Phobos_constants.bogus_symbol then
      raise (PhobosError "no start production is defined in compiled grammar");
   let clenv = Phobos_tokenizer.create_clenv lenv in
   let tokens = Phobos_tokenizer.tokenize_string str clenv in
      Phobos_main.parse_source gst clenv penv ptable tokens,
      gst.grammar_post_rewrites,
      gst.grammar_inline_forms,
      gst.grammar_local_rewrites

(*
 * TEMP: no post-rewrites are applied.
 *)
let term_of_string paths gfile s =
   let cgfile = Phobos_util.filename_of_compiled_grammar gfile in
   let term, _, _, _ =
      if Sys.file_exists cgfile then
         compile_with_saved_grammar paths cgfile s
      else
         compile_with_source_grammar [] gfile s
   in
      term
   
