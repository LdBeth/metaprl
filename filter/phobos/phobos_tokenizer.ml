(*
 * Generic tokenizer.
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
open Phobos_util
open Phobos_debug
open Xstr_search

(*
 * Tokenizer.
 *
 * For now, this is done using Ocaml's regexps.
 * Later, I will probably write a simulator for
 * an NFA.
 *
 *)
(*
let compile_regexps regexps =
   List.map (fun (ignore, id, regexp) ->
      try
         debug_string (string_format "Creating regexp for %s [%s]\n" id regexp);
         (ignore, id, Str.regexp regexp)
      with
           Failure s ->
            raise (LexerException
               (string_add ["failed to create regular expression for ["; id; "] - "; s]))
         | exn ->
            raise (LexerException
               (string_add ["failed to create regular expression for ["; id; "]"]))) regexps
*)

let loptions_of loptions =
   let long = list_mem loptions Lo_longest in
   let frst = list_mem loptions Lo_first in
      match long, frst with
         a, b when a = b && a ->
            raise (LexerException "inconsistent lexer options (both -longest and -first)")
       | _ ->
            long, frst

let rec num_of_newlines s count =
   try
      let index = String.index s '\n' in
         num_of_newlines (String.sub s (index+1) (String.length s-index-1)) (count+1)
   with
      Not_found ->
         count

let num_of_newlines s = num_of_newlines s 0

let inside_last_line s =
   try
      let index = String.rindex s '\n' in
         (String.length s-index-1)
   with
      Not_found ->
         String.length s

let calculate_pos pos mstr =
   let newlines = num_of_newlines mstr in
   let right = inside_last_line mstr in
   let (name, ll, ln, rl, rn) = pos in
      if newlines = 0 then
         (name, rl, rn, rl+newlines, rn+right)
      else
         (name, rl, rn, rl+newlines, right)

(*
 * Find longest/first matching regexp, recurse until end of string
 * or if no regexp matches.
 *)
let tokens = ref []
let buffer = ref ""
let buf_pos = ref 0
let buf_len = ref 0

exception Got_it

let rec shift_token pos regexps long frst mterm mstr mlen ignored =
   if mlen = 0 then
      raise (LexerPosException (pos, "Syntax error"))
   else
      if not ignored then
         tokens := (mterm, mstr, pos) :: !tokens;
      ()

(*
 * Split an input file into tokens defined by
 * a list of regexps.
 * Return a list of tokens, along with their
 * matched strings, and the regexp that matched each.
 *)
and do_tokenize name buf regexps loptions =
   (* input file *)
   let temp_pos = ref (name, 1, 0, 1, 0) in
   let long, frst = loptions_of loptions in
   buf_pos := 0;
   buf_len := String.length buf;
   buffer := buf;
   while buf_pos < buf_len
   do
      let mterm = ref "" in
      let mstr = ref "" in
      let mlen = ref 0 in
      let rege = ref "" in
      let ignored = ref false in
      try
         List.iter (fun (ignore, id, r) ->
            rege := id;
            if Str.string_match r !buffer !buf_pos then
            begin
               let ms = Str.matched_string !buffer in
               let lms = String.length ms in
               if lms > !mlen then
               begin
                  mterm := id;
                  mstr := ms;
                  mlen := lms;
                  ignored := ignore;
                  temp_pos := calculate_pos !temp_pos !mstr;
                  if frst then
                     raise Got_it
               end
            end) regexps;
         shift_token !temp_pos regexps long frst !mterm !mstr !mlen !ignored;
         buf_pos := !buf_pos + !mlen;
      with
           Got_it ->
            shift_token !temp_pos regexps long frst !mterm !mstr !mlen !ignored;
            buf_pos := !buf_pos + !mlen
   done;
   List.rev !tokens

let create_lenv gst loptions =
   let split_regexps regexps =
      let tmp =
         List.map (fun (ignore, ((id, _), _), regexp, rewrites) ->
               (ignore, id, [regexp]), (id, rewrites)) regexps
      in
      let regexps = List.map fst tmp in
      let rewrites = List.map snd tmp in
      let lex_rewrite_table =
         List.fold_left (fun lex_rewrite_table (id, rewrites) ->
            lex_rewrite_add_or_replace_list lex_rewrite_table (Terminal id) rewrites) lex_rewrite_empty rewrites
      in
         regexps, lex_rewrite_table
   in
   let regexps, lex_rewrite_table = split_regexps gst.grammar_token_rules in
      { lexer_regexps = regexps;
        lexer_options = loptions;
        lexer_rewrites = lex_rewrite_table
      }

let create_clenv
   { lexer_regexps = regexps;
     lexer_options = loptions;
     lexer_rewrites = rewrites
   } =
   let regexps =
      (* Replace special characters in regular expressions *)
      List.map (fun (ignore, id, regexps) ->
         let regexps =
            List.map (fun regexp ->
               let regexp = replace_substring regexp ["\\n"] (fun _ _ -> "\n") in
               let regexp = replace_substring regexp ["\\t"] (fun _ _ -> "\t") in
               let regexp = replace_substring regexp ["\\'"] (fun _ _ -> "'") in
               let regexp = replace_substring regexp ["\\\""] (fun _ _ -> "\"") in
               let regexp = replace_substring regexp ["\\\\"] (fun _ _ -> "\\") in
                  regexp) regexps
         in
            ignore, id, regexps) regexps
   in
   let disjunction_of regexps =
      let empty_string = "" in
         List.fold_left (fun disj regexp ->
            (* Make sure we are adding a legal regular expression *)
            try
               let _ = Str.regexp regexp in
                  string_add ["\\("; regexp; "\\)\\|\\("; disj; "\\)"]
            with
               Failure s ->
                  raise (LexerException (**)
                     (string_add ["failed to create regular expression for ["; regexp; "] - "; s]))) empty_string regexps
   in

   let cregexps =
      List.map (fun (ignore, id, regexps) ->
         try
            let regexp = disjunction_of regexps in
            debug_string (Lm_printf.sprintf "Creating regexp for %s [%s]\n" id regexp);
               ignore, id, Str.regexp regexp
         with
              Failure s ->
               raise (LexerException (**)
                  (string_add ["failed to create regular expression for ["; id; "] - "; s]))
(*            | exn ->
               raise (LexerException (**)
                  (string_add ["failed to create regular expression for ["; id; "]"]))*)) regexps
   in
      { clexer_regexps = cregexps;
        clexer_options = loptions;
        clexer_rewrites = rewrites
      }

let tokenize name
   { clexer_regexps = regexps;
     clexer_options = loptions;
     clexer_rewrites = _
   } =
   tokens := [];
   let buf = string_of_file name in
   let tokens = do_tokenize name buf regexps loptions in
   debug_tokens "The input token list:\n" tokens;
   List.map (fun (s1, s2, pos) ->
      Terminal s1, s2, pos) tokens

let tokenize_string str
   { clexer_regexps = regexps;
     clexer_options = loptions;
     clexer_rewrites = _
   } =
   tokens := [];
   let tokens = do_tokenize "<<string>>" str regexps loptions in
   debug_tokens "The input token list:\n" tokens;
   List.map (fun (s1, s2, pos) ->
      Terminal s1, s2, pos) tokens
