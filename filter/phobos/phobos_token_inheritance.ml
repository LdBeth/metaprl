(*
 * Implement token inheritance strategy.
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

open Smap
(*open Mc_string_util*)
open Phobos_type
open Phobos_exn
open Phobos_util

module OrderedString = struct
   type t = string
   let compare = compare
end

module StringSerialMap = SerialMapMake (OrderedString)

(*
 * Reconstruct the grammar and populate the lexer environment
 * according to the token rules.
 *)
let apply_token_inheritance
   { grammar_nonterminals     = nonterminals;
     grammar_terminals        = terminals;
     grammar_assocs           = assocs;
     grammar_token_rules      = token_rules;
     grammar_start_symbol     = start_symbol;
     grammar_grammar          = grammar;
     grammar_termsets         = termsets;
     grammar_local_rewrites   = local_rewrites;
     grammar_post_rewrites    = post_rewrites;
     grammar_inline_forms     = inline_forms
   }
   { lexer_regexps            = regexps;
     lexer_options            = loptions;
     lexer_rewrites           = lex_rewrites
   } =
   let check_ignore ignr ignore s_pos =
      if ignr <> ignore then
         print_warning s_pos "token class changed";
   in
   (* Perform a token option and return the new token table
    * and a table containing token substitutions.
    *)
   let perform_token_option table replace (ignore, ((tok, tok_pos), tok_options), regexp, rewrites) = function
      (* Extend terminal symbol's defintion *)
      Token_extend None ->
         (try
            let regexps, ignr = StringSerialMap.find table tok in
            check_ignore ignr ignore tok_pos;
            StringSerialMap.add table tok (StringSet.add regexps regexp, ignr), replace
         with
            Not_found ->
               raise (LexerPosException (tok_pos, "unbound token")))
      (* Inherit from other terminal symbols *)
    | Token_extend (Some ids) ->
         List.fold_left (fun table (s, s_pos) ->
            try
               let regexps, ignr = StringSerialMap.find table s in
               check_ignore ignr ignore s_pos;
               StringSerialMap.add table tok (StringSet.add regexps regexp, ignr)
            with
               Not_found ->
                  raise (LexerPosException (s_pos, "unbound token"))) table ids, replace
      (* Remove a regular expression from a terminal symbol's definition *)
    | Token_remove to_remove ->
         (try
            (* Retrieve regular expressions associated with token *)
            let regexps, ignr = StringSerialMap.find table tok in
            (* Remove specified regular expressions *)
            let regexps =
               List.fold_left (fun regexps (regexp, re_pos) ->
                  try
                     StringSet.remove regexps regexp
                  with
                     Not_found ->
                           raise (LexerPosException (re_pos, "regexp not found"))) regexps to_remove
            in
            (* Add final regular expression to remaining list (if non-empty) *)
            if regexp <> "" then
               StringSerialMap.add table tok ((StringSet.add regexps regexp), ignore), replace
            else
               StringSerialMap.add table tok (regexps, ignore), replace
         with
            Not_found ->
               raise (LexerPosException (tok_pos, "unbound token")))
      (* Replace terminal symbols with a new symbol *)
    | Token_override ids ->
         (* Remove ids from table *)
         let table =
            List.fold_left (fun table (s, s_pos) ->
               try
                  let regexps, ignr = StringSerialMap.find table s in
                  check_ignore ignr ignore s_pos;
                  StringSerialMap.remove table s
               with
                  Not_found ->
                     raise (LexerPosException (s_pos, "unbound token"))) table ids
         in
         (* Add new regexp *)
         let table = StringSerialMap.add table tok (StringSet.of_list [regexp], ignore) in
            table, List.fold_left (fun replace (s, s_pos) ->
               StringTable.add replace s tok) replace ids
   in
   (* Calculate the token table (mapping tokens to a list of regular expressions),
    * and the substitutions table (containing what has been replaced).
    *)
   let ttable, replace =
      List.fold_left (fun (ttable, replace) item ->
         let (ignore, ((tok, tok_pos), tok_options), regexp, rewrites) = item in
         let ttable, replace =
            match tok_options with
            (* No token options, we just add token to table with single regular expression *)
               [] ->
                  StringSerialMap.add ttable tok (StringSet.of_list [regexp], ignore), replace
            (* Apply all token options *)
             | tok_options ->
                  List.fold_left (fun (ttable, replace) op ->
                     perform_token_option ttable replace item op) (ttable, replace) tok_options
         in
            ttable, replace) (StringSerialMap.empty, StringTable.empty) token_rules
   in
   let change_string replace s =
      try
         StringTable.find replace s
      with
         Not_found ->
            s
   in
   let change_symbol replace = function
      Terminal s ->
         let s = 
            (try
               StringTable.find replace s
            with
               Not_found ->
                  s)
         in
            Terminal s
    | sym ->
         sym
   in 
   (* Replace terminal symbols in grammar. *)
   let grammar =
      StringTable.fold (fun grammar str_from str_to ->
         let sym_from = Terminal str_from in
         let sym_to = Terminal str_to in
            List.map (fun (head, pos, body, prec, rewrites) ->
               (* Also replace in rule precedence symbol *)
               let prec =
                  match prec with
                     Some (s, pos) ->
                        Some (change_string replace s, pos)
                   | None ->
                        None
               in
               let pos: pos = pos in
                  change_symbol replace head,
                  pos,
                  List.map (fun (sym, pos) -> change_symbol replace sym, pos) body,
                  prec,
                  rewrites) grammar) grammar replace
   in
   (* Calculate new regular expression list.
    * Note, that we obey the invariant that newest regexp is first.
    *)
   let regexps =
      StringSerialMap.fold (fun ttable tok (regexps, ignore) ->
         (ignore, tok, StringSet.to_list regexps) :: ttable) [] ttable
   in
      { grammar_nonterminals     = nonterminals;
        grammar_terminals        = terminals;
        grammar_assocs           = assocs;
        grammar_token_rules      = token_rules;
        grammar_start_symbol     = start_symbol;
        grammar_grammar          = grammar;
        grammar_termsets         = termsets;
        grammar_local_rewrites   = local_rewrites;
        grammar_post_rewrites    = post_rewrites;
        grammar_inline_forms     = inline_forms
      },
      { lexer_regexps            = regexps;
        lexer_options            = loptions;
        lexer_rewrites           = lex_rewrites
      }
