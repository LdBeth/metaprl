(*
 * Lexer for Phobos.
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

{
open Phobos_parser
open Phobos_parse_state
open Phobos_type
open Phobos_constants
open Phobos_util

(*
 * File position.
 *)
let current_line = ref 1
let current_schar = ref 0

(*
 * Advance a line.
 *)
let set_next_line lexbuf =
   incr current_line;
   current_schar := Lexing.lexeme_end lexbuf

(*
 * Get the position of the current lexeme.
 * We assume it is all on one line.
 *)
let set_lexeme_position lexbuf =
   let line = !current_line in
   let schar = Lexing.lexeme_start lexbuf - !current_schar in
   let echar = Lexing.lexeme_end lexbuf - !current_schar in
   let file = current_file () in
   let pos = file, line, schar, line, echar in
      set_current_position pos;
      pos

(*
 * Provide a buffer for building strings.
 *)
let stringbuf = Buffer.create 32
let string_start = ref (0, 0)

let string_add_char c =
   Buffer.add_string stringbuf 

let pop_string lexbuf =
   let s = Buffer.contents stringbuf in
   let sline, schar = !string_start in
   let eline = !current_line in
   let echar = Lexing.lexeme_end lexbuf - !current_schar in
   let pos = current_file (), sline, schar, eline, echar in
      Buffer.clear stringbuf;
      s, pos

let set_string_start lexbuf =
   string_start := !current_line, Lexing.lexeme_start lexbuf - !current_schar
}

(*
 * Regular expressions.
 *)
let decimal = ['0'-'9']+
let octal = '0' ['0'-'7']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+

let float1 = decimal '.' decimal (('e' | 'E') ('+' | '-')? decimal)?
let float2 = decimal (('e' | 'E') ('+' | '-')? decimal)?
let float = float1 | float2

let name_prefix = ['_' 'A'-'Z' 'a'-'z']
let name_suffix = ['_' 'A'-'Z' 'a'-'z' '0'-'9']
let basic_name = name_prefix name_suffix*
let name = basic_name '\''*
let quoted_name = '\'' basic_name
let option = '-' name_suffix*
let directive = "%" name_suffix*

let decimal_char= ['0'-'9']
let decimal	= ['1'-'9'] decimal_char*

let char_normal = [' '-'\255']

(*
 * Main lexer.
 *)
rule main = parse 
     [' ' '\t']+        { main lexbuf }
   | '\n'               { set_next_line lexbuf; main lexbuf }
   | '_'                { TokId ("_", set_lexeme_position lexbuf) }
   | decimal            { let pos = set_lexeme_position lexbuf in
                           TokInt (int_of_string (Lexing.lexeme lexbuf), pos)
                        }
   | octal              { let pos = set_lexeme_position lexbuf in
                          let s = Lexing.lexeme lexbuf in
                           TokInt (int_of_string ("0o" ^ s), pos)
                        }
   | hex                { let pos = set_lexeme_position lexbuf in
                          let s = Lexing.lexeme lexbuf in
                           TokInt (int_of_string s, pos)
                        }
   | float              { let pos = set_lexeme_position lexbuf in
                          let s = Lexing.lexeme lexbuf in
                           TokFloat (float_of_string s, pos)
                        }
   | quoted_name        { let pos = set_lexeme_position lexbuf in
                          let s = Lexing.lexeme lexbuf in
                           TokQuotedId (String.sub s 1 (String.length s - 1), pos)
                        }
   | name               { let pos = set_lexeme_position lexbuf in
                          let old_id = Lexing.lexeme lexbuf in
                          let id = String.lowercase old_id in
                          match id with 
                             "module" ->
                              TokModule pos
                           | "include" ->
                              TokInclude pos
                           | "declare" ->
                              TokDeclare pos
                           | "inline" ->
                              TokInline pos
                           | "terms" ->
                              TokTerms pos
                           | "grammar" ->
                              TokGrammar pos
                           | "tokens" ->
                              TokTokens pos
                           | "rewrites" ->
                              TokRewrites pos
                           | _ ->
                              TokId (old_id, pos)
                        }
   | option             { let pos = set_lexeme_position lexbuf in
                          let old_id = Lexing.lexeme lexbuf in
                          let old_id = String.sub old_id 1 (String.length old_id - 1) in
                          let id = String.lowercase old_id in
                          match id with 
                           | "longest" ->
                              TokLongest pos
                           | "first" ->
                              TokFirst pos
                           | "start" ->
                              TokStart pos
                           | "extend" ->
                              TokExtend pos
                           | "remove" ->
                              TokRemove pos
                           | "override" ->
                              TokOverride pos
                           | _ ->
                              TokOption (old_id, pos)
                        }

   | directive          { let pos = set_lexeme_position lexbuf in
                          let old_id = Lexing.lexeme lexbuf in
                          let old_id = String.sub old_id 1 (String.length old_id - 1) in
                          let id = String.lowercase old_id in
                          match id with 
                           | "nonassoc" ->
                              TokNonAssoc pos
                           | "left" ->
                              TokLeftAssoc pos
                           | "right" ->
                              TokRightAssoc pos
                           | "prec" ->
                              TokPrec pos
                           | _ ->
                              raise (ParseError (pos, string_add ["Invalid directive ["; id; "]"]))
                        }

   | "//" [^ '\n']* '\n'{ set_next_line lexbuf; main lexbuf }
   | "(*"               { comment lexbuf; main lexbuf }
   | "/*"               { comment lexbuf; main lexbuf }
   | "\""               { set_string_start lexbuf;
                          string lexbuf;
                          let s, pos = pop_string lexbuf in
                           TokString(s, pos)
                        }
   (*
    * Special chars.
    *)
   | "@"                { let pos = set_lexeme_position lexbuf in TokAt pos }
   | "="                { let pos = set_lexeme_position lexbuf in TokEq pos }
   | "::="              { let pos = set_lexeme_position lexbuf in TokRuleEq pos }
   | "->"               { let pos = set_lexeme_position lexbuf in TokArrow pos }
   | "=>"               { let pos = set_lexeme_position lexbuf in TokDoubledArrow pos }
   | "*"                { let pos = set_lexeme_position lexbuf in TokIgnore pos }
   | "|"                { let pos = set_lexeme_position lexbuf in TokPipe pos }
   | ";"                { let pos = set_lexeme_position lexbuf in TokSemi pos }
   | ":"                { let pos = set_lexeme_position lexbuf in TokColon pos }
   | ","                { let pos = set_lexeme_position lexbuf in TokComma pos }
   | "{"                { let pos = set_lexeme_position lexbuf in TokLeftBrace pos }
   | "}"                { let pos = set_lexeme_position lexbuf in TokRightBrace pos }
   | "["                { let pos = set_lexeme_position lexbuf in TokLeftBrack pos }
   | "]"                { let pos = set_lexeme_position lexbuf in TokRightBrack pos }
   | "!"                { let pos = set_lexeme_position lexbuf in TokBang pos }
   | "."                { let pos = set_lexeme_position lexbuf in TokDot pos }
   | "?"                { let pos = set_lexeme_position lexbuf in TokQuestionMark pos }
   | "<"                { let pos = set_lexeme_position lexbuf in TokLe pos }
   | ">"                { let pos = set_lexeme_position lexbuf in TokGe pos }

   | eof                { TokEof }
   | _                  { let pos = set_lexeme_position lexbuf in
                          raise (ParseError (pos, Printf.sprintf "illegal char: '%s'"
                          (String.escaped (Lexing.lexeme lexbuf))))
                        }

and string = parse 
     "\"" | eof         { () }
   | '\n'               { set_next_line lexbuf; string lexbuf }
   | "\\n"              { Buffer.add_char stringbuf '\n'; string lexbuf }
   | "\\t"              { Buffer.add_char stringbuf '\t'; string lexbuf }
   | "\\\'"             { Buffer.add_char stringbuf '\''; string lexbuf }
   | "\\\""             { Buffer.add_char stringbuf '"'; string lexbuf }
   | _                  { Buffer.add_string stringbuf (Lexing.lexeme lexbuf);
                          string lexbuf
                        }
   
and comment = parse 
     "(*"               { comment lexbuf; comment lexbuf }
   | "/*"               { comment lexbuf; comment lexbuf }
   | "*)"               { () }
   | "*/"               { () }
   | eof                { () }
   | '\n'               { set_next_line lexbuf;
                          comment lexbuf
                        }
   | _                  { comment lexbuf }

