(*
 * Copy the text into a buffer, replacing variables of the form %%name%%
 *
 * ----------------------------------------------------------------
 *
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

{
type token =
   TokString of string
 | TokEqual
 | TokEof

let string_buf = Buffer.create 32
}

(*
 * Tokens.
 *)
let white = [' ' '\t' '\r' '\n']+
let name = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
let escapec = '\\' ['r' 'n' 't' '"']
let escaped = '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']

(*
 * Lexer definition.
 *)
rule main = parse
   white
   { main lexbuf }
 | name
   { TokString (Lexing.lexeme lexbuf) }
 | '='
   { TokEqual }
 | '"'
   { Buffer.clear string_buf;
     string lexbuf;
     TokString (Buffer.contents string_buf)
   }
 | _
 | eof
   { TokEof }

and string = parse
   escapec
   { let s = Lexing.lexeme lexbuf in
     let c =
        match s.[1] with
           'r' -> '\r'
         | 't' -> '\t'
         | 'n' -> '\n'
         | c -> c
     in
        Buffer.add_char string_buf c;
        string lexbuf
   }
 | escaped
   { let s = Lexing.lexeme lexbuf in
     let code =
        Char.code s.[1] * 100
        + Char.code s.[2] * 10
        + Char.code s.[3]
        - 111 * Char.code '0'
     in
     let c = Char.chr (code land 0xff) in
        Buffer.add_char string_buf c;
        string lexbuf
   }
 | '"'
 | eof
   { () }
 | _
   { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
     string lexbuf
   }

{
(*
 * Read variables from the file.
 *)
let rec parse args lexbuf =
   match main lexbuf with
      TokString v ->
         parse_var args v lexbuf
    | TokEqual ->
         parse args lexbuf
    | TokEof ->
         args

and parse_var args v lexbuf =
   match main lexbuf with
      TokString _ ->
         parse args lexbuf
    | TokEqual ->
         parse_value args v lexbuf
    | TokEof ->
         args

and parse_value args v lexbuf =
   match main lexbuf with
      TokString x ->
         parse ((String.lowercase v, x) :: args) lexbuf
    | TokEqual ->
         parse args lexbuf
    | TokEof ->
         args

(*
 * Parse the input file.
 *)
let parse_file filename =
   try
      let inx = open_in filename in
      let lexbuf = Lexing.from_channel inx in
      let args = parse [] lexbuf in
         close_in inx;
         args
   with
      Sys_error _ ->
         []
}

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
