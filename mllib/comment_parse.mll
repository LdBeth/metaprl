(*
 * This is a simple lexer to extract terms from a comment string.
 * Grammar:
 *
 * Text is a sequence of tokens containing strings, terms, and quotations
 * Literal-text is any sequence of chars not containing the trailing delimiter.
 *
 * For text:
 *    1. Whitespace is ignored
 *    2. Strings are any sequence of non-special chars
 *    3. Variables are alphanumeric names preceded by a single quote
 *    4. Quoted strings are surrounded by double-quotes
 *    5. There are three types of terms:
 *       a. @opname[params]{args}
 *          Opname is an alphnumeric sequence, or a quoted string
 *          [params] are optional; a param is a string or quoted string
 *          {args} are optional; a sequence of text separated by ;
 *
 *       b. @begin[name]
 *          text
 *          @end[name]
 *
 *          This builds a term @name{text}
 *
 *       c. $literal-str$
 *          This builds the term @math[str]
 *    6. Quotations have the form
 *       <<literal-str>> and <:tag<literal-str>>
 *       are also allowed.
 *
 * Special forms:
 *
 * Quotations can be nested:
 *    TokQuote ("", text):   << text >>
 *    TokQuote (tag, text):  <:tag< text >>
 *
 * TokName:                  @opname
 * TokString (b, s):         any sequence of non-whitespace, non-special chars;
 *                           b is true iff the string can be used in opname position
 * TokVariable s:            a variable name
 * TokQString s:             any text between double-quotes
 * TokMath s:                any literal text between $
 *
 * TokWhite:                 whitespace
 * TokLeftBrace:             {
 * TokRightBrace:            }
 * TokLeftBrack:             [
 * TokRightBrack:            ]
 * TokComma:                 ,
 * TokSemi:                  ;
 *
 * The lexer removes the first leading * on each line of the
 * input string.
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

{
(*
 * A program is a sequence of strings and terms.
 *)
type t = item list

and item =
   White
 | String of string
 | Variable of string
 | Term of opname * string list * t list
 | Quote of loc * string * string
 | Block of t

and opname = string list * loc

and loc = int * int

(*
 * Tokens.
 *)
type token =
   TokWhite of bool
 | TokQString of string
 | TokMath of bool
 | TokString of bool * string
 | TokVariable of string
 | TokQuote of string * string
 | TokName of string
 | TokLeftBrace
 | TokRightBrace
 | TokSpecial of char
 | TokEof

type code_token =
   CodeString of string
 | CodeEnd

(*
 * Items returned by the item parser.
 *)
type item_item =
   ItemItem of item
 | ItemMath of bool
 | ItemEnd of string list
 | ItemSpecial of char
 | ItemBrace
 | ItemEof

(*
 * Modes.
 * The parser can be looking for arguments (so the ';' and ','
 *    chars are special),
 * and it can be in math mode (so the '_' and '^' chars are special)
 *)
type mode =
   ModeNormal
 | ModeArg
 | ModeMath
 | ModeArgMath

(*
 * Turn a variable into a string.
 *)
let varname_of_string s =
   let len = String.length s in
      if len = 0 then
          raise (Invalid_argument "varname_of_string");
      String.sub s 1 (len - 1)

(*
 * Test for special chars.
 *)
let is_special mode c =
   let special =
      match mode with
         ModeNormal -> []
       | ModeArg -> [';']
       | ModeMath -> ['_'; '^']
       | ModeArgMath -> ['_'; '^'; ';']
   in
      List.mem c special

(*
 * Test for math mode.
 *)
let is_math_mode = function
   ModeMath
 | ModeArgMath ->
      true
 | ModeNormal
 | ModeArg ->
      false

(*
 * Move to arg mode.
 *)
let arg_mode = function
   ModeNormal
 | ModeArg ->
    ModeArg
 | ModeMath
 | ModeArgMath ->
    ModeArgMath

let math_mode = function
   ModeNormal
 | ModeMath ->
    ModeMath
 | ModeArg
 | ModeArgMath ->
    ModeArgMath

let non_arg_mode = function
   ModeNormal
 | ModeArg ->
    ModeNormal
 | ModeMath
 | ModeArgMath ->
    ModeMath

(*
 * Termination items.
 *)
type terminator =
   TermEof
 | TermBrace
 | TermSemi
 | TermMath of bool
 | TermEnd of string list

let string_of_term = function
   TermEof -> "EOF"
 | TermBrace -> "{}"
 | TermSemi -> ";"
 | TermMath false -> "$"
 | TermMath true -> "$$"
 | TermEnd l -> "end(" ^ (String.concat "," l) ^ ")"

(*
 * State for parsing quotations.
 *)
let level = ref 0
let tag = ref ""
let buffer = Buffer.create 19

let set_tag tag' =
   tag := tag';
   Buffer.clear buffer

let add_string s =
   Buffer.add_string buffer s

let flush_buffer () =
   let s = Buffer.contents buffer in
      Buffer.clear buffer;
      TokQuote (!tag, s)

let flush_string () =
   let s = Buffer.contents buffer in
      Buffer.clear buffer;
      s

(*
 * Pushback buffer.
 *)
type token_buffer =
   { lexbuf : Lexing.lexbuf;
     mutable tokens : token list
   }

(*
 * Errors.
 *)
exception Parse_error of string * loc

let loc_of_lexbuf lexbuf =
   Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf

let loc_of_buf buf =
   loc_of_lexbuf buf.lexbuf

let parse_error_buf s lexbuf =
   raise (Parse_error (s, loc_of_lexbuf lexbuf))

let parse_error s buf =
   parse_error_buf s buf.lexbuf
}

let white = [' ' '\t']+
let optwhite = [' ' '\t']*
let newline = ['\r' '\n']
let name = ['a'-'z''A'-'Z']+
let number = ['0'-'9']+
let special = ['[' ']' ';' ',' '_' '^' '!']
let varname = '\'' ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule main = parse
   (* White space *)
   newline
   { TokWhite true }
 | white
   { TokWhite false }

   (* Nested comments *)
 | "(*"
   { comment lexbuf; main lexbuf }

   (* Quotations *)
 | "<<"
   { set_tag "";
     quotation lexbuf
   }
 | "<:" name '<'
   { let buf = Lexing.lexeme lexbuf in
        set_tag (String.sub buf 2 (pred (String.length buf)));
        quotation lexbuf
   }

   (* This is temporary *)
 | '\\'
   { parse_error_buf "fix this backslash" lexbuf }

   (* Strings *)
 | '"'
   { TokQString (string lexbuf) }
 | '\'' varname '\''
   { TokString (false, Lexing.lexeme lexbuf) }
 | '\'' varname
   { TokVariable (Lexing.lexeme lexbuf) }

   (* Special tokens *)
 | "$$"
   { TokMath true }
 | '$'
   { TokMath false }
 | '@'
   { opname lexbuf }
 | '{'
   { TokLeftBrace }
 | '}'
   { TokRightBrace }
 | special
   { TokSpecial (Lexing.lexeme_char lexbuf 0) }

   (* Alphanumeric names *)
 | name
 | number
   { TokString (true, Lexing.lexeme lexbuf) }
 | _
   { TokString (false, Lexing.lexeme lexbuf) }

 | eof
   { TokEof }

(*
 * Comments.
 *)
and comment = parse
   "(*"
   { comment lexbuf; comment lexbuf }
 | "*)"
   { () }
 | _
   { comment lexbuf }
 | eof
   { parse_error_buf "comment is not terminated" lexbuf }

(*
 * Read the first string in the opname.
 *)
and opname = parse
   name
   { TokName (Lexing.lexeme lexbuf) }
 | '"'
   { TokName (string lexbuf) }
 | _
   { TokString (false, Lexing.lexeme lexbuf) }
 | eof
   { parse_error_buf "opname is not terminated" lexbuf }

(*
 * Quotations.
 * Watch for nested quotations.
 *)
and quotation = parse
   "<<"
   { add_string "<<";
     incr level;
     quotation lexbuf
   }
 | "<:" name '<'
   { add_string (Lexing.lexeme lexbuf);
     incr level;
     quotation lexbuf
   }
 | ">>"
   { if !level = 0 then
        flush_buffer ()
     else
        begin
           add_string ">>";
           decr level;
           quotation lexbuf
        end
   }
 | _
   { add_string (Lexing.lexeme lexbuf);
     quotation lexbuf
   }
 | eof
   { parse_error_buf "quotation is not terminated" lexbuf }

(*
 * Strings.
 * Remove escaped eol.
 *)
and string = parse
   '"' (* '"' *)
      { flush_string () }
 | '\\'
      { escape lexbuf }
 | _
      { add_string (Lexing.lexeme lexbuf);
        string lexbuf
      }
 | eof
      { parse_error_buf "string is not terminated" lexbuf }

and escape = parse
    newline
      { string lexbuf }
  | _
      { add_string (Lexing.lexeme lexbuf);
        string lexbuf
      }
  | eof
      { parse_error_buf "escape sequence is not terminated" lexbuf }

(*
 * Literal forms.
 *)
and code_string_brace = parse
   newline optwhite '*'
      { CodeString "\n" }
 | '}'
      { CodeEnd }
 | _
      { CodeString (Lexing.lexeme lexbuf) }
 | eof
      { parse_error_buf "code string is not terminated" lexbuf }

and code_string_end = parse
   newline optwhite '*'
      { CodeString "\n" }
 | "@end[verbatim]"
 | "@end[literal]"
 | "@end[html]"
      { CodeEnd }
 | _
      { CodeString (Lexing.lexeme lexbuf) }
 | eof
      { parse_error_buf "code block is not terminated" lexbuf }

{
(*
 * In math mode, add the "math_" prefix to the opname.
 *)
let rec mk_math_opname = function
   [name] ->
      ["math_" ^ name]
 | h :: t ->
      h :: mk_math_opname t
 | [] ->
      raise (Invalid_argument "Comment_parse.mk_math_opname")

let mk_opname mode opname =
   if is_math_mode mode then
      mk_math_opname opname
   else
      opname

(************************************************************************
 * Pushback buffer.
 *)
let parse_token buf =
   match buf with
      { tokens = token :: t } ->
         buf.tokens <- t;
         token
    | { lexbuf = lexbuf } ->
         main lexbuf

let push_back token buf =
   buf.tokens <- token :: buf.tokens

(*
 * Special forms.
 *)
let parse_code_string_brace buf =
   assert (buf.tokens = []);
   code_string_brace buf.lexbuf

let parse_code_string_end buf =
   assert (buf.tokens = []);
   code_string_end buf.lexbuf

(************************************************************************
 * Toplevel recursive-descent parser.
 *    term: the expected terminator for this block
 *    mode: the current parsing mode
 *    items: the list of items collected so far (in reverse order)
 *    buf: the token buffer
 *)
let rec parse_block term mode items buf =
   let item = parse_item mode buf in
      match item with
         ItemItem item ->
            parse_block term mode (item :: items) buf
       | ItemSpecial '_' ->
               parse_math_script term mode "math_subscript" items buf
       | ItemSpecial '^' ->
            parse_math_script term mode "math_superscript" items buf
       | ItemMath flag ->
            finish_block term (TermMath flag) items buf
       | ItemEnd tag ->
            finish_block term (TermEnd tag) items buf
       | ItemSpecial ';' ->
            finish_block term TermSemi items buf
       | ItemBrace ->
            finish_block term TermBrace items buf
       | ItemEof ->
            finish_block term TermEof items buf
       | ItemSpecial _ ->
            parse_error "illegal special character" buf

(*
 * Found a terminator.
 *)
and finish_block term term' items buf =
   if not (List.mem term' term) then
      parse_error ("terminator mismatch (" ^ (string_of_term term') ^ ")") buf;
   term', List.rev items

(*
 * Math mode sub/superscripts.
 *)
and parse_math_script term mode opname items buf =
   let loc = loc_of_buf buf in
   match items, parse_item (math_mode mode) buf with
      item :: items, ItemItem item' ->
         let items = Term (([opname], loc), [], [[item]; [item']]) :: items in
            parse_block term mode items buf
    | _ ->
         parse_error "illegal sub/superscript operation" buf

(*
 * Parse the entire next item.
 *)
and parse_item mode buf =
   let token = parse_token buf in
      match token with
         TokWhite is_nl_flag ->
            ItemItem (parse_white is_nl_flag buf)
       | TokMath flag ->
            if is_math_mode mode then
               ItemMath flag
            else
               let loc = loc_of_buf buf in
               let opname =
                  if flag then
                     "centermath"
                  else
                     "math"
               in
               let _, items = parse_block [TermMath flag] ModeMath [] buf in
                  ItemItem (Term (([opname], loc), [], [items]))
       | TokName s ->
            parse_term mode s buf
       | TokQuote (tag, next) ->
            let loc = loc_of_buf buf in
            let item = Quote (loc, tag, next) in
            if is_math_mode mode then ItemItem item
            else ItemItem (Term ((["math"], loc), [], [[item]]))
       | TokQString s ->
            ItemItem (String ("\"" ^ s ^ "\""))
       | TokString (_, s) ->
            ItemItem (String s)
       | TokVariable s ->
            ItemItem (Variable (varname_of_string s))
       | TokLeftBrace ->
            let _, items = parse_block [TermBrace] (non_arg_mode mode) [] buf in
               ItemItem (Block items)
       | TokRightBrace ->
            ItemBrace
       | TokSpecial c ->
            if is_special mode c then
               ItemSpecial c
            else
               ItemItem (String (String.make 1 c))
       | TokEof ->
            ItemEof

(*
 * Adjacent non-nl whitespace is concatenated.
 *)
and parse_white is_nl_flag buf =
   let token = parse_token buf in
      match token with
         TokWhite is_nl_flag' ->
            if is_nl_flag && is_nl_flag' then
               begin
                  push_back token buf;
                  White
               end
            else
               parse_white (is_nl_flag || is_nl_flag') buf
       | _ ->
            push_back token buf;
            White

(*
 * Adjacent strings are concatenated.
 *)
and parse_strings mode s buf =
   let buffer = Buffer.create 19 in
      Buffer.add_string buffer s;
      parse_strings' mode buffer buf

and parse_strings' mode buffer buf =
   let token = parse_token buf in
      match token with
         TokString (_, s) ->
            add_string mode s buffer buf
       | TokSpecial c ->
            if is_special mode c then
               flush_string token buffer buf
            else
               add_char mode c buffer buf
       | TokWhite _
       | TokVariable _
       | TokMath _
       | TokQString _
       | TokQuote _
       | TokName _
       | TokLeftBrace
       | TokRightBrace
       | TokEof ->
            flush_string token buffer buf

and add_string mode s buffer buf =
   Buffer.add_string buffer s;
   parse_strings' mode buffer buf

and add_char mode c buffer buf =
   Buffer.add_char buffer c;
   parse_strings' mode buffer buf

and flush_string token buffer buf =
   push_back token buf;
   String (Buffer.contents buffer)

(*
 * Parse a term.
 * There are several mode cases to consider.
 *)
and parse_term mode s buf =
   let l1, _ = loc_of_buf buf in
   let opname = parse_opname mode s buf in
   let _, l2 = loc_of_buf buf in
   let loc = l1, l2 in
   let params = parse_params mode false buf in
      if opname = ["code"] || opname = ["email"] then
         let s = parse_code_arg buf in
            ItemItem (Term ((opname, loc), [s], []))
      else
         let args =
            if opname = ["mbox"] || opname = ["hbox"] then
               parse_args ModeNormal false buf
            else
               parse_args mode false buf
         in let args =
            if args = [[]] then
               []
            else
               args
         in
            (* Mode cases *)
            match opname, params, args with
                ["begin"], [["verbatim" as tag]], []
              | ["begin"], [["literal" as tag]], []
              | ["begin"], [["html" as tag]], [] ->
                   let s = parse_code_block buf in
                      ItemItem (Term (([tag], loc), [s], []))
              | ["begin"], tag :: params, [] ->
                   let _, args = parse_block [TermEnd tag] mode [] buf in
                   let opname = mk_opname mode tag in
                      ItemItem (Term ((opname, loc), flatten_params params, [args]))
              | ["end"], [tag], [] ->
                   ItemEnd tag
              | _ ->
                   let opname = mk_opname mode opname in
                      ItemItem (Term ((opname, loc), flatten_params params, args))

(*
 * Code blocks.
 *)
and parse_code_arg buf =
   let buffer = Buffer.create 19 in
      match parse_token buf with
         TokLeftBrace ->
            parse_code_arg' buffer buf
       | token ->
            push_back token buf;
            ""

and parse_code_arg' buffer buf =
   match parse_code_string_brace buf with
      CodeString s ->
         Buffer.add_string buffer s;
         parse_code_arg' buffer buf
    | CodeEnd ->
         Buffer.contents buffer

and parse_code_block buf =
   let buffer = Buffer.create 19 in
      buf.tokens <- [];
      parse_code_block' buffer buf

and parse_code_block' buffer buf =
    match parse_code_string_end buf with
       CodeString s ->
          Buffer.add_string buffer s;
          parse_code_block' buffer buf
     | CodeEnd ->
          Buffer.contents buffer

(*
 * Opname.
 *)
and parse_opname mode s buf =
   let buffer = Buffer.create 19 in
      Buffer.add_string buffer s;
      parse_opname_name mode buffer [] buf

and parse_opname_name mode buffer opname buf =
   let token = parse_token buf in
      match token with
         TokString (true, s)
       | TokQString s ->
            add_opname_string mode buffer opname s buf
       | TokSpecial '_' ->
            if is_math_mode mode then
               flush_opname token buffer opname buf
            else
               add_opname_string mode buffer opname "_" buf
       | TokSpecial '!' ->
            push_opname mode buffer opname buf
       | _ ->
            flush_opname token buffer opname buf

and add_opname_string mode buffer opname s buf =
   Buffer.add_string buffer s;
   parse_opname_name mode buffer opname buf

and flush_opname token buffer opname buf =
   let s = Buffer.contents buffer in
   let opname =
      if s = "" then
         opname
      else
         s :: opname
   in
      Buffer.clear buffer;
      push_back token buf;
      List.rev opname

and push_opname mode buffer opname buf =
   let s = Buffer.contents buffer in
   let opname =
      if s = "" then
         opname
      else
         s :: opname
   in
      Buffer.clear buffer;
      parse_opname_name mode buffer opname buf

(*
 * Param list is a list of opnames.
 *)
and parse_params mode found_white buf =
   let token = parse_token buf in
      match token with
         TokWhite false ->
            parse_params mode true buf
       | TokSpecial '[' ->
            let buffer = Buffer.create 19 in
               parse_inner_params mode buffer [] buf
       | _ ->
            push_back token buf;
            if found_white then
               push_back (TokWhite false) buf;
            []

and parse_inner_params mode buffer items buf =
   let param = parse_opname_name ModeNormal buffer [] buf in
   let items =
      if param <> [] then
         param :: items
      else
         items
   in
      let token = parse_token buf in
         match token with
            TokWhite _
          | TokSpecial ','
          | TokSpecial ';' ->
               parse_inner_params mode buffer items buf
          | TokSpecial ']' ->
               List.rev items
          | TokName _
          | TokString _
          | TokVariable _
          | TokQString _
          | TokSpecial _
          | TokMath _
          | TokLeftBrace
          | TokRightBrace
          | TokQuote _
          | TokEof ->
               parse_error "illegal parameter" buf

and flatten_params params =
   List.map (fun l ->
      List.fold_left (fun buf s -> buf ^ s) "" l) params

(*
 * Arguments.
 *)
and parse_args mode found_white buf =
   let token = parse_token buf in
      match token with
         TokWhite false ->
            parse_args mode true buf
       | TokLeftBrace ->
            parse_inner_args (arg_mode mode) [] buf
       | _ ->
            push_back token buf;
            if found_white then
               push_back (TokWhite false) buf;
            []

and parse_inner_args mode items buf =
   let term, t = parse_block [TermSemi; TermBrace] mode [] buf in
   let items = t :: items in
      match term with
         TermSemi ->
            parse_inner_args mode items buf
       | TermBrace ->
            List.rev items
       | TermEof
       | TermEnd _
       | TermMath _ ->
            parse_error "illegal terminator" buf

(*
 * Main function.
 *)
let parse math s =
   let lexbuf = Lexing.from_string s in
   let buf = { lexbuf = lexbuf; tokens = [] } in
   let _, items = parse_block [TermEof] (if math then ModeMath else ModeNormal) [] buf in
      items
}

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
