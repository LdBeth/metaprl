(*
 * Resources for parsing and lexing.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
extends Perv

open Lm_symbol

open Refiner.Refiner.TermType

(************************************************************************
 * Lexer.
 *)
type lexer_item = string * (string array -> symbol * term)

type lexer

resource (lexer_item, lexer) lexer

(*
 * Nearly every grammar will need eof.
 *)
declare TokEof    (* eof *)

(************************************************************************
 * Parser.
 *)
type assoc = Lm_parser.assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc

type parser_item =
   ParseStart      of symbol list
 | ParsePrec       of assoc * Precedence.precedence * symbol list
 | ParseProduction of symbol * symbol list * symbol option * (term array -> term)

type parser

resource (parser_item, parser) parser

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
