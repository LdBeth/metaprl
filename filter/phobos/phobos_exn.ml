(*
 * Phobos exceptions.
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

open Phobos_constants
open Phobos_parse_state
open Phobos_type
open Refiner.Refiner.TermType
open Opname
open Simple_print.SimplePrint
open Format

type conv_exn = 
   ParamError of param' * string
 | ParamError2 of param' * param' * string
 | TermError of bound_term * string * string 

type conv_loc =
   LocString of string * bound_term * conv_loc
 | LocStart of string * bound_term

exception PhobosException of pos * string
exception PhobosError of string
exception LexerException of string
exception LexerPosException of pos * string
exception RewriteException of pos * string
exception ConvertException of conv_loc * conv_exn

exception SyntaxError of pos
exception SourceAccepted

let print_term term =
   print_string (string_of_bterm term)

let loc_of_string loc term s =
   LocString (s, term, loc)

let loc_start term s =
   LocStart (s, term)

let rec print_loc = function
   LocString (s, term, loc) ->
      let opname, _, _ = breakup_bterm term in
      let term_string = string_of_opname opname in
      print_loc loc;
      print_string "\\";
      print_string s;
      print_string " (";
      print_string term_string;
      print_string ")\n"
 | LocStart (s, term) ->
      let opname, _, _ = breakup_bterm term in
      let term_string = string_of_opname opname in
      print_string "\\";
      print_string s;
      print_string " (";
      print_string term_string;
      print_string ")\n"

let print_loc_last = function
   LocString (s, term, loc) ->
      print_string " (";
      print_term term;
      print_string ")\n"
 | LocStart (s, term) ->
      print_string " (";
      print_term term;
      print_string ")\n"

let print_exn = function
   PhobosException (pos, s) ->
      print_string "*** Phobos error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string "\n";
      print_pos pos
 | PhobosError s ->
      print_string "*** Phobos error:\n";
      print_string "*** Error: ";
      print_string s
 | LexerException s ->
      print_string "*** Phobos lexer error:\n";
      print_string "*** Error: ";
      print_string s
 | LexerPosException (pos, s) ->
      print_string "*** Phobos lexer error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string "\n";
      print_pos pos
 | RewriteException (pos, s) ->
      print_string "*** Phobos rewrite error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string "\n";
      print_pos pos
 | ConvertException (loc, ParamError (param, s)) ->
      print_string "*** Phobos convert error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string "\n";
      print_loc loc
 | ConvertException (loc, ParamError2 (param1, param2, s)) ->
      print_string "*** Phobos convert error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string "\n";
      print_loc loc
 | ConvertException (loc, TermError (term, opname, s)) ->
      print_string "*** Phobos convert error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string " - [";
      print_string opname;
      print_string "]\n";
      print_loc loc;
      print_string "Error in\n";
      print_loc_last loc
 | ParseError (pos, s) ->
      print_string "*** Parse error:\n";
      print_string "*** Error: ";
      print_string s;
      print_string "\n";
      print_pos pos
 | Parsing.Parse_error ->
      print_string "*** Phobos syntax error:\n";
      print_pos (current_position ())
 | SyntaxError pos ->
      print_string "*** Syntax error:\n";
      print_pos pos
 | (Refiner.Refiner.RefineError.RefineError _) as exn ->
      Refine_exn.print_exn Dform_print.null_mode_base stderr "*** MetaPRL error:" exn  
 | exn ->
(*      Fc_ir_exn_print.pp_print_exn err_formatter exn*)
      raise exn


let print_exn_chan out exn =
   flush stdout;
   flush out;
   print_string "\n";
   print_exn exn;
   print_string "\n"

let catch f x =
   try
      f x
   with
      exn ->
         print_exn_chan stderr exn;
         exit 2
