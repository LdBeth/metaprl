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

open Rformat
open Phobos_constants
open Phobos_parse_state
open Phobos_type
open Refiner.Refiner.TermType
open Simple_print.SimplePrint

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

let format term buf term =
   format_string buf (string_of_bterm term)

let loc_of_string loc term s =
   LocString (s, term, loc)

let loc_start term s =
   LocStart (s, term)

let rec format_loc buf = function
   LocString (s, term, loc) ->
      let opname, _, _ = breakup_bterm term in
      let term_string = string_of_opname opname in
      format_loc buf loc;
      format_string buf "\\";
      format_string buf s;
      format_string buf " (";
      format_string buf term_string;
      format_string buf ")\n"
 | LocStart (s, term) ->
      let opname, _, _ = breakup_bterm term in
      let term_string = string_of_opname opname in
      format_string buf "\\";
      format_string buf s;
      format_string buf " (";
      format_string buf term_string;
      format_string buf ")\n"

let format_loc_last buf = function
   LocString (s, term, loc) ->
      format_string buf " (";
      format term buf term;
      format_string buf ")\n"
 | LocStart (s, term) ->
      format_string buf " (";
      format term buf term;
      format_string buf ")\n"

let format_exn buf = function
   PhobosException (pos, s) ->
      format_string buf "*** Phobos error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf "\n";
      format_pos buf pos
 | PhobosError s ->
      format_string buf "*** Phobos error:\n";
      format_string buf "*** Error: ";
      format_string buf s
 | LexerException s ->
      format_string buf "*** Phobos lexer error:\n";
      format_string buf "*** Error: ";
      format_string buf s
 | LexerPosException (pos, s) ->
      format_string buf "*** Phobos lexer error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf "\n";
      format_pos buf pos
 | RewriteException (pos, s) ->
      format_string buf "*** Phobos rewrite error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf "\n";
      format_pos buf pos
 | ConvertException (loc, ParamError (param, s)) ->
      format_string buf "*** Phobos convert error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf "\n";
      format_loc buf loc
 | ConvertException (loc, ParamError2 (param1, param2, s)) ->
      format_string buf "*** Phobos convert error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf "\n";
      format_loc buf loc
 | ConvertException (loc, TermError (term, opname, s)) ->
      format_string buf "*** Phobos convert error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf " - [";
      format_string buf opname;
      format_string buf "]\n";
      format_loc buf loc;
      format_string buf "Error in\n";
      format_loc_last buf loc
 | ParseError (pos, s) ->
      format_string buf "*** Parse error:\n";
      format_string buf "*** Error: ";
      format_string buf s;
      format_string buf "\n";
      format_pos buf pos
 | Parsing.Parse_error ->
      format_string buf "*** Phobos syntax error:\n";
      format_pos buf (current_position ())
 | SyntaxError pos ->
      format_string buf "*** Syntax error:\n";
      format_pos buf pos
 | (Refiner.Refiner.RefineError.RefineError _) as exn ->
      Refine_exn.print_exn Dform_print.null_mode_base stderr "*** MetaPRL error:" exn  
 | exn ->
      Filter_exn.format_exn Dform.null_base buf exn


let format_exn_chan out exn =
   let buf = new_buffer () in
      format_exn buf exn;
      format_newline buf;
      print_to_channel default_width buf out;
      flush out

let catch f x =
   try
      f x
   with
      exn ->
         format_exn_chan stderr exn;
         exit 2
