(*
 * Print exceptions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Printf

open Mp_debug

open Rformat
open Dform
open Dform_print
open Simple_print.SimplePrint
open File_type_base

open Filter_ocaml
open Filter_type

(*
 * Print an argument list.
 *)
let rec format_arg_list db buf = function
   (sl, t) :: tl ->
      let rec format = function
         h::t ->
            format_string buf h;
            format_space buf;
            format t
       | [] ->
            format_term db buf t
      in
         format sl;
         format_space buf;
         format_arg_list db buf tl
 | [] ->
      ()

(*
 * Convert an exception to a string.
 *)
let rec format_exn db buf = function
   FormatError (name, t) ->
      format_string buf "FormatError:";
      format_space buf;
      format_string buf name;
      format_space buf;
      format_term db buf t
 | NotANumber name ->
      format_string buf "Not a number:";
      format_space buf;
      format_string buf name
 | BadParam name ->
      format_string buf "Bad parameter:";
      format_space buf;
      format_string buf name
 | BadLevelExp l ->
      format_string buf "Bad level expression:";
      format_space buf;
      format_simple_level_exp buf l
 | BadParamCast (p, s) ->
      format_string buf "Bad param cast:";
      format_space buf;
      format_simple_param buf p;
      format_space buf;
      format_string buf "to";
      format_space buf;
      format_string buf s
 | BadArgList l ->
      format_string buf "Bad argument list:";
      format_space buf;
      format_arg_list db buf l
 | BadBinder t ->
      format_string buf "Bad binder:";
      format_space buf;
      format_term db buf t
 | ParseError s ->
      format_string buf "Parse error:";
      format_space buf;
      format_string buf s
 | BadCommand s ->
      format_string buf "Bad command:";

      format_string buf s
 | EmptyModulePath s ->
      format_string buf "Empty module path:";
      format_space buf;
      format_string buf s
 | Bad_magic s ->
      format_string buf "File ";
      format_string buf s;
      format_string buf " has a wrong magic number.";
      format_newline buf;
      format_string buf "This means that is is either not a MetaPRL file";
      format_newline buf;
      format_string buf "or is not compatible with the version of the MetaPRL you are trying to use.";
      format_newline buf;
      format_string buf "If you are sure this file does not contain any unsaved data, delete it.";
      format_newline buf;
      format_string buf "If it does contain unsaved data, you might need to get a different version of MetaPRL";
      format_newline buf;
      format_string buf "and possibly export the data to a different format."
 | Stdpp.Exc_located ((start, finish), exn) ->
      format_pushm buf 3;
      format_string buf "Chars ";
      format_int buf start;
      format_string buf "-";
      format_int buf finish;
      format_string buf ": ";
      format_szone buf;
      format_exn db buf exn;
      format_ezone buf;
      format_popm buf;
 | Pcaml.Qerror (name, where, exn) ->
      let name = if name = "" then !(Quotation.default) else name in
      format_pushm buf 3;
      format_string buf ("While " ^
         (match where with
            Pcaml.Finding -> "finding quotation"
          | Pcaml.Expanding -> "expanding quotation"
          | Pcaml.ParsingResult _ -> "parsing result of quotation"
          | Pcaml.Locating -> "parsing"
         ) ^ " \"" ^ name ^ "\":");
      format_space buf;
      format_szone buf;
      format_exn db buf exn;
      format_ezone buf;
      format_popm buf
 | exn ->
      Refine_exn.format_exn db buf exn

(*
 * Print an exception if it occurs, then reraise it.
 *)
let print_exn db out exn =
   let buf = new_buffer () in
      format_exn db buf exn;
      format_newline buf;
      print_to_channel default_width buf stderr;
      flush stderr;
      raise exn

let print db f x =
   try f x with
      exn ->
         print_exn db stderr exn

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
