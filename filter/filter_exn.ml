(*
 * Print exceptions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf

open Nl_debug

open Rformat
open Dform
open Dform_print
open Simple_print.SimplePrint

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
let rec format_exn db buf exn =
   let format = function
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
         format_space buf;
         format_string buf s
    | EmptyModulePath s ->
         format_string buf "Empty module path:";
         format_space buf;
         format_string buf s
    | Stdpp.Exc_located ((start, finish), exn) ->
         format_string buf "Chars ";
         format_int buf start;
         format_string buf "-";
         format_int buf finish;
         format_string buf ": ";
         format_exn db buf exn
    | exn ->
         Refine_exn.format_exn db buf exn
   in
      format exn

(*
 * Print an exception if it occurs, then reraise it.
 *)
let print_exn db out exn =
   let buf = new_buffer () in
      format_exn db buf exn;
      format_newline buf;
      print_to_channel 80 buf stderr;
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
