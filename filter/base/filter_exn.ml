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
open Lm_rformat
open Lm_rprintf
open Lexing

open Dform
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

let format_version buf i =
   let major, minor, rev = unpack_version i in
      format_int buf major;
      format_char buf '.';
      format_int buf minor;
      format_char buf '.';
      format_int buf rev

let format_loc buf (bp, ep) =
   if (bp.pos_fname <> "") && (bp.pos_fname <> "-") && (bp.pos_fname = ep.pos_fname) then begin
      format_string buf "File \"";
      format_string buf bp.pos_fname;
      format_string buf "\", line ";
   end
   else
      format_string buf "Line ";
   format_int buf bp.pos_lnum;
   format_string buf ", ";
   if bp.pos_lnum = ep.pos_lnum then begin
      format_string buf "characters ";
      format_int buf (bp.pos_cnum - bp.pos_bol + 1);
      format_string buf "-";
   end
   else begin
      format_string buf "char ";
      format_int buf (bp.pos_cnum - bp.pos_bol + 1);
      format_string buf " -- line ";
      format_int buf ep.pos_lnum;
      format_string buf ", char ";
   end;
   format_int buf (ep.pos_cnum - ep.pos_bol)

(*
 * Convert an exception to a string.
 *)
let rec format_exn db buf exn =
   match exn with
      FormatError (name, t) ->
         format_string buf "FormatError:";
         format_space buf;
         format_string buf name;
         format_space buf;
         format_term db buf t
    | BadParamCast (p, s) ->
         format_string buf "Bad param cast:";
         format_space buf;
         format_simple_param buf p;
         format_space buf;
         format_string buf "to";
         format_space buf;
         format_string buf s
    | ParseError s ->
         format_string buf "Parse error:";
         format_space buf;
         format_string buf s
    | IterfImplemMismatch s ->
         format_pushm buf 3;
         format_string buf "Mismatch between the interface and implementation:";
         format_newline buf;
         format_string buf s;
         format_popm buf
    | BadCommand s ->
         format_string buf "Bad command:";
         format_string buf s
    | EmptyModulePath s ->
         format_string buf "Empty module path:";
         format_space buf;
         format_string buf s
    | Bad_magic s ->
         format_string buf "! File ";
         format_string buf s;
         format_string buf " has a wrong magic number.";
         format_newline buf;
         format_string buf "! This means that is is either not a MetaPRL file";
         format_newline buf;
         format_string buf "! or is not compatible with the version of the MetaPRL you are trying to use.";
         format_newline buf;
         format_string buf "! If you are sure this file does not contain any unsaved data, delete it.";
         format_newline buf;
         format_string buf "! If it does contain unsaved data, you might need to get a different version of MetaPRL";
         format_newline buf;
         format_string buf "! and possibly export the data to a different format."
    | Bad_version(s,versions,version) ->
         format_string buf "! File ";
         format_string buf s;
         format_string buf " has an unsupported version.";
         format_newline buf;
         format_string buf "! MetaPRL currently supports version(s) ";
         List.iter (fun v -> format_version buf v; format_string buf ", ") versions;
         format_newline buf;
         format_string buf "! but the file has version ";
         format_version buf version;
         format_newline buf;
         format_string buf "! If you are sure this file does not contain any unsaved data, delete it.";
         format_newline buf;
         format_string buf "! If it does contain unsaved data, you might need to get a different version of MetaPRL";
         format_newline buf;
         format_string buf "! and possibly export the data to a different format."
    | Stdpp.Exc_located (loc, exn) ->
         format_pushm buf 3;
         format_loc buf loc;
         format_string buf ": ";
         format_newline buf;
         format_szone buf;
         format_exn db buf exn;
         format_ezone buf;
         format_popm buf;
    | Pcaml.Qerror (name, where, exn) ->
         let name =
            if name = "" then
               !Quotation.default
            else
               name
         in
            format_pushm buf 3;
            format_string buf "While ";
            (match where with
                Pcaml.Finding -> format_string buf "finding quotation"
              | Pcaml.Expanding -> format_string buf "expanding quotation"
              | Pcaml.ParsingResult _ -> format_string buf "parsing result of quotation"
              | Pcaml.Locating -> format_string buf "parsing");
            format_string buf (" \"" ^ String.escaped name ^ "\":");
            format_space buf;
            format_szone buf;
            format_exn db buf exn;
            format_ezone buf;
            format_popm buf
    | Lm_parser.ParseError loc ->
         format_pushm buf 3;
         format_string buf "Syntax error:";
         format_space buf;
         (* XXX: HACK (jyh, nogin) The next two lines are a BIG hack, not threads-safe *)
         Lm_location.pp_print_location Lm_printf.stdstr loc;
         format_string buf (Lm_printf.flush_stdstr ());
         format_popm  buf
    | exn ->
         Exn_boot.format_exn db buf exn

(*
 * Print an exception if it occurs, then reraise it.
 *)
let dont_print_exn _ _ f x =
   f x

let format_message buf s =
   match s with
      None ->
         format_pushm buf 0;
         format_szone buf
    | Some s ->
         format_pushm buf 3;
         format_string buf s;
         format_szone buf

let print_exn db s f x =
   try f x with
      exn ->
         let buf = new_buffer () in
            format_message buf s;
            format_exn db buf exn;
            format_ezone buf;
            format_popm buf;
            format_newline buf;
            output_rbuffer stderr buf;
            flush stderr;
            raise exn

let handle_exn db s loc f =
   try f () with
      exn ->
         let exn =
            match exn with
               Stdpp.Exc_located _ ->
                  exn
             | _ ->
                  Stdpp.Exc_located (loc, exn)
         in
         let buf = new_buffer () in
         let () = format_message buf s in
         let buf =
            try format_exn db buf exn; buf with
               exn ->
                  let buf = new_buffer () in
                     format_message buf s;
                     format_string buf "!!! Filter_exn.handle_exn: error during exception printing !!!";
                     format_space buf;
                     format_loc buf loc;
                     format_space buf;
                     format_string buf (Printexc.to_string exn);
                     buf
         in
            format_ezone buf;
            format_popm buf;
            format_newline buf;
            output_rbuffer stderr buf;
            flush stderr;
            exit 2

let print_exn =
   if Exn_boot.backtrace then
      dont_print_exn
   else
      print_exn

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
