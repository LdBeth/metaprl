(*
 * Exception printing.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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

open Rformat

open Proof_boot.Proof
open Tactic_boot.TacticInternal

(*
 * Flag is true if we don't want to catch exceptions.
 *)
let backtrace =
   try String.contains (Sys.getenv "OCAMLRUNPARAM") 'b' with
      Not_found ->
         false

module TacticExn =
struct
   (*
    * Convert an exception to a string.
    *)
   let rec format_exn db buf exn =
      let format = function
         ExtRefineError (name, proof, ref_error) ->
            format_szone buf;
            format_pushm buf 4;
            format_string buf "ProofError:";
            format_hspace buf;
            Refine_exn.format_refine_error db buf name ref_error;
            format_newline buf;
            format_extract db buf proof;
            format_popm buf;
            format_ezone buf

       | ProofRefineError (name, proof, ref_error) ->
            format_szone buf;
            format_pushm buf 4;
            format_string buf "ProofError:";
            format_hspace buf;
            Refine_exn.format_refine_error db buf name ref_error;
            format_newline buf;
            format_proof db buf proof;
            format_popm buf;
            format_ezone buf
       | exn ->
            Filter_exn.format_exn db buf exn
      in
         format exn

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
      if backtrace then
         f x
      else
         try f x with
            exn ->
               print_exn db stderr exn
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
