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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_rformat

open Proof_boot.Proof
open Tactic_boot.TacticInternal

let format_exn db buf = function
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
 | ProofRefineError (name, proof, address, ref_error) ->
      format_szone buf;
      format_pushm buf 4;
      format_string buf "ProofError:";
      format_hspace buf;
      Refine_exn.format_refine_error db buf name ref_error;
      format_newline buf;
      format_proof db buf (index proof address);
      format_popm buf;
      format_ezone buf
 | Unix.Unix_error (errno, funinfo, arginfo) ->
      format_string buf "UnixError:";
      format_hspace buf;
      format_string buf (Unix.error_message errno);
      format_hspace buf;
      format_string buf ": ";
      format_string buf funinfo;
      if arginfo <> "" then
         begin
            format_hspace buf;
            format_string buf "(";
            format_string buf arginfo;
            format_string buf ")"
         end
 | exn ->
      Refine_exn.format_exn db buf exn

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
