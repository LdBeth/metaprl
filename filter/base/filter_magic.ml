(*
 * This file defines version numbers and magic numbers for some
 * of the MetaPRL file formats.
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

open Lm_pervasives
open File_type_base

(************************************************************************
 * FILE FORMAT VERSIONS SUPPORTED                                       *
 ************************************************************************)
(*
 * The arguments for pack_version are: major version number, minor sub-version number, revision number
 *)

(*
 * Raw data structures revision history
 *
 * Rev 0: whatever was around when the versioning was added
 * Rev 1: added PRL bindings to several summary items (many types have chaged)
 * Rev 2: variables are now implemented as symbols.
 * Rev 3: got rid of MVar parameters
 *)
let raw_versions = List.map (pack_version 1 0) [3]

let term_versions = List.map (pack_version 1 0) [3]

(*
 * ASCII IO format revision history:
 *
 * Rev 0: whatever was around when the versioning was added
 * Rev 1: added new hypothesis syntax (hyps with and w/o bindings)
 * Rev 2: removed support for variable name (string) arguments to rules/rewrites
 * Rev 3: added a real summary item for "define" directives (instead of declare + prim_rw implementation)
 * Rev 4: corrected term representation for prec_rel
 * Rev 5: added PRL bindings to several summary items
 * Rev 6: got rid of MVar parameters
 * Rev 7: worked around of having to split sequents into two ("S" and "G") lines by adding support for the \\ separator
 *
 * Filter_summary has a HACK needed to read some rev 0-1 files
 * Ascii_io has a HACK needed to read some rev 0-5 files
 * Ascii_io has another HACK to read sequents in rev 0-6 files
 *)
let ascii_versions = List.map (pack_version 1 0) [7;6;5;4;3;2;1;0]

(************************************************************************
 * Magic numbers for interactive files                                  *
 ************************************************************************)

(* Previously used somewhere: 0x63ac6be3, 0x63ac6be7, 0x63ac6be9, 0x73ac6be2, 0x73ac6be4, 0x73ac6be5, 0x73ac6be7 *)

let int_term_sig_magic = 0x63ac6be1
let int_term_str_magic = 0x63ac6be3
let int_lib_sig_magic  = 0x63ac6be5
let int_lib_str_magic  = 0x63ac6be6
let int_raw_sig_magic  = 0x63ac6be8
let int_raw_str_magic  = 0x63ac6bea
let interactive_magics =
   [int_term_sig_magic;
    int_raw_sig_magic;
    int_term_str_magic;
    int_raw_str_magic;
    int_lib_sig_magic;
    int_lib_str_magic]

(*
 * See if the file is interactive by checking the magic number.
 *)
let file_interactive file =
   try
      let inx = open_in_bin file in
         try
            let magic = input_binary_int inx in
               close_in inx;
               List.mem magic interactive_magics
         with
            _ ->
               close_in inx;
               false
   with
      Sys_error _ ->
         false

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
