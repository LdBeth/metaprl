(*
 * Add Phobos extensions to MetaPRL syntax.
 *
 * The grammar of MetaPRL is extended to include Phobos commands.
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
 * Copyright (C) 2003 Adam Granicz, Caltech
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
 * Author: Adam Granicz <granicz@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Lm_debug
open Filter_util
open Filter_type
open Phobos_compile

(*
 * Show loading of the file.
 *)
let _ =
   show_loading "Loading Filter_phobos%t"

let debug_filter_phobos =
   create_debug (**)
      { debug_name = "filter_phobos";
        debug_description = "parse external sources";
        debug_value = false
      }

(************************************************************************
 * QUOTATIONS                                                           *
 ************************************************************************)

(*
 * External sources.
 *)
let ext_exp s =
   let grammar_filename =
      try
         Sys.getenv "LANG_FILE"
      with
         Not_found ->
            !Phobos_state.mp_grammar_filename
   in
   let t = Phobos_exn.catch (term_of_string [] grammar_filename) s in
      add_binding (BindTerm t)

let ext_patt s =
   let loc = dummy_loc in
      <:patt<$str:s$>>

let _ = Quotation.add "ext" (Quotation.ExAst (ext_exp, ext_patt))

(*
 * Descriptions.
 *)
let desc_exp s =
   let grammar_filename =
      try
         Sys.getenv "DESC_LANG_FILE"
      with
         Not_found ->
            !Phobos_state.mp_desc_grammar_filename
   in
   let t = Phobos_exn.catch (term_of_string [] grammar_filename) s in
      add_binding (BindTerm t)

let desc_patt s =
   let loc = dummy_loc in
      <:patt<$str:s$>>

let _ = Quotation.add "desc" (Quotation.ExAst (desc_exp, desc_patt))

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
