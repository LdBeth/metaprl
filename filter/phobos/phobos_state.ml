(*
 * Phobos global state variables.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
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
 * Author: Adam Granicz
 * @email{granicz@cs.caltech.edu}
 * @end[license]
 *)

open Mp_debug

(*
 * Default grammar and options.
 *)
let mp_desc_grammar_filename = ref "m_ir.pho"
let mp_grammar_filename = ref "m_ast.pho"

let debug_grammar = ref true
let save_grammar = ref true

let grammar_filename = ref ""
let compiled_grammar_filename = ref ""
let use_fc_ast = ref false
let apply_no_rewrites = ref false
let phobos_paths = ref [""]

(*
 * Phobos debugging.
 *)

let debug_phobos =
   create_debug (**)
      { debug_name = "phobos";
        debug_description = "print debugging information for Phobos";
        debug_value = false
      }
