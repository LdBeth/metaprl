(*
 * Extra terminal commands.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_printf
open Unix

let debug_terminal =
   create_debug {
      debug_name = "terminal";
      debug_description = "show terminal size operations";
      debug_value = false
   }

external term_size : file_descr -> int * int = "caml_term_size"

let min_screen_width = ref 40

let term_width out width =
   try
      let _, cols = term_size (descr_of_out_channel out) in
         if !debug_terminal then
            eprintf "Terminal size: requested %i, got %i, minimal witdth is %i%t" width cols (!min_screen_width) eflush;
         max (!min_screen_width) cols
   with
      Failure s ->
         if !debug_terminal then
            eprintf "Can't get terminal size: %s%t" s eflush;
         width

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
