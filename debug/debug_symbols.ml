(*
 * Load the symbol file, and pass the result to the
 * marshaler.
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
 *
 *)

open Printf

(*
 * This tells the marshaler about the symbols.
 *)
external extern_symbols : (int * string * int) array -> unit = "ml_extern_symbols"

let print_symbol (pos, modname, cpos) =
   eprintf "0x%08x %s/%d\n" pos modname cpos

(*
 * Load the symbol file.
 *)
let debug_symbols file =
   let file = file ^ ".symbols" in
      try
         let inx = open_in_bin file in
         let symbols = (input_value inx : (int * string * int) array) in
            close_in inx;
            extern_symbols symbols
      with
         Sys_error _ ->
            eprintf "Debug_symbols: file %s not found\n" file;
            flush stderr

(*
 * -*-
 * Local Variables:
 * Caml-master: "mp.run"
 * End:
 * -*-
 *)
