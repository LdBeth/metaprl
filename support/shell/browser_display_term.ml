(*
 * This is the standard interface to the window system.
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
open Printf
open Lm_debug

(*
 * The display buffer is global.
 *)
let buffer = Buffer.create 1024
let message = Buffer.create 1024

(*
 * Reset the buffer in case nothing happened.
 *)
let reset () =
   Buffer.clear message;
   Buffer.clear buffer;
   Buffer.add_string buffer "The editor did not display a term"

(*
 * Set the rule text.
 *)
let set_message text =
   Buffer.clear message;
   Buffer.add_string message text

(*
 * Display a term in the window.
 *)
let set_main width buf =
   Buffer.clear buffer;
   Rformat.print_html_buffer width buf buffer

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
