(*
 * Copy HTML files, with variable replacement.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol

open Browser_sig

(*
 * Lookup table for HTML translation.
 *)
module BrowserTable : BrowserTableSig

(*
 * Some common keys.
 *)
val title_sym       : symbol
val buttons_sym     : symbol
val location_sym    : symbol
val body_sym        : symbol
val host_sym        : symbol
val port_sym        : symbol
val challenge_sym   : symbol
val response_sym    : symbol
val message_sym     : symbol
val style_sym       : symbol
val history_sym     : symbol
val menu_sym        : symbol
val session_sym     : symbol
val menu_macros_sym : symbol
val buttons_macros_sym : symbol

(*
 * Translate a file.
 *)
val print_raw_file_to_http           : out_channel -> string -> unit
val print_translated_file_to_http    : out_channel -> BrowserTable.t -> string -> unit
val print_translated_file_to_channel : out_channel -> BrowserTable.t -> string -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
