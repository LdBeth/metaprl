(*
 * Formatter like in the standard library.
 * Output is organized into boxes, each of which has an indentation.
 *
 * Commands:
 *    format_sbreak str str': soft break is taken if necessary
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_hbreak str str': hard breaks are taken in groups
 *        if taken, str is printed
 *        if not, str' is printed
 *
 *    format_lzone: begin a zone with no breaks
 *    format_szone: soft break zone (all or no hard breaks are taken)
 *    format_hzone: all hard breaks are taken.
 *    format_ezone: end the current zone.
 *
 *    format_pushm i: push left margin from here by i more spaces
 *    format_popm: pop last pushm
 *
 *    format_char: add a single char
 *    format_int: print a number
 *    format_string: add a string to the buffer
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type 'tag buffer

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Buffer creation.
 *)
val new_buffer : unit -> 'tag buffer
val clear_buffer : 'tag buffer -> unit

(*
 * Breaks.
 *)
val format_sbreak : 'tag buffer -> string -> string -> unit
val format_hbreak : 'tag buffer -> string -> string -> unit
val format_space : 'tag buffer -> unit
val format_hspace : 'tag buffer -> unit
val format_newline : 'tag buffer -> unit

(*
 * Break zones.
 *)
val format_lzone : 'tag buffer -> unit
val format_szone : 'tag buffer -> unit
val format_hzone : 'tag buffer -> unit
val format_ezone : 'tag buffer -> unit
val format_izone : 'tag buffer -> unit
val format_tzone : 'tag buffer -> 'tag -> unit

(*
 * Margins.
 *)
val format_pushm : 'tag buffer -> int -> unit
val format_popm : 'tag buffer -> unit

(*
 * Printers.
 *)
val format_char : 'tag buffer -> char -> unit
val format_string : 'tag buffer -> string -> unit
val format_quoted_string : 'tag buffer -> string -> unit
val format_int : 'tag buffer -> int -> unit
val format_num : 'tag buffer -> Mp_num.num -> unit

(*
 * Collecting output.
 *)
val print_to_channel : int -> 'tag buffer -> out_channel -> unit
val print_to_string : int -> 'tag buffer -> string
val print_to_html : int -> 'tag buffer -> out_channel -> (int * 'tag) list
val print_to_tex : int -> 'tag buffer -> out_channel -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
