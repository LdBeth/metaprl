(*
 * Formatter like in the standard library.
 * Output is organized into boxes, each of which has an indentation.
 *
 * Commands:
 *    format_sbreak str str': soft break is taken if necessary
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_break str str': hard break is takenin groups
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
 * This file is part of Nuprl-Light, a modular, higher order
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

type buffer

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Buffer creation.
 *)
val new_buffer : unit -> buffer
val clear_buffer : buffer -> unit

(*
 * Breaks.
 *)
val format_sbreak : buffer -> string -> string -> unit
val format_break : buffer -> string -> string -> unit
val format_ibreak : buffer -> string -> string -> unit
val format_space : buffer -> unit
val format_hspace : buffer -> unit
val format_newline : buffer -> unit

(*
 * Break zones.
 *)
val format_lzone : buffer -> unit
val format_szone : buffer -> unit
val format_hzone : buffer -> unit
val format_ezone : buffer -> unit

(*
 * MArgins.
 *)
val format_pushm : buffer -> int -> unit
val format_popm : buffer -> unit

(*
 * Printers.
 *)
val format_char : buffer -> char -> unit
val format_string : buffer -> string -> unit
val format_quoted_string : buffer -> string -> unit
val format_int : buffer -> int -> unit
val format_num : buffer -> Nl_num.num -> unit
val format_buffer : buffer -> buffer -> unit

(*
 * Collecting output.
 *)
val print_to_channel : int -> buffer -> out_channel -> unit
val print_to_string : int -> buffer -> string

(*
 * Debug variables.
 *)
val debug_simple_print : bool ref
val debug_dform : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
