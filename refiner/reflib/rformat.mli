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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type buffer

(*
 * The BufferOverflow exception is raised when too much visible text is
 * put in the buffer.  You can control how much visible text is allowed
 * by using the format_bound function below.
 *)
exception BufferOverflow

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Buffer creation.
 *)
val new_buffer : unit -> buffer
val clear_buffer : buffer -> unit

(*
 * Specify the max number of characters in the buffer.
 * This will not raise an exception even if the buffer
 * is already too large.  You will get the exception
 * the next time you insert visible text.
 *)
val format_bound : buffer -> int -> unit

(*
 * Breaks.
 *)
val format_cbreak : buffer -> string -> string -> unit
val format_sbreak : buffer -> string -> string -> unit
val format_hbreak : buffer -> string -> string -> unit
val format_space : buffer -> unit
val format_hspace : buffer -> unit
val format_newline : buffer -> unit

(*
 * Break zones.
 *)
val zone_depth   : buffer -> int
val format_lzone : buffer -> unit
val format_szone : buffer -> unit
val format_hzone : buffer -> unit
val format_ezone : buffer -> unit
val format_izone : buffer -> unit

(* TeX boxes *)
val format_tzone : buffer -> string -> unit

(*
 * Margins.
 *)
val format_pushm : buffer -> int -> unit
val format_pushm_str : buffer -> string -> unit
val format_popm : buffer -> unit

(*
 * Printers.
 *)
val format_char : buffer -> char -> unit
val format_string : buffer -> string -> unit
val format_raw_string : buffer -> string -> unit
val format_quoted_string : buffer -> string -> unit
val format_int : buffer -> int -> unit
val format_num : buffer -> Mp_num.num -> unit

(*
 * Collecting output.
 *)
val default_width : int (* 80 *)
val print_to_channel : int -> buffer -> out_channel -> unit
val print_to_string : int -> buffer -> string
val print_to_html : int -> buffer -> out_channel -> unit
val print_to_tex : int -> buffer -> out_channel -> unit

(*
 * Special case: 1-line buffer.
 *)
val line_format : int -> (buffer -> unit) -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
