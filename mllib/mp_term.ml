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
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)

type winsize =
   NoSize
 | SomeSize of int * int

external c_term_size : unit -> winsize = "caml_term_size"

let term_size () =
   match c_term_size () with
      NoSize ->
         raise (Failure "Mp_term.term_size: standard input is not a terminal")
    | SomeSize (rows, cols) ->
         rows, cols

let term_width width =
   match c_term_size () with
      NoSize ->
         width
    | SomeSize (_, cols) ->
         max cols width

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
