(*
 * Parsing state.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2001 Adam Granicz, Caltech
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
 * Email: granicz@cs.caltech.edu
 *)

open Phobos_type

let tabstop = 8

(*
 * File position information.
 *)
let position = ref ("<null>", 1, 0, 1, 0)

let set_current_position pos =
   position := pos

let current_position () =
   !position

let current_file () =
   let name, _, _, _, _ = !position in
      name

let string_of_pos (name, i1, i2, i3, i4) =
   Printf.sprintf "file \"%s\" pos %d:%d-%d:%d" name i1 i2 i3 i4

let print_pos pos =
   Printf.printf "%s" (string_of_pos pos)

(*
 * Generic parser position.
 *
let phobos_position = ref ("", 0, 0, 0, 0)

let phobos_current_position () = !phobos_position

let phobos_current_file () =
   let name, _, _, _, _ = !phobos_position in
      name

let set_phobos_position pos =
   phobos_position := pos
*)
(*
 * Combine two positions.
 *)
let union_pos
    (file1, sline1, schar1, eline1, echar1)
    (file2, sline2, schar2, eline2, echar2) =
   if file1 <> file2 then
      raise (Invalid_argument (**)
                (Printf.sprintf "union_pos: file mistmatch: \"%s\":\"%s\"" (**)
                    (String.escaped file1) (String.escaped file2)));
   let sline, schar =
      if sline1 < sline2 then
         sline1, schar1
      else if sline1 > sline2 then
         sline2, schar2
      else
         sline1, min schar1 schar2
   in
   let eline, echar =
      if eline1 > eline2 then
         eline1, echar1
      else if eline1 < eline2 then
         eline2, echar2
      else
         eline1, max echar1 echar2
   in
      file1, sline, schar, eline, echar
