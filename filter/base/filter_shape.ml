(*
 * Opname shape classes are represented with bit fields.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
open Lm_num
open Lm_printf

(*
 * Opname declaration modifiers.
 * We represent the modifiers as a bit field.
 *)
(* %%MAGICBEGIN%% *)
type shape_class = int

let shape_normal = 0
let shape_iform  = 1
let shape_const  = 2
(* %%MAGICEND%% *)

let shape_combine = (lor)

let is_shape_normal sc =
   sc land shape_iform = 0

let is_shape_iform sc =
   sc land shape_iform <> 0

let is_shape_const sc =
   sc land shape_const <> 0

(*
 * Term conversion.
 *)
let shape_class_of_num = int_of_num
let num_of_shape_class = num_of_int

let rec pp_print_string_list buf sl =
   match sl with
      [s] ->
         pp_print_string buf s
    | s :: sl ->
         pp_print_string buf s;
         pp_print_string buf " ";
         pp_print_string_list buf sl
    | [] ->
         ()

let pp_print_shape_class buf sc =
   if sc = shape_normal then
      pp_print_string buf "normal"
   else
      let sl =
         if is_shape_iform sc then
            ["iform"]
         else
            []
      in
      let sl =
         if is_shape_const sc then
            "const" :: sl
         else
            sl
      in
         pp_print_string_list buf sl

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
