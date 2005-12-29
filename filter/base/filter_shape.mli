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
 * Declarations have multiple classes.
 * We represent them as a bit field.
 *)
type shape_class

val shape_normal    : shape_class
val shape_iform     : shape_class
val shape_const     : shape_class

val shape_combine   : shape_class -> shape_class -> shape_class

val is_shape_normal : shape_class -> bool
val is_shape_iform  : shape_class -> bool
val is_shape_const  : shape_class -> bool

val shape_class_of_num : num -> shape_class
val num_of_shape_class : shape_class -> num

val pp_print_shape_class : out_channel -> shape_class -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
