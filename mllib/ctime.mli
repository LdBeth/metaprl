(*
 * Time functions.
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

(*
 * Blown out representation.
 *)
type localtime =
   { time_sec : int;
     time_min : int;
     time_hour : int;
     time_mday : int;
     time_mon : int;
     time_year : int;
     time_zone : int
   }

(*
 * Convert a string to a float.
 *)
val localtime : int -> localtime
val mktime : localtime -> int
val parse_time : string -> int

(*
 * Convert to a string.
 *)
val ctime : int -> string
val simpletime : int -> string

(*
 * Month names (starting from 0).
 *)
val name_of_month : int -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
