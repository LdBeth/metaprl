(*
 * Our implementation of big_int.
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
 *)

type big_int

val big_int_of_int : int -> big_int

(*
 * Operations.
 *)
val add_big_int : big_int -> big_int -> big_int
val sub_big_int : big_int -> big_int -> big_int
val mult_big_int : big_int -> big_int -> big_int
val div_big_int : big_int -> big_int -> big_int
val quo_big_int : big_int -> big_int -> big_int
val mod_big_int : big_int -> big_int -> big_int
val rem_big_int : big_int -> big_int -> big_int

val abs_big_int : big_int -> big_int

(*
 * Comparisons.
 *)
val eq_big_int : big_int -> big_int -> bool
val compare_big_int : big_int -> big_int -> int

(*
 * Conversion.
 *)
val is_integer_big_int : big_int -> bool
val integer_big_int : big_int -> int

val string_of_big_int : big_int -> string
val big_int_of_string : string -> big_int

(*
 * Special cases.
 *)
val div10 : big_int -> int * big_int
val mult10 : big_int -> big_int
val is_zero_big_int : big_int -> bool

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
