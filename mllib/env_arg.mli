(*
 * Environment/command line option interface to arguments.
 * Taken from ensemble/appl/cdarg.ml
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
 *)

(*
 * Type of variable setting functions.
 * Args:
 *     1. the name of the option
 *     2. reference to the old value
 *     3. the new value
 * The function should put the new value into the old cell.
 * This may require parsing the new value.
 *
 * env_set: set the option from a string
 * arg_set: set the option from the command line
 * var_set: set the option from a value
 *)
type 'a env_set = string -> 'a ref -> string -> unit
type 'a arg_set = string -> 'a ref -> Arg.spec
type ('a, 'b) var_set = string -> 'a ref -> 'b -> unit

(*
 * Add an argument:
 *    1. name
 *    2. default value
 *    3. documentation string
 *    4. function to call when set as env variable
 *    5. function to call as command line option
 * Returns reference to the value.
 *)
val general : string -> 'a -> string -> 'a env_set -> 'a arg_set -> 'a ref

(*
 * Special cases.
 *)
val string : string -> 'a -> string -> ('a, string) var_set -> 'a ref
val int : string -> 'a -> string -> ('a, int) var_set -> 'a ref
val bool : string -> bool -> string -> (bool, bool) var_set -> bool ref

(*
 * Standard variable setting functions.
 *)
val set_string_string : (string, string) var_set
val set_string_option_string : (string option, string) var_set
val set_int_int : (int, int) var_set
val set_int_option_int : (int option, int) var_set
val set_bool_bool : (bool, bool) var_set

(*
 * Return the args to be added to the command prompt.
 *)
val args : unit -> (string * Arg.spec * string) list

(*
 * Print a usage message.
 *)
val usage : (string * Arg.spec * string) list -> string -> unit

(*
 * Our parser saves the spec list.
 *)
val parse : (string * Arg.spec * string) list -> (string -> unit) -> string -> unit

(*
 * Library directory
 *)
val lib : string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
