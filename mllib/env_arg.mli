(*
 * Environment/command line option interface to arguments.
 * Taken from ensemble/appl/cdarg.ml
 *
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
 *)
type 'a env_set = string -> 'a ref -> string -> unit
type 'a arg_set = string -> 'a ref -> Arg.spec
type 'a var_set = string -> 'a ref -> 'a -> unit

(*
 * Add an argument:
 *    1. name
 *    2. default value
 *    3. documentation string
 *    4. function to call when set as env variable
 *    5. function to call as command line option
 * Returns reference to variable value.
 *)
val general : string -> 'a -> string -> 'a env_set -> 'a arg_set -> 'a ref

(*
 * Special cases.
 *)
val string : string -> string -> string -> string var_set -> string ref
val int : string -> int -> string -> int var_set -> int ref
val bool : string -> bool -> string -> bool var_set -> bool ref

(*
 * Return the args to be added to the command prompt.
 *)
val args : unit -> (string * Arg.spec * string) list

(*
 * Our parser saves the spec list.
 *)
val parse : (string * Arg.spec * string) list -> (string -> unit) -> string -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
