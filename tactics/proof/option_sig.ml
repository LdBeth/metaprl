(*
 * Option types.
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
open Opname
open Refiner.Refiner.RefineError

(*
 * Flags associated with options.
 *)
type option_info =
   OptionAllow
 | OptionExclude
 | OptionIgnore

type option_table =
   (opname * option_info) list

let string_of_option = function
   OptionAllow -> "allow"
 | OptionExclude -> "exclude"
 | OptionIgnore -> "ignore"

let option_of_string = function
   "allow" -> OptionAllow
 | "exclude" -> OptionExclude
 | "ignore" -> OptionIgnore
 | s -> raise (RefineError ("option_of_string", StringError (Printf.sprintf "illegal option string '%s': legal values are 'allow', 'exclude', 'ignore'" s)))

let is_option_string = function
   "allow"
 | "exclude"
 | "ignore" ->
      true
 | _ ->
      false

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
