(*
 * There are three types of packages.  This is an interactive
 * package that contains interactive proofs, theorem statements, etc.
 * A package has a name, and a magic number.
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

include Package_type

(*
 * The package type.
 *)
type t

(*
 * Constructors.
 *)
val create : unit -> t

(*
 * Operations.
 *)
val add : t -> item -> unit
val rename : t -> string -> unit

(*
 * Listing the package.
 *)
val items_of_package : t -> item list

(*
 * IO operations.
 *)
val save : t -> unit

val restore_tactics : in_channel -> int -> Ast.expr array
val restore : in_channel -> int -> tactic_resources -> tactic array -> t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
