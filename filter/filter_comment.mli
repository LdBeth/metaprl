(*
 * This module is used to provide an association between
 * terms in an ML file, and their comments.
 *
 * The algorithm is best match.  We parse the comments from
 * the file, the iterate through all the terms in the
 * program.  The closest, largest program block after the comment
 * is associated with the comment through a table.
 * The comments can then be queried through the table.
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
 * Type of comment associations.
 *)
type t

(*
 * Read the comments from a file.
 * The argument is the name of the file,
 * and the result is the list of comments with
 * their offset into the file.
 *
 * Raises Sys_error if the file does not exist.
 *)
val parse : string -> (int * string) list

(*
 * Create an association.
 *)
val create_sig : (int * string) list -> MLast.sig_item list -> t
val create_str : (int * string) list -> MLast.str_item list -> t

(*
 * Query the association.
 * Raises Not_found
 *)
val get : t -> MLast.loc -> int * string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
