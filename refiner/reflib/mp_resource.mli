(*
 * Resource management.
 * Each resource provides four operations:
 *    1. Create a new, empty resource
 *    2. Join two resource providers
 *    3. Extract a value from the resource
 *    4. Add a value to the resource
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

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Resources are saved when they are labeled.
 *)
type ('info, 'result, 'data) t

(*
 * these are the methods for modifying a resource.
 *)
type ('info, 'result, 'data) info =
   { resource_join : 'data -> 'data -> 'data;
     resource_extract : 'data -> 'result;
     resource_improve : 'data -> 'info -> 'data;
     resource_close : 'data -> string -> 'data
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Create a resource, passing the primitive methods.
 *)
val create : ('info, 'result, 'data) info -> 'data -> ('info, 'result, 'data) t

(*
 * Update a resource.
 *)
val join : ('info, 'result, 'data) t -> ('info, 'result, 'data) t -> ('info, 'result, 'data) t
val extract : ('info, 'result, 'data) t -> 'result
val improve : ('info, 'result, 'data) t -> 'info -> ('info, 'result, 'data) t
val close : ('info, 'result, 'data) t -> string -> ('info, 'result, 'data) t
val wrap : ('info, 'result, 'data) t -> ('data -> 'data) -> ('info, 'result, 'data) t

(*
 * Add a list of improvements.
 *)
val improve_list : ('info, 'result, 'data) t -> 'info list -> ('info, 'result, 'data) t

(*
 * Get a resource by name.
 *)
val find : ('info, 'result, 'data) t -> string -> ('info, 'result, 'data) t

(*
 * Debugging.
 *)
val debug_resource : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
