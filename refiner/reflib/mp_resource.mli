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

open Refiner.Refiner.TermType
open Refiner.Refiner.TermMeta

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Resources are saved when they are labeled.
 *)
type ('info, 'result, 'data, 'arg) t

(*
 * these are the methods for modifying a resource.
 *)
type ('info, 'result, 'data, 'arg) info = {
   resource_empty : 'data;
   resource_join : 'data -> 'data -> 'data;
   resource_extract : 'data -> 'result;
   resource_improve : 'data -> 'info -> 'data;
   resource_improve_arg :
      'data ->
      string ->               (* Name of the new resource *)
      string array ->         (* Names of the context vars *)
      string array ->         (* Names of the new variables *)
      term list ->            (* Arguments *)
      term list ->            (* Parameters *)
      meta_term ->            (* Rule statement *)
      'arg ->                 (* Extra arguments *)
      'data;
   resource_close : 'data -> string -> 'data
}

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Create a resource, passing the primitive methods.
 *)
val create : ('info, 'result, 'data, 'arg) info -> ('info, 'result, 'data, 'arg) t

(*
 * Update a resource.
 *)
val join : ('info, 'result, 'data, 'arg) t -> ('info, 'result, 'data, 'arg) t -> ('info, 'result, 'data, 'arg) t
val extract : ('info, 'result, 'data, 'arg) t -> string -> 'result
val extract_top : ('info, 'result, 'data, 'arg) t -> 'result
val improve : ('info, 'result, 'data, 'arg) t -> 'info -> ('info, 'result, 'data, 'arg) t
val improve_list : ('info, 'result, 'data, 'arg) t -> 'info list -> ('info, 'result, 'data, 'arg) t
val improve_arg : ('info, 'result, 'data, 'arg) t ->
   string ->
   string array ->
   string array ->
   term list ->
   term list ->
   meta_term ->
   'arg ->
   ('info, 'result, 'data, 'arg) t
val label : ('info, 'result, 'data, 'arg) t -> string -> ('info, 'result, 'data, 'arg) t
val close : ('info, 'result, 'data, 'arg) t -> string -> ('info, 'result, 'data, 'arg) t
val wrap : ('info, 'result, 'data, 'arg) t -> ('data -> 'data) -> ('info, 'result, 'data, 'arg) t

(*
 * Add a list of improvements.
 *)
val improve_list : ('info, 'result, 'data, 'arg) t -> 'info list -> ('info, 'result, 'data, 'arg) t

(*
 * Get the module by name.
 *)
val find : ('info, 'result, 'data, 'arg) t -> string -> ('info, 'result, 'data, 'arg) t

(*
 * This function is a utility to fail on improvement by arguments.
 *)
val improve_arg_fail : string -> 'a -> string -> string array -> string array -> term list -> term list -> meta_term -> 'b -> 'c

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
