(*
 * This is a DAG used for cycle detection.
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
 * Exception for reporting cycles.
 *)
exception Cycle

(*
 * Nodes are labelled with 'node,
 * and edges have label 'edge.
 *
 * 'node should be an equality type.
 *)
type ('node, 'edge) t

(*
 * Create an empty DAG.
 *)
val create : unit -> ('node, 'edge) t

(*
 * Make it from a previous subst.
 *)
val make : ('node list * ('edge * 'node list) option) list -> ('node, 'edge) t

(*
 * Find an edge.
 *)
val find : ('node, 'edge) t -> 'node -> 'edge

(*
 * Equate two nodes.
 * This raises Cycle if the two nodes are already
 * related by an edge.
 *)
val equate : ('node, 'edge) t -> 'node -> 'node -> unit

(*
 * Insert an edge-list from the source
 * to a list of sinks, with the given label.
 * This may raise the Cycle exception.
 *)
val insert : ('node, 'edge) t -> 'node -> 'edge -> 'node list -> unit

(*
 * Sort the edges, and return them in a list.
 *)
val sort : ('node, 'edge) t -> ('node list * 'edge option) list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
