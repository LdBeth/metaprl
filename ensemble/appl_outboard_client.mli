(*
 * Define an application interface for Ensemble.  This differs
 * a little from Ensemble, because messages can be queued at
 * any time (so there is no hearbeat).  Also, the local state
 * and view state are just reduced.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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
 * Identifier for an endpoint.
 *)
type id

type view_state = id list

(*
 * Possible messages.
 *)
type ('cast, 'send) message =
   Cast of 'cast
 | Send of id * 'send

type ('cast, 'send) message_data =
   CastData of 'cast
 | SendData of 'send

(*
 * Ensemble info.
 *)
type ('cast, 'send) t

(*
 * View handlers are functions that are used for upcalls
 * per view.
 *)
type ('cast, 'send) view_handlers =
   { block : ('cast, 'send) t -> ('cast, 'send) message list;
     receive : ('cast, 'send) t -> id -> ('cast, 'send) message_data -> ('cast, 'send) message list
   }

(*
 * Application handlers are specified once at creation time.
 *)
type ('cast, 'send) appl_handlers =
   { install : ('cast, 'send) t -> ('cast, 'send) message list * ('cast, 'send) view_handlers }

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Arguments of the program.
 *)
val args : unit -> (string * Arg.spec * string) list

(*
 * Start Ensemble.
 *)
val main_loop : unit -> unit

(*
 * Create an Ensemble application.
 * We need:
 *    1. an endpoint name,
 *    2. an application name
 *    3. an application handler record
 *)
val create : string -> string -> (('cast, 'send) t -> 'a * ('cast, 'send) appl_handlers) -> 'a

(*
 * Send a message in the current view.
 * If the "block" handler has been called, these messages
 * are postponed until the next view.
 *)
val send : ('cast, 'send) t -> ('cast, 'send) message -> unit

(*
 * View info.
 *)
val endpt : ('cast, 'send) t -> id
val view : ('cast, 'send) t -> id list

(*
 * String form of id for debugging.
 *)
val string_of_id : id -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
