(*
 * A Lazy queue is just like a Queue (as defined
 * in Remote_queue_sig), except that shared memory values
 * are computed lazily.  The shared values are communicated
 * as functions, but the return values are fully evaluated.
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
module type RemoteLazyQueueSig =
sig
   (*
    * These parts correspond directly to the Queue.
    *)
   type ('a, 'b, 'c) t
   type ('a, 'b) handle
   type ('a, 'b) lock
   type 'c key
   type ('a, 'b) upcall =
      UpcallCancel of ('a, 'b) lock
    | UpcallResult of ('a, 'b) handle * 'b
    | UpcallLock of ('a, 'b) lock
    | UpcallPreLock of ('a, 'b) lock
    | UpcallView

   val create : bool -> ('a, 'b, 'c) t
   val event_of_queue : ('a, 'b, 'c) t -> ('a, 'b) upcall Thread_event.event
   val add : ('a, 'b, 'c) t -> 'a -> ('a, 'b) handle
   val delete : ('a, 'b, 'c) t -> ('a, 'b) handle -> unit
   val lock : ('a, 'b, 'c) t -> unit
   val arg_of_lock : ('a, 'b) lock -> 'a
   val cancel : ('a, 'b, 'c) t -> ('a, 'b) lock -> unit
   val unlock : ('a, 'b, 'c) t -> ('a, 'b) lock -> 'b -> unit
   val args : unit -> (string * Arg.spec * string) list
   val main_loop : ('a, 'b, 'c) t -> unit

   (*
    * Install a lazy value.
    *)
   val share : ('a, 'b, 'c) t -> string -> (unit -> 'c) -> 'c key

   (*
    * Get the value associated with a key.
    *)
   val arg_of_key : ('a, 'b, 'c) t -> 'c key -> 'c
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
