(*
 * Build a lazy queue from a queue.
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

open Remote_queue_sig

module Make (Queue : RemoteQueueSig) =
struct
   (*
    * Shared values are computed lazily.
    *)
   type 'c lazy_value =
      LazyFunction of (unit -> 'c)
    | LazyValue of 'c

   (*
    * Shared values.
    *)
   type 'c key = 'c lazy_value Queue.key

   (*
    * These parts correspond directly to the Queue.
    *)
   type ('a, 'b, 'c) t = ('a, 'b, 'c lazy_value) Queue.t
   type ('a, 'b) handle = ('a, 'b) Queue.handle
   type ('a, 'b) lock = ('a, 'b) Queue.lock
   type ('a, 'b) upcall = ('a, 'b) Queue.upcall =
      UpcallCancel of ('a, 'b) lock
    | UpcallResult of ('a, 'b) handle * 'b
    | UpcallLock of ('a, 'b) lock
    | UpcallPreLock of ('a, 'b) lock
    | UpcallView

   let create = Queue.create
   let event_of_queue = Queue.event_of_queue
   let add = Queue.add
   let arg_of_handle = Queue.arg_of_handle
   let delete = Queue.delete
   let lock = Queue.lock
   let arg_of_lock = Queue.arg_of_lock
   let cancel = Queue.cancel
   let unlock = Queue.unlock
   let args = Queue.args
   let main_loop = Queue.main_loop

   (*
    * Wrap the shared value in a ref.
    * Evaluate the function early to catch errors.
    *)
   let share queue debug f =
      let v = f () in
      let key = Queue.share queue debug (LazyFunction f) in
         Queue.share_local queue key (LazyValue v);
         key

   (*
    * Dereference and evaluate the lazy value.
    *)
   let arg_of_key queue key =
      match Queue.arg_of_key queue key with
         LazyValue v ->
            v
       | LazyFunction f ->
            let v = f () in
               Queue.share_local queue key (LazyValue v);
               v
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
