(*
 * This is the raw abstraction layered over Ensemble.
 * We implemented a shared queue abstraction.  The
 * supported operations are:
 *    1. Add a value to the queue.
 *       The issuing process becomes the owner
 *       of the entry.  the entry is initially
 *       unlocked.
 *    2. Delete a value in the queue.
 *       The issuing process must be the owner.
 *       A notification is sent to any process
 *       that has locked the entry, and the
 *       lock is removed.
 *    3. Lock a value in the queue.
 *       This value can belong to any process,
 *       and the owning process may cancel it
 *       by failing or actively deleting the entry.
 *    4. Cancel a lock being held.
 *    5. Send a value to the owner of an entry.
 *       The entry in the queue is removed along
 *       with its lock when the value is passed
 *       passed to the owner. This will silently
 *       fail if the owner has failed.
 *    6. Query the length of the queue.
 *       This is an approximate value.
 *
 * Ensemble may also issue upcalls when the status
 * of the queue changes.  This upcalls occurs in a
 * separate threead, so synchronization should be enforced.
 *    1. Cancel a lock.
 *       This happens when the owning process dies,
 *       or the owning process actively deletes the
 *       entry.  No response is necessary.
 *    2. Return a value for an entry that was
 *       submitted.  The entry an its lock are
 *       deleted just before the upcall.
 *    3. Deliver a lock that was requested (UpcallLock).
 *    4. Deliver a lock before locking has
 *       been fully negotiated.  This lock may
 *       be followed by an UpcallCancel if the
 *       negotiation fails.
 *    5. Notify of a view event.  This just restarts
 *       the upcall scheduler.
 *
 * In addition, we include a system for shared memory.
 * The client can broadcast an entry, which becomes available to
 * everyone in the group.  These entries are saved using weak
 * pointers.  They are removed once all references disappear.
 * Shared entries may or may not reflect mutations in
 * copies of the object.  However, the following guarantee
 * is made: all copies are either equal or mutations
 * in one copy do not affect the other copies.
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
module type RemoteQueueSig =
sig
   (*
    * This is the type of Ensemble queues.
    *    'a: The type of the queue entry
    *    'b: The type of return values
    *    'c: The type of shared memory entries
    *)
   type ('a, 'b, 'c) t

   (*
    * Each queue entry in the local process has
    * a handle that refers to it.
    *)
   type ('a, 'b) handle

   (*
    * This is the type of lock on queue entries.
    *)
   type ('a, 'b) lock

   (*
    * This is a key for shared memory values.
    *)
   type 'c key

   (*
    * Upcalls.
    *)
   type ('a, 'b) upcall =
      UpcallCancel of ('a, 'b) lock
    | UpcallResult of ('a, 'b) handle * 'b
    | UpcallLock of ('a, 'b) lock
    | UpcallPreLock of ('a, 'b) lock
    | UpcallView

   (*
    * Connect to the shared Ensemble queue.
    * The flag is for quick-locking.
    *)
   val create : bool -> ('a, 'b, 'c) t

   (*
    * Event for receiving upcalls.
    *)
   val event_of_queue : ('a, 'b, 'c) t -> ('a, 'b) upcall Thread_event.event

   (*
    * Submit a new queue entry.
    *)
   val add : ('a, 'b, 'c) t -> 'a -> ('a, 'b) handle

   (*
    * Delete an entry in the queue.
    * This will issue a notification to
    * any process that holds a lock.
    *)
   val delete : ('a, 'b, 'c) t -> ('a, 'b) handle -> unit

   (*
    * Find a free entry in the queue an lock it.
    * This may take some time for negotiation.
    * The lock will be issued in an upcall.
    *)
   val lock : ('a, 'b, 'c) t -> unit

   (*
    * Return the value associated with the lock.
    *)
   val arg_of_lock : ('a, 'b) lock -> 'a

   (*
    * Cancel an outstanding lock.
    * The entry is just pushed back onto the queue.
    *)
   val cancel : ('a, 'b, 'c) t -> ('a, 'b) lock -> unit

   (*
    * Atomically send a value to the owner of the entry,
    * and unlock and delete the entry from the queue.
    *)
   val unlock : ('a, 'b, 'c) t -> ('a, 'b) lock -> 'b -> unit

   (*
    * Add a shared-memory entry.
    * The string is for debugging.
    *)
   val share : ('a, 'b, 'c) t -> string -> 'c -> 'c key

   (*
    * Set a local value for the share.
    *)
   val share_local : ('a, 'b, 'c) t -> 'c key -> 'c -> unit

   (*
    * Get the value associated with a key.
    *)
   val arg_of_key : ('a, 'b, 'c) t -> 'c key -> 'c

   (*
    * Start the main loop.
    *)
   val args : unit -> (string * Arg.spec * string) list
   val main_loop : ('a, 'b, 'c) t -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
