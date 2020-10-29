(*
 * This is a null implementation of the
 * shared queue for testing a single process.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
open Lm_debug

open Lm_printf
open Lm_thread
open Lm_thread_util

let debug_queue =
   create_debug (**)
      { debug_name = "queue";
        debug_description = "Show remote queue operations";
        debug_value = false
      }

module Queue =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * A handle is just a queue value.
    *)
   type ('a, 'b) handle = 'a

   (*
    * A lock contains the owning handle,
    *)
   type ('a, 'b) lock = 'a

   (*
    * Since this queue is only local, key sharing is trivial.
    *)
   type 'c key =
      { key_value : 'c;
        mutable key_local : 'c
      }

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
    * The queue contains:
    *    1. a list of unlocked entries
    *    2. a list of locked entries
    *)
   type ('a, 'b, 'c) t =
      { queue_chan : ('a, 'b) upcall Lm_thread_event.channel;
        queue_lock : Mutex.t;
        queue_cond : Condition.t;
        mutable queue_unlocked : ('a, 'b) handle list;
        mutable queue_locked : ('a, 'b) lock list;
        mutable queue_upcalls : ('a, 'b) upcall list
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Place a message in the upcall queue.
    *)
   let send_upcall queue upcall =
      queue.queue_upcalls <- upcall :: queue.queue_upcalls;
      Condition.signal queue.queue_cond

   (*
    * Deliver all the upcall messages.
    * The queue_lock must be unlocked in this
    * function.
    *)
   let issue_upcalls queue upcalls =
      let issue upcall =
         if !debug_queue || true then
            begin
               lock_printer ();
               eprintf "Remote_queue_null.issue_upcall: begin%t" eflush;
               unlock_printer ()
            end;
         Lm_thread_event.sync 0 (Lm_thread_event.send queue.queue_chan upcall);
         if !debug_queue || true then
            begin
               lock_printer ();
               eprintf "Remote_queue_null.issue_upcall: end%t" eflush;
               unlock_printer ()
            end
      in
         List.iter issue upcalls

   (*
    * Get an event for receiving upcalls.
    *)
   let event_of_queue queue =
      Lm_thread_event.receive queue.queue_chan

   (*
    * Submit a value to the queue.
    *)
   let add queue x =
      Mutex.lock queue.queue_lock;
      queue.queue_unlocked <- x :: queue.queue_unlocked;
      Mutex.unlock queue.queue_lock;
      x

   (*
    * Get the value associated with a handle.
    *)
   let arg_of_handle x =
      x

   (*
    * Delete a value in the queue.
    * May issue an upcall.
    *)
   let delete queue lock =
      Mutex.lock queue.queue_lock;
      try
         queue.queue_unlocked <- Lm_list_util.removeq lock queue.queue_unlocked;
         Mutex.unlock queue.queue_lock;
      with
         Not_found ->
            queue.queue_locked <- Lm_list_util.removeq lock queue.queue_locked;
            send_upcall queue (UpcallCancel lock);
            Mutex.unlock queue.queue_lock

   (*
    * Lock the first unlocked value.
    * Don't worry if there are no unlocked values--
    * we'll never send an upcall.
    *)
   let lock queue =
      Mutex.lock queue.queue_lock;
      if !debug_queue || true then
         begin
            lock_printer ();
            eprintf "Remote_queue_null.lock: %d/%d%t" (**)
               (List.length queue.queue_unlocked)
               (List.length queue.queue_upcalls)
               eflush;
            unlock_printer ()
         end;
      match queue.queue_unlocked with
         h :: t ->
            lock_printer ();
            eprintf "Remote_queue_null.lock: returned lock%t" eflush;
            unlock_printer ();
            queue.queue_locked <- h :: queue.queue_locked;
            queue.queue_unlocked <- t;
            send_upcall queue (UpcallLock h);
            Mutex.unlock queue.queue_lock
       | [] ->
            Mutex.unlock queue.queue_lock

   let arg_of_lock lock =
      lock

   (*
    * Cancel a pending lock.
    *)
   let cancel queue lock =
      Mutex.lock queue.queue_lock;
      queue.queue_locked <- Lm_list_util.removeq lock queue.queue_locked;
      queue.queue_unlocked <- lock :: queue.queue_unlocked;
      Mutex.unlock queue.queue_lock

   (*
    * Unlock and return a value to the owner.
    *)
   let unlock queue lock x =
      Mutex.lock queue.queue_lock;
      queue.queue_locked <- Lm_list_util.removeq lock queue.queue_locked;
      send_upcall queue (UpcallResult (lock, x));
      Mutex.unlock queue.queue_lock

   (*
    * Add a key to the queue.
    *)
   let share _queue _ x =
      { key_value = x;
        key_local = x
      }

   let share_local _ key x =
      key.key_local <- x

   let arg_of_key _ key =
      key.key_local

   (*
    * Main loop waits for upcall events to be triggered.
    *)
   let thread_main_loop queue =
      while true do
         Mutex.lock queue.queue_lock;
         if queue.queue_upcalls = [] then
            Condition.wait queue.queue_cond queue.queue_lock;
         let upcalls = List.rev queue.queue_upcalls in
            queue.queue_upcalls <- [];
            Mutex.unlock queue.queue_lock;
            issue_upcalls queue upcalls
      done

   (*
    * Create an empty queue.
    *)
   let create _upcalls =
      { queue_chan = Lm_thread_event.new_channel ();
        queue_lock = Mutex.create "Remote_queue_null";
        queue_cond = Condition.create ();
        queue_unlocked = [];
        queue_locked = [];
        queue_upcalls = []
      }

   (*
    * Nothing needed in main loop.
    *)
   let args () =
      []

   let main_loop queue =
      let _ = Thread.create thread_main_loop queue in
         ()
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
