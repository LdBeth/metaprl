(*
 * Wrap a remote queue so we can print out its entries.
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
 *
 *)

open Lm_printf
open Lm_thread_util

open Remote_sig

module MakeMonitor (Remote : RemoteSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Things that may happen to an entry.
    *)
   type remote_info =
      Submitted
    | Canceled
    | Locked
    | LockCanceled
    | LockReleased
    | LockReturned
    | Returned

   (*
    * Wrapped types.
    *)
   type ('a, 'b) handle = (int * 'a, 'b) Remote.handle
   type ('a, 'b) local = (int * 'a, 'b) Remote.local
   type 'c key = 'c Remote.key
   type 'a event = 'a Remote.event

   (*
    * These are the possible responses to
    * a job.  The RemoteCanceled may be returned
    * if the job was canceled, but it is not required.
    * If the job was not canceled, the Cancel event is
    * never returned.
    *)
   type 'b response = 'b Remote.response =
      RemoteCanceled
    | RemoteSuccess of 'b

   (*
    * This wrapper just keeps a list of remote entries.
    * index: counts the entries,
    * count: counts the operation number
    *)
   type ('a, 'b, 'c) t =
      { mutable remote_info : (int * remote_info) list array;
        mutable remote_index : int;
        mutable remote_count : int;
        mutable remote_remote : (int * 'a, 'b, 'c) Remote.t
      }

   (************************************************************************
    * PRINTING                                                             *
    ************************************************************************)

   (*
    * Print the info.
    *)
   let print_events queue =
      let print_event (count, event) =
         let s =
            match event with
               Submitted ->
                  "Submitted"
             | Canceled ->
                  "Canceled"
             | Locked ->
                  "Locked"
             | LockCanceled ->
                  "LockCanceled"
             | LockReleased ->
                  "LockReleased"
             | LockReturned ->
                  "LockReturned"
             | Returned ->
                  "Returned"
         in
            eprintf "\t%s: %d\n" s count
      in
      let rec print_events = function
         h :: t ->
            print_events t;
            print_event h
       | [] ->
            ()
      in
      let info = queue.remote_info in
      let length = Array.length info in
      let rec print i =
         match info.(i) with
            [] ->
               ()
          | events ->
               eprintf "%d:" i;
               print_events events;
               print (succ i)
      in
         lock_printer ();
         print 0;
         flush stderr;
         unlock_printer ()

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Create a new empty queue.
    *)
   let create () =
      { remote_info = Array.create 128 [];
        remote_index = 0;
        remote_count = 0;
        remote_remote = Remote.create ()
      }

   (*
    * Add an event.
    *)
   let push_event queue index event =
      let { remote_info = info; remote_count = count } = queue in
         info.(index) <- (count, event) :: info.(index);
         queue.remote_count <- succ count;
         print_events queue

   (*
    * Submit a new job.  Just create a handle and queue the job.
    *)
   let submit queue arg =
      let { remote_info = info;
            remote_index = index;
            remote_remote = remote
          } = queue
      in
      let info =
         if index = Array.length info then
            let info' = Array.create (2 * index) [] in
               Array.blit info 0 info' 0 index;
               queue.remote_info <- info';
               info'
         else
            info
      in
         push_event queue index Submitted;
         queue.remote_index <- succ index;
         Remote.submit remote (index, arg)

   (*
    * Get the value associated with a handle.
    *)
   let arg_of_handle hand =
      snd (Remote.arg_of_handle hand)

   (*
    * Get the receive event for the handle.
    *)
   let event_of_handle queue hand =
      let wrapper arg =
         let index, _ = Remote.arg_of_handle hand in
            push_event queue index Returned;
            arg
      in
      let remote = queue.remote_remote in
      let event = Remote.event_of_handle remote hand in
         Remote.wrap event wrapper

   (*
    * Cancel a submitted event.
    * If this handle is being served by a local job,
    * cancel the local job.
    *)
   let cancel_handle queue hand =
      let remote = queue.remote_remote in
      let index, _ = Remote.arg_of_handle hand in
         push_event queue index Canceled;
         Remote.cancel_handle remote hand

   (*
    * When polled, the request event will try to pop a pending
    * job for local service.
    *)
   let request queue =
      let wrapper local =
         let index, _ = Remote.arg_of_local local in
            push_event queue index Locked;
            local
      in
         Remote.wrap (Remote.request queue.remote_remote) wrapper

   (*
    * Get the argument for the local event.
    *)
   let arg_of_local local =
      snd (Remote.arg_of_local local)

   (*
    * Poll the local event.
    *)
   let event_of_local queue local =
      let wrapper () =
         let index, _ = Remote.arg_of_local local in
            push_event queue index LockCanceled
      in
         Remote.wrap (Remote.event_of_local queue.remote_remote local) wrapper

   (*
    * Cancel a local job.
    * This may restart the handle.
    *)
   let cancel_local queue local =
      let index, _ = Remote.arg_of_local local in
         push_event queue index LockReleased;
         Remote.cancel_local queue.remote_remote local

   (*
    * Return a value for the local job.
    *)
   let return_local queue local x =
      let index, _ = Remote.arg_of_local local in
         push_event queue index LockReturned;
         Remote.return_local queue.remote_remote local x

   (************************************************************************
    * SHARED MEMORY                                                        *
    ************************************************************************)

   (*
    * Trivial shares.
    *)
   let share queue debug f =
      Remote.share queue.remote_remote debug f

   let arg_of_key queue x =
      Remote.arg_of_key queue.remote_remote x

   (************************************************************************
    * SCHEDULING                                                           *
    ************************************************************************)

   (*
    * Scheduling is the same
    *)
   let wrap = Remote.wrap
   let wrap_event = Remote.wrap_event
   let select queue events =
      Remote.select queue.remote_remote events

   let args = Remote.args
   let main_loop queue =
      Remote.main_loop queue.remote_remote
end


(*
 * -*-
 * Local Variables:
 * Caml-master: "mp.run"
 * End:
 * -*-
 *)
