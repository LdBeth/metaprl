(*
 * Interface to ensemble.
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
open Lm_thread_util

open Lm_printf

(*
 * Debug variables.
 *)
let debug_ensemble =
   create_debug (**)
      { debug_name = "ensemble";
        debug_description = "Display MP Ensemble Application actions";
        debug_value = false
      }

let debug_share =
   create_debug (**)
      { debug_name = "share";
        debug_description = "Display MP Ensemble Application memory sharing";
        debug_value = false
      }

let debug_marshal =
   create_debug (**)
      { debug_name = "ensemble";
        debug_description = "Display MP Ensemble Application actions";
        debug_value = false
      }

let debug_message =
   create_debug (**)
      { debug_name = "message";
        debug_description = "Display Ensemble messages";
        debug_value = false
      }

module Queue =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Each entry is identified by a handle and
    * a number.
    *)
   type id =
      { id_id : Appl_outboard_client.id;
        id_number : int
      }

   (*
    * Entries in the queue have:
    *    entry_id: id of the owner of the entry
    *    entry_value: value of the entry
    *
    * A remote entry also keeps the lock endpt,
    * so that the entry can be unlocked if the
    * locking process fails.
    *)
   type 'a entry =
      { entry_id : id;
        mutable entry_value : 'a option
      }

   type 'a remote_entry =
      { remote_entry : 'a entry;
        mutable remote_lock : Appl_outboard_client.id
      }

   type ('a, 'b) handle = 'a entry
   type ('a, 'b) lock = 'a entry

   (*
    * Shared keys.  Make sure this is a separate cell,
    * so that the garbage collector can collect it.
    *)
   type 'c key =
      { key_id : Appl_outboard_client.id;
        key_index : int
      }

   type 'c key_value =
      { keyv_id : Appl_outboard_client.id;
        keyv_index : int;
        keyv_value : 'c;
        mutable keyv_local : 'c
      }

   type 'c key_info =
      { keyi_id : Appl_outboard_client.id;
        keyi_index : int;
        keyi_value : 'c
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
    * These are the message types we use over Ensemble.
    *)
   type ('a, 'b, 'c) message =
      CastLock of id
    | CastLocalLock of id
    | CastUnlock of id
    | CastEntry of int
    | CastDelete of int
    | CastResult of id
    | CastNewShare of int * 'c
    | CastDeleteShare of int
    | CastQueue of 'a entry list * 'a entry list * 'a remote_entry list * 'c key_info list
    | SendEntry of int * 'a
    | SendResult of id * 'b
    | SendQueue of 'a entry list * 'a entry list * 'a remote_entry list * 'c key_info list

   (*
    * This is the info we keep about the local queue.
    *)
   type ('a, 'b, 'c) t =
      { mp_lock : Mutex.t;

        (*
         * Queues:
         *    unlocked: entries that have not been locked
         *    local: entries that have been locked by the local client
         *    pending: entries owned locally for which a result is pending
         *    remote: entries locked by a remote process
         *    index: the number of the last entry we locked
         *)
        mutable mp_unlocked : 'a entry list;
        mutable mp_local : 'a entry list;
        mutable mp_pending : 'a entry list;
        mutable mp_remote : 'a remote_entry list;
        mutable mp_index : int;

        (*
         * Shared memory.
         * Local keys are kept as weak pointers, so that we can
         * collect the entries.  We assume that when the key owner
         * drops the key no other process will want to use it.
         * The key numbers are the numbers associated with
         * the keys, so we can delete the entries event after the
         * key is dropped.
         *)
        mutable mp_keys : 'c key Weak.t;
        mutable mp_key_index : int;
        mutable mp_key_numbers : int array;
        mutable mp_values : 'c key_value list;

        (*
         * Client info:
         *   pending_lock: the id of the unlocked event we want to lock
         *   upcalls: a list of messages to be sent as upcalls (in reverse order)
         *   upcall_chan: the channel for communicating the upcall
         *)
        mutable mp_pending_lock : ('a, 'b) lock option;
        mutable mp_upcalls : ('a, 'b) upcall list;
        mp_upcall_chan : ('a, 'b) upcall Lm_thread_event.channel;

        (*
         * Server info:
         *    queue: list of messages to be sent to Ensemble on the heartbeat
         *    new_view: a flag indicating that a new view is in progress
         *    rank_flags: a flag indicating which ranks have sent their stack
         *       to the coordinator after a view change.
         *)
        mutable mp_new_view : bool;
        mutable mp_rank_flags : bool array;

        (*
         * Ensemble info:
         *    mp_ensemble: handle to Ensemble interface
         *)
        mutable mp_ensemble : (('a, 'b, 'c) message, ('a, 'b, 'c) message) Appl_outboard_client.t
      }

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Printing.
    *)
   let print_entry_list out entries =
      let print { entry_id = { id_id = id; id_number = number } } =
         fprintf out " %s:%d" (Appl_outboard_client.string_of_id id) number
      in
         List.iter print entries

   let print_remote_list out entries =
      let print { remote_entry = { entry_id = { id_id = id; id_number = number } } } =
         fprintf out " %s:%d" (Appl_outboard_client.string_of_id id) number
      in
         List.iter print entries

   let print_share_list out values =
      let print { keyv_id = id; keyv_index = number } =
         fprintf out " %s:%d" (Appl_outboard_client.string_of_id id) number
      in
         List.iter print values

   let print_message out = function
      CastLock { id_id = id; id_number = number } ->
         eprintf "Lock (%s, %d)" (Appl_outboard_client.string_of_id id) number
    | CastLocalLock { id_id = id; id_number = number } ->
         eprintf "LocalLock (%s, %d)" (Appl_outboard_client.string_of_id id) number
    | CastUnlock { id_id = id; id_number = number } ->
         eprintf "Unlock (%s, %d)" (Appl_outboard_client.string_of_id id) number
    | CastEntry id ->
         eprintf "CastEntry %d" id
    | CastDelete id ->
         eprintf "Delete %d" id
    | CastResult { id_id = id; id_number = number } ->
         eprintf "CastResult (%s, %d)" (Appl_outboard_client.string_of_id id) number
    | CastNewShare (id, _) ->
         eprintf "Share %d" id
    | CastDeleteShare id ->
         eprintf "DeleteShare %d" id
    | CastQueue _ ->
         eprintf "Queue"
    | SendEntry (number, _) ->
         eprintf "SendEntry %d" number
    | SendResult ({ id_id = id; id_number = number }, _) ->
         eprintf "SendResult (%s, %d)" (Appl_outboard_client.string_of_id id) number
    | SendQueue _ ->
         eprintf "SendQueue"

   (*
    * Find and remove and entry based on its id.
    *)
   let rec mem_entry id = function
      entry :: entries ->
         entry.entry_id = id || mem_entry id entries
    | [] ->
         false

   let rec mem_remote id = function
      remote :: remotes ->
         remote.remote_entry.entry_id = id || mem_remote id remotes
    | [] ->
         false

   let rec mem_share id number = function
      { keyv_id = id'; keyv_index = number' } :: t ->
         (id' = id && number' = number) || mem_share id number t
    | [] ->
         false

   (*
    * Check all the possible places for the entry.
    *)
   let entry_exists info id =
      (mem_entry id info.mp_unlocked) ||
      (mem_entry id info.mp_local) ||
      (mem_entry id info.mp_pending) ||
      (mem_remote id info.mp_remote)

   (*
    * Remove entries from their queues.
    *)
   let rec find_entry id = function
      entry :: entries ->
         if entry.entry_id = id then
            entry
         else
            find_entry id entries
    | [] ->
         raise Not_found

   let rec remove_entry id = function
      entry :: entries ->
         if entry.entry_id = id then
            entry, entries
         else
            let entry', entries = remove_entry id entries in
               entry', entry :: entries
    | [] ->
         raise Not_found

   let rec remove_entry_id id = function
      entry :: entries ->
         if entry.entry_id.id_id = id then
            entry, entries
         else
            let entry', entries = remove_entry_id id entries in
               entry', entry :: entries
    | [] ->
         raise Not_found

   let rec remove_remote id = function
      remote :: remotes ->
         if remote.remote_entry.entry_id = id then
            remote, remotes
         else
            let remote', remotes = remove_remote id remotes in
               remote', remote :: remotes
    | [] ->
         raise Not_found

   (************************************************************************
    * ENSEMBLE INTERFACE                                                   *
    ************************************************************************)

   (*
    * Issue the upcalls to the client.
    * The state should be unlocked before this function
    * is called.
    *)
   let issue_upcalls info upcalls =
      let issue upcall =
         if !debug_ensemble then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.issue_upcall%t" eflush;
               unlock_printer ()
            end;
         Lm_thread_event.sync 0 (Lm_thread_event.send info.mp_upcall_chan upcall)
      in
         if upcalls <> [] then
            begin
               if !debug_ensemble then
                  begin
                     lock_printer ();
                     eprintf "Ensemble_queue.issue_upcalls: start%t" eflush;
                     unlock_printer ()
                  end;
               List.iter issue upcalls;
               if !debug_ensemble then
                  begin
                     lock_printer ();
                     eprintf "Ensemble_queue.issue_upcalls: issued%t" eflush;
                     unlock_printer ()
                  end
            end

   let send_upcall info upcall =
      info.mp_upcalls <- upcall :: info.mp_upcalls

   let send_message info debug message =
      if !debug_message then
         begin
            lock_printer ();
            begin
               match message with
                  Appl_outboard_client.Cast msg ->
                     eprintf "Ensemble_queue.send_message: Cast: %a%t" (**)
                        print_message msg eflush
                | Appl_outboard_client.Send (id, msg) ->
                     eprintf "Ensemble_queue.Send %s: %a%t" (**)
                        (Appl_outboard_client.string_of_id id)
                        print_message msg eflush
            end;
            unlock_printer ()
         end;
      Appl_outboard_client.send info.mp_ensemble message

   (*
    * Add a new entry to the unlocked queue.
    * Check that we have no prior knowledge of then entry--
    * This would occur if the entry was added locally.
    *)
   let handle_new_entry_notify info srcid number =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_new_entry_notify: %d%t" number eflush;
            unlock_printer ()
         end;
      let id = { id_id = srcid; id_number = number } in
         if not (srcid = Appl_outboard_client.endpt info.mp_ensemble || entry_exists info id) then
            let entry = { entry_id = id; entry_value = None } in
               send_upcall info UpcallView;
               info.mp_unlocked <- entry :: info.mp_unlocked

   (*
    * Once a lock is granted, the entry value is updated.
    * This is a response to a lock request, so if the
    * lock already moved the entry to the local queue,
    * pass the lock to the application.  Otherwise,
    * this response arrived before the lock, so just
    * update the value of the entry.
    *)
   let handle_new_entry info srcid number x =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_new_entry: %d%t" number eflush;
            unlock_printer ()
         end;
      let id = { id_id = srcid; id_number = number } in
         try
            (* Lock arrived, so grant the lock *)
            let entry = find_entry id info.mp_local in
               entry.entry_value <- Some x;
               send_upcall info (UpcallLock entry)
         with
            Not_found ->
               try
                  (* Lock response hasn't arrived yet *)
                  let entry = find_entry id info.mp_unlocked in
                     entry.entry_value <- Some x
               with
                  Not_found ->
                     (* Entry has been canceled *)
                     ()

   (*
    * When a lock is granted to a remote process, move it
    * to the mp_remote queue. If the entry is owned locally,
    * send the entry value to the remote process.
    *)
   let remote_lock info srcid id =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.remote_lock: %s:%d%t" (**)
               (Appl_outboard_client.string_of_id id.id_id) id.id_number eflush;
            unlock_printer ()
         end;
      try
         let entry, entries = remove_entry id info.mp_unlocked in
         let remote =
            { remote_entry = entry;
              remote_lock = srcid
            }
         in
            info.mp_unlocked <- entries;
            info.mp_remote <- remote :: info.mp_remote;

            (*
             * If
             * This should mean that the entry is local.
             *)
            if id.id_id = Appl_outboard_client.endpt info.mp_ensemble then
               match entry.entry_value with
                  Some x ->
                     send_message info "remote_lock" (**)
                        (Appl_outboard_client.Send (srcid, (SendEntry (id.id_number, x))))
             | None ->
                  eprintf "Ensemble_queue.remote_lock: no value for the local entry%t" eflush
      with
         Not_found ->
            ()

   (*
    * When we are granted the lock, create an entry in
    * mp_local.  If the entry has a value, that means the
    * entry is either local, or a SendEntry has already
    * been received for it, so grant the lock request.
    * Otherwise, wait for the value to be sent.
    *)
   let install_lock info id =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.install_lock: %s:%d%t" (**)
               (Appl_outboard_client.string_of_id id.id_id) id.id_number eflush;
            unlock_printer ()
         end;
      try
         let entry, entries = remove_entry id info.mp_unlocked in
            info.mp_unlocked <- entries;
            info.mp_local <- entry :: info.mp_local;
            info.mp_pending_lock <- None;
            match entry.entry_value with
               Some _ ->
                  send_upcall info (UpcallLock entry)
             | None ->
                  send_upcall info UpcallView
      with
         Not_found ->
            info.mp_pending_lock <- None

   (*
    * When another process gets the lock we requested,
    * cancel our request, and grant the remote lock.
    * Wake up the application, so it will try another
    * lock request.
    *)
   let cancel_lock info srcid entry =
      let id = entry.entry_id in
         if !debug_ensemble then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.cancel_lock: %s:%d%t" (**)
                  (Appl_outboard_client.string_of_id id.id_id) id.id_number eflush;
               unlock_printer ()
            end;
         remote_lock info srcid id;
         info.mp_pending_lock <- None;
         send_upcall info UpcallView

   (*
    * When a lock arrives, it can be in three cases.
    *   1. The lock is granted for a local request
    *   2. The lock for the local request is granted to a remote process
    *   3. The lock is unrelated to any request we have made
    *)
   let handle_lock info srcid id =
      match info.mp_pending_lock with
         Some entry ->
            if entry.entry_id = id then
               if srcid = Appl_outboard_client.endpt info.mp_ensemble then
                  (* We got the lock *)
                  install_lock info id
               else
                  (* Someone else got the lock before us *)
                  cancel_lock info srcid entry
            else
               (* Some other entry *)
               remote_lock info srcid id
       | None ->
            remote_lock info srcid id

   (*
    * An entry was stolen by its owner.
    * If the entry is unlocked, grant the lock to its owner.
    * If the entry is local, waiting for the entry,
    * cancel the lock.
    * If the entry was granted to a remote process,
    * modify the remote entry.
    *)
   let revoke_remote_lock info srcid id =
      try
         let entry, entries = remove_remote id info.mp_remote in
            entry.remote_lock <- srcid
      with
         Not_found ->
            ()

   let revoke_local_lock info srcid id =
      try
         let entry, entries = remove_entry id info.mp_local in
            info.mp_unlocked <- entry :: info.mp_unlocked;
            info.mp_local <- entries;
            info.mp_pending_lock <- None;
            send_upcall info UpcallView
      with
         Not_found ->
            ()

   let handle_local_lock info srcid id =
      if not (srcid = Appl_outboard_client.endpt info.mp_ensemble) then
         begin
            revoke_remote_lock info srcid id;
            revoke_local_lock info srcid id;
            remote_lock info srcid id
         end

   (*
    * A previously locked entry is now unlocked.
    * If the entry was locked remotely, move it to
    * mp_unlocked queue.  If the entry was local,
    * we have already moved it, so just delete it
    * from the mp_local queue.
    *)
   let unlock_remote info id =
      try
         let { remote_entry = entry }, remotes = remove_remote id info.mp_remote in
            info.mp_remote <- remotes;
            info.mp_unlocked <- entry :: info.mp_unlocked;
            true
      with
         Not_found ->
            false

   let unlock_local info id =
      try
         let _, entries = remove_entry id info.mp_local in
            info.mp_local <- entries
      with
         Not_found ->
            ()

   let handle_unlock info srchand id =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_unlock%t" eflush;
            unlock_printer ()
         end;
      if not (unlock_remote info id) then
         unlock_local info id

   (*
    * Handle a result for an entry that was locked remotely.
    * Remove it from the mp_remote queue.  If the entry is owned
    * locally, move it to the mp_pending queue, and wait for
    * the response to be sent.
    *)
   let remote_result_notify info id =
      try
         let { remote_entry = entry }, remotes = remove_remote id info.mp_remote in
            info.mp_remote <- remotes;
            if entry.entry_id.id_id = Appl_outboard_client.endpt info.mp_ensemble then
               begin
                  if !debug_ensemble then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.remote_result: %d%t" (**)
                           entry.entry_id.id_number eflush;
                        unlock_printer ();
                     end;
                  info.mp_pending <- entry :: info.mp_pending
               end;
            true
      with
         Not_found ->
            false

   (*
    * Handle a result for an entry that is locked locally.
    * This means the local lock can be canceled.  This should
    * only happen if multiple locks are granted for the entries.
    * If the entry is owned locally, we move it to the mp_pending
    * queue to wait for the final result.
    *)
   let local_result_notify info id =
      begin
         if !debug_ensemble then
            let print_entry { entry_id = { id_id = id; id_number = number } } =
               eprintf "(%s, %d)" (Appl_outboard_client.string_of_id id) number
            in
               lock_printer ();
               eprintf "Ensemble_queue.local_result: try local %s, %d: " (**)
                  (Appl_outboard_client.string_of_id id.id_id) id.id_number;
               List.iter print_entry info.mp_local;
               eflush stderr;
               unlock_printer ()
      end;
      try
         let entry, entries = remove_entry id info.mp_local in
            info.mp_local <- entries;
            if entry.entry_id.id_id = Appl_outboard_client.endpt info.mp_ensemble then
               begin
                  if !debug_ensemble then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.local_result: %d%t" (**)
                           entry.entry_id.id_number eflush;
                        unlock_printer ();
                     end;
                  info.mp_pending <- entry :: info.mp_pending
               end
      with
         Not_found ->
            ()

   let handle_result_notify info srchand id =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_result%t" eflush;
            unlock_printer ()
         end;
      if not (remote_result_notify info id) then
         local_result_notify info id

   (*
    * Handle a result message for an entry.
    * If the entry is on the mp_remote queue,
    * pass the result to the application.
    *)
   let remote_result info id x =
      try
         let { remote_entry = entry }, remotes = remove_remote id info.mp_remote in
            if !debug_ensemble then
               begin
                  lock_printer ();
                  eprintf "Ensemble_queue.remote_result: %d%t" (**)
                     entry.entry_id.id_number eflush;
                  unlock_printer ();
               end;
            info.mp_remote <- remotes;
            send_upcall info (UpcallResult (entry, x))
      with
         Not_found ->
            ()

   (*
    * Handle a result message for a local entry.
    * This happens only if multiple locks can be
    * granted for entries.  If so, remove the entry from
    * the mp_local queue, and pass the result to the
    * application.
    *)
   let local_result info id x =
      try
         let entry, entries = remove_entry id info.mp_local in
            if !debug_ensemble then
               begin
                  lock_printer ();
                  eprintf "Ensemble_queue.local_result: %d%t" (**)
                     entry.entry_id.id_number eflush;
                  unlock_printer ()
               end;
            info.mp_local <- entries;
            send_upcall info (UpcallResult (entry, x))
      with
         Not_found ->
            ()

   (*
    * Handle a result message for an entry on the mp_pending
    * queue.  This means that the result notification has already
    * arrived.
    *)
   let pending_result info id x =
      try
         let entry, entries = remove_entry id info.mp_pending in
            if !debug_ensemble then
               begin
                  lock_printer ();
                  eprintf "Ensemble_queue.pending_result: %d%t" (**)
                     entry.entry_id.id_number eflush;
                  unlock_printer ()
               end;
            info.mp_pending <- entries;
            send_upcall info (UpcallResult (entry, x))
      with
         Not_found ->
            ()

   (*
    * Handle a SendResult message.
    * It should always be the case that the entry is
    * owned locally, so pass the result to the
    * application.
    *)
   let handle_result info srchand id x =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_result%t" eflush;
            unlock_printer ()
         end;
      remote_result info id x;
      local_result info id x;
      pending_result info id x

   (*
    * The owner of the entry has issued a cancelation.
    * The entry should be removed from the queue,
    * and if there is a local lock, we issue an upcall
    * to cancel the local lock.
    *)
   let delete_remote info id =
      try
         let _, remotes = remove_remote id info.mp_remote in
            info.mp_remote <- remotes;
            true
      with
         Not_found ->
            false

   let delete_local info id =
      try
         let entry, entries = remove_entry id info.mp_local in
            info.mp_local <- entries;
            send_upcall info (UpcallCancel entry);
            true
      with
         Not_found ->
            false

   let delete_unlocked info id =
      try
         let _, entries = remove_entry id info.mp_unlocked in
            info.mp_unlocked <- entries;
            true
      with
         Not_found ->
            false

   let handle_delete info srcid number =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_delete: %d%t" number eflush;
            unlock_printer ()
         end;
      let id = { id_id = srcid; id_number = number } in
      let _ =
         (*
          * If a lock was being reqested, delete it.
          * The next lock request will restart it.
          *)
         match info.mp_pending_lock with
            Some entry ->
               if entry.entry_id = id then
                  info.mp_pending_lock <- None
          | None ->
               ()
      in
      let _ =
         (delete_remote info id) ||
         (delete_local info id) ||
         (delete_unlocked info id)
      in
         ()

   (*
    * Share management.
    *)
   let rec share_exists id number = function
      { keyv_id = id'; keyv_index = number' } :: t ->
         if id' = id && number' = number then
            true
         else
            share_exists id number t
    | [] ->
         false

   let handle_new_share info srcid number x =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue: new_share%t" eflush;
            unlock_printer ()
         end;
      if not (share_exists srcid number info.mp_values) then
         let keyv =
            { keyv_id = srcid;
              keyv_index = number;
              keyv_value = x;
              keyv_local = x
            }
         in
            info.mp_values <- keyv :: info.mp_values

   let handle_delete_share info srcid number =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue: delete_share%t" eflush;
            unlock_printer ()
         end;
      let rec remove = function
         { keyv_id = id; keyv_index = number' } as h :: t ->
            if id = srcid && number' = number then
               t
            else
               h :: remove t
       | [] ->
            []
      in
         info.mp_values <- remove info.mp_values

   (*
    * State locking.
    *
    * During a normal unlock, issue all the upcalls,
    * and return the pending messages to Ensemble.
    *
    * If a view is pending, postpone these messages
    * until the new view is installed.  This will cause the
    * client to starve for a short period of time.
    *)
   let lock_info info =
      Mutex.lock info.mp_lock

   let unlock_info info =
      if info.mp_new_view then
         begin
            Mutex.unlock info.mp_lock;
            []
         end
      else
         let upcalls = List.rev info.mp_upcalls in
            info.mp_upcalls <- [];
            Mutex.unlock info.mp_lock;
            issue_upcalls info upcalls;
            []

   (************************************************************************
    * VIEW CHANGE                                                          *
    ************************************************************************)

   (*
    * If the owner of a remote entry fails, drop the entry.
    * If the lock holder of the entry fails, return the
    * entry to the unlocked queue.
    *)
   let prune_remote info view =
      let rec prune = function
         remote :: remotes ->
            if List.mem remote.remote_entry.entry_id.id_id view then
               if List.mem remote.remote_lock view then
                  remote :: prune remotes
               else
                  let { remote_entry = entry } = remote in
                     info.mp_unlocked <- entry :: info.mp_unlocked;
                     prune remotes
            else
               prune remotes
       | [] ->
            []
      in
         info.mp_remote <- prune info.mp_remote

   (*
    * If the owner of a local entry fails, send a cancelation
    * as an upcall.
    *)
   let prune_local info view =
      let rec prune = function
         local :: locals ->
            if List.mem local.entry_id.id_id view then
               local :: prune locals
            else
               begin
                  send_upcall info (UpcallCancel local);
                  prune locals
               end
       | [] ->
            []
      in
         info.mp_local <- prune info.mp_local

   (*
    * If the owner of an unlocked entry fails,
    * just delete it from the queue.
    *)
   let prune_unlocked info view =
      let rec prune = function
         entry :: entries ->
            if List.mem entry.entry_id.id_id view then
               entry :: prune entries
            else
               prune entries
       | [] ->
            []
      in
         info.mp_unlocked <- prune info.mp_unlocked

   (*
    * Delete the pending lock if the owner fails.
    *)
   let prune_lock info view =
      match info.mp_pending_lock with
         Some entry ->
            if not (List.mem entry.entry_id.id_id view) then
               info.mp_pending_lock <- None
       | None ->
            ()

   (*
    * Prune the shared info.
    *)
   let prune_shares info view =
      let rec prune = function
         { keyv_id = id } as h :: t ->
            if List.mem id view then
               h :: prune t
            else
               prune t
       | [] ->
            []
      in
         info.mp_values <- prune info.mp_values

   (*
    * Merge a list of entries into the main list.
    *)
   let rec merge_entries entries = function
      entry :: entries' ->
         let id = entry.entry_id in
            if mem_entry id entries then
               merge_entries entries entries'
            else
               merge_entries (entry :: entries) entries'
    | [] ->
         entries

   let rec merge_remote remotes = function
      remote :: remotes' ->
         let id = remote.remote_entry.entry_id in
            if mem_remote id remotes then
               merge_remote remotes remotes'
            else
               merge_remote (remote :: remotes) remotes'
    | [] ->
         remotes

   let rec merge_share shares = function
      { keyi_id = id; keyi_index = index; keyi_value = x } :: shares' ->
         if mem_share id index shares then
            merge_share shares shares'
         else
            let keyv =
               { keyv_id = id;
                 keyv_index = index;
                 keyv_value = x;
                 keyv_local = x
               }
            in
               merge_share (keyv :: shares) shares'
    | [] ->
         shares

   let merge_queue info unlocked local remote shares =
      info.mp_unlocked <- merge_entries info.mp_unlocked unlocked;
      info.mp_local <- merge_entries info.mp_local local;
      info.mp_remote <- merge_remote info.mp_remote remote;
      info.mp_values <- merge_share info.mp_values shares

   (*
    * Don't send local values of shares.
    *)
   let strip_share values =
      let strip { keyv_id = id; keyv_index = index; keyv_value = x } =
         { keyi_id = id;
           keyi_index = index;
           keyi_value = x
         }
      in
         List.map strip values

   (*
    * Send the stack if we are not the coordinator.
    * The coordinator has rank 0.
    *)
   let send_stack info =
      let { mp_unlocked = unlocked;
            mp_local = local;
            mp_remote = remote;
            mp_values = values;
            mp_ensemble = ensemble
          } = info
      in
      let coord_id = List.hd (Appl_outboard_client.view ensemble) in
         if !debug_ensemble then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.send_stack: %s%t" (Appl_outboard_client.string_of_id (Appl_outboard_client.endpt info.mp_ensemble)) eflush;
               unlock_printer ()
            end;
         [Appl_outboard_client.Send (coord_id, (SendQueue (unlocked, local, remote, strip_share values)))]

   (*
    * If the rank array is filled, broadcast the new stack.
    * This should be the first queued message to go out.
    *)
   let cast_stack info =
      if Lm_array_util.all_true info.mp_rank_flags then
         let { mp_unlocked = unlocked;
               mp_local = local;
               mp_remote = remote;
               mp_values = values;
               mp_ensemble = ensemble
             } = info
         in
            if !debug_message then
               begin
                  lock_printer ();
                  eprintf "Ensemble_queue.cast_stack%t" eflush;
                  unlock_printer ()
               end;
            (* info.mp_rank_flags <- [||]; *)
            info.mp_new_view <- false;
            Appl_outboard_client.send ensemble (Appl_outboard_client.Cast (CastQueue (unlocked, local, remote, strip_share values)))

   (*
    * Merge the communicated stack with our stack.
    * Once all the entries have been received,
    * broadcast the new stack.
    *)
   let handle_queue_send info srcid unlocked local remote shares =
      let rank = Lm_list_util.find_index srcid (Appl_outboard_client.view info.mp_ensemble) in
         if !debug_message then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.handle_queue_send: %d%t" rank eflush;
               unlock_printer ()
            end;
         info.mp_rank_flags.(rank) <- true;
         merge_queue info unlocked local remote shares;
         cast_stack info

   (*
    * Handle the queue and release the new view.
    *)
   let handle_queue_cast info unlocked local remote shares =
      merge_queue info unlocked local remote shares;
      send_upcall info UpcallView;
      info.mp_new_view <- false;
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.new_view: %s\n" (Appl_outboard_client.string_of_id (Appl_outboard_client.endpt info.mp_ensemble));
            eprintf "\tUnlocked: %a\n" print_entry_list info.mp_unlocked;
            eprintf "\tLocal: %a\n" print_entry_list info.mp_local;
            eprintf "\tRemote: %a\n" print_remote_list info.mp_remote;
            eprintf "\tShares: %a%t" print_share_list info.mp_values eflush;
            unlock_printer ()
         end

   (*
    * Handle the view change.
    *    1. Go through all the queues and cancel entries whose
    *       owner has failed.
    *
    *    2. If the view has more than one member,
    *       send the queue to the coordinator.
    *
    * Its important to prune the remote entries first,
    * because remote entries may move onto the unlocked
    * queue.
    *)
   let handle_view_change info =
      let id = Appl_outboard_client.endpt info.mp_ensemble in
      let view = Appl_outboard_client.view info.mp_ensemble in
      let length = List.length view in
      let rank = Lm_list_util.find_index id view in
         if !debug_message then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.handle_view_change: rank=%d %s\n" (**)
                  rank
                  (Appl_outboard_client.string_of_id id);
               List.iter (fun id ->
                     eprintf "\t%s\n" (Appl_outboard_client.string_of_id id)) (**)
                  view;
               flush stderr;
               unlock_printer ()
            end;
         prune_remote info view;
         prune_local info view;
         prune_unlocked info view;
         prune_shares info view;
         info.mp_unlocked <- info.mp_pending @ info.mp_unlocked;
         info.mp_pending <- [];
         if length > 1 then
            begin
               info.mp_new_view <- true;
               if rank = 0 then
                  let flags = Array.create length false in
                     flags.(0) <- true;
                     info.mp_rank_flags <- flags;
                     []
               else
                  send_stack info
            end
         else
            []

   (************************************************************************
    * ENSEMBLE INTERFACE                                                   *
    ************************************************************************)

   (*
    * Handle a received message.
    *)
   let receive info _ srcid msg =
      lock_info info;
      begin
         let msg =
            match msg with
               Appl_outboard_client.CastData msg ->
                  msg
             | Appl_outboard_client.SendData msg ->
                  msg
         in
            match msg with
               CastEntry number ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastEntry (%s, %d)%t" (**)
                           (Appl_outboard_client.string_of_id srcid) number eflush;
                        unlock_printer ()
                     end;
                  handle_new_entry_notify info srcid number
             | SendEntry (number, x) ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastEntry (%s, %d)%t" (**)
                           (Appl_outboard_client.string_of_id srcid) number eflush;
                        unlock_printer ()
                     end;
                  handle_new_entry info srcid number x
             | CastDelete number ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastDelete (%s, %d)%t" (**)
                           (Appl_outboard_client.string_of_id srcid) number eflush;
                        unlock_printer ()
                     end;
                  handle_delete info srcid number
             | CastLock id ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastLock (%s, (%s, %d))%t" (**)
                           (Appl_outboard_client.string_of_id srcid)
                           (Appl_outboard_client.string_of_id id.id_id)
                           id.id_number eflush;
                        unlock_printer ()
                     end;
                  handle_lock info srcid id
             | CastLocalLock id ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastLocalLock (%s, (%s, %d))%t" (**)
                           (Appl_outboard_client.string_of_id srcid)
                           (Appl_outboard_client.string_of_id id.id_id)
                           id.id_number eflush;
                        unlock_printer ()
                     end;
                  handle_local_lock info srcid id
             | CastUnlock id ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastUnlock (%s, (%s, %d))%t" (**)
                           (Appl_outboard_client.string_of_id srcid)
                           (Appl_outboard_client.string_of_id id.id_id)
                           id.id_number eflush;
                        unlock_printer ()
                     end;
                  handle_unlock info srcid id
             | CastResult id ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastResult (%s, (%s, %d))%t" (**)
                           (Appl_outboard_client.string_of_id srcid)
                           (Appl_outboard_client.string_of_id id.id_id)
                           id.id_number eflush;
                        unlock_printer ()
                     end;
                  handle_result_notify info srcid id
             | SendResult (id, x) ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: SendResult (%s, (%s, %d))%t" (**)
                           (Appl_outboard_client.string_of_id srcid)
                           (Appl_outboard_client.string_of_id id.id_id)
                           id.id_number eflush;
                        unlock_printer ()
                     end;
                  handle_result info srcid id x
             | CastNewShare (number, x) ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastNewShare (%s, %d)%t" (**)
                           (Appl_outboard_client.string_of_id srcid) number eflush;
                        unlock_printer ()
                     end;
                  handle_new_share info srcid number x
             | CastDeleteShare number ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastDeleteShare (%s, %d)%t" (**)
                           (Appl_outboard_client.string_of_id srcid) number eflush;
                        unlock_printer ()
                     end;
                  handle_delete_share info srcid number
             | SendQueue (unlocked, local, remote, shares) ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: SendQueue%t" eflush;
                        unlock_printer ()
                     end;
                  handle_queue_send info srcid unlocked local remote shares
             | CastQueue (unlocked, local, remote, shares) ->
                  if !debug_message then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.recv: CastQueue%t" eflush;
                        unlock_printer ()
                     end;
                  handle_queue_cast info unlocked local remote shares
      end;
      send_upcall info UpcallView;
      unlock_info info

   (*
    * When a view change is signaled, we don't
    * do anything.
    *)
   let block info _ =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.block%t" eflush;
            unlock_printer ()
         end;
      []

   let disable info () =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.disable%t" eflush;
            unlock_printer ()
         end

   let make_handlers info =
      { Appl_outboard_client.block     = block info;
        Appl_outboard_client.receive   = receive info
      }

   let install info _ =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.install%t" eflush;
            unlock_printer ()
         end;
      lock_info info;
      let msgs = handle_view_change info in
      let handlers = make_handlers info in
      let msgs' = unlock_info info in
         List.append msgs msgs', handlers

   (*
    * Create the initial state for the application.
    *)
   let open_nl ensemble =
      { mp_lock = Mutex.create ();

        mp_unlocked = [];
        mp_local = [];
        mp_pending = [];
        mp_remote = [];
        mp_index = 0;

        mp_keys = Weak.create 0;
        mp_key_index = 0;
        mp_key_numbers = [||];
        mp_values = [];

        mp_pending_lock = None;
        mp_upcalls = [];
        mp_upcall_chan = Lm_thread_event.new_channel ();

        mp_new_view = false;
        mp_rank_flags = [||];
        mp_ensemble = ensemble
      }

   (************************************************************************
    * QUEUE IMPLEMENTATION                                                 *
    ************************************************************************)

   (*
    * Return the Ensemble arguments.
    *)
   let args = Appl_outboard_client.args

   (*
    * Startup code.
    * The quick flag if PreLocks are to be delivered.
    *)
   let create quick =
      let create ensemble =
         let info = open_nl ensemble in
            info, { Appl_outboard_client.install = install info }
      in
         Appl_outboard_client.create "MPAPP" "MPSERVER" create

   (*
    * Main loop is just the Appl main loop,
    * but we print exceptions.
    *)
   let main_loop _ =
      Appl_outboard_client.main_loop ()

   (*
    * Queue locking.
    * When the queue is unlocked, request a heartbeat if the
    * send queue is nonempty.
    *)
   let lock_queue info =
      Mutex.lock info.mp_lock

   let unlock_queue info =
      Mutex.unlock info.mp_lock

   (*
    * Get the upcall event queue.
    *)
   let event_of_queue info =
      Lm_thread_event.receive info.mp_upcall_chan

   (*
    * Add a new element to the queue.
    * We perform the action early so we can return a
    * value.  When the broadcast is received locally, we
    * filter it out (see handle_new_entry).
    *)
   let add info x =
      lock_queue info;
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.add%t" eflush;
            unlock_printer ()
         end;
      let index = info.mp_index in
      let id = { id_id = Appl_outboard_client.endpt info.mp_ensemble; id_number = index } in
      let entry = { entry_id = id; entry_value = Some x } in
         info.mp_index <- succ index;
         info.mp_unlocked <- entry :: info.mp_unlocked;
         send_message info "add" (Appl_outboard_client.Cast (CastEntry index));
         unlock_queue info;
         entry

   (*
    * Get the value associated with a handle.
    *)
   let arg_of_handle entry =
      match entry.entry_value with
         Some x ->
            x
       | None ->
            raise (Failure "arg_of_handle")

   (*
    * Delete an entry in the queue.
    * The entry is not actually deleted until the
    * broadcast message comes back.
    *)
   let delete info entry =
      lock_queue info;
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.delete: %d%t" entry.entry_id.id_number eflush;
            unlock_printer ()
         end;
      send_message info "delete" (Appl_outboard_client.Cast (CastDelete entry.entry_id.id_number));
      unlock_queue info

   (*
    * Try an lock an entry.
    * If a lock is already pending, don't do anything.
    * Otherwise, see if we can lock a locally-owned entry.
    *   If so, grant the lock immediately, and mark the
    *   entry so any remaining lock request will fail.
    *
    *   If not, choose a random entry from the unlocked
    *   list and try to get a lock on it.
    *)
   let try_local_lock info unlocked =
      let id = Appl_outboard_client.endpt info.mp_ensemble in
         try
            let entry, entries = remove_entry_id id unlocked in
               (* Found an unlocked local entry *)
               info.mp_local <- entry :: info.mp_local;
               info.mp_unlocked <- entries;
               if entry.entry_value = None then
                  raise (Failure "Lock has no value?");
               send_upcall info (UpcallLock entry);
               send_message info "local_lock" (Appl_outboard_client.Cast (CastLocalLock entry.entry_id));
               true
         with
            Not_found ->
               false

   let remote_lock info unlocked =
      let entry = List.nth unlocked (Random.int (List.length unlocked)) in
      let id = entry.entry_id in
         if !debug_ensemble then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.lock: attempting lock on %s:%d%t" (**)
                  (Appl_outboard_client.string_of_id id.id_id) id.id_number eflush;
               unlock_printer ()
            end;
         info.mp_pending_lock <- Some entry;
         send_message info "remote_lock" (Appl_outboard_client.Cast (CastLock id))

   let lock info =
      lock_queue info;
      begin
         let { mp_pending_lock = pending;
               mp_unlocked = unlocked
             } = info
         in
            if !debug_ensemble then
               begin
                  let print_entry { entry_id = { id_id = id; id_number = number } } =
                     eprintf " (%s, %d)" (Appl_outboard_client.string_of_id id) number
                  in
                     lock_printer ();
                     eprintf "Ensemble_queue.lock:";
                     List.iter print_entry unlocked;
                     eflush stderr;
                     unlock_printer ()
               end;
            if pending = None && unlocked <> [] then
               (if not (try_local_lock info unlocked) then
                   remote_lock info unlocked)
            else if !debug_ensemble then
               if pending <> None then
                  begin
                     lock_printer ();
                     eprintf "Ensemble_queue.lock: lock already pending%t" eflush;
                     unlock_printer ()
                  end
               else if unlocked = [] then
                  begin
                     lock_printer ();
                     eprintf "Ensemble_queue.lock: no unlocked entries%t" eflush;
                     unlock_printer ()
                  end
      end;
      unlock_queue info

   (*
    * Get the argument for a lock.
    *)
   let arg_of_lock = arg_of_handle

   (*
    * Cancel an outstanding lock.
    * The entry is actually unlocked only when the broadcast is received.
    *)
   let cancel info lock =
      lock_queue info;
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.cancel%t" eflush;
            unlock_printer ()
         end;
      send_message info "unlock" (Appl_outboard_client.Cast (CastUnlock lock.entry_id));
      unlock_queue info

   (*
    * Unlock the entry, and send the value to the owner.
    * If the lock was just pending, we can safely ignore the
    * lock result, but the entry must be removed from the
    * unlocked queue so we don't try to lock it again.
    *
    * The entry _must_ be on the unlocked queue, because the
    * lock would have been removed if the entry was deleted
    * or locked elsewhere.
    *)
   let unlock info lock x =
      lock_queue info;
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.unlock%t" eflush;
            unlock_printer ()
         end;
      begin
         match info.mp_pending_lock with
            Some entry ->
               if entry.entry_id = lock.entry_id then
                  let _, entries = remove_entry entry.entry_id info.mp_unlocked in
                     (*
                      * Remove this entry from the lock and also
                      * the unblocked queue.
                      *)
                     info.mp_pending_lock <- None;
                     info.mp_unlocked <- entries
          | None ->
               ()
      end;
      send_message info "result" (Appl_outboard_client.Cast (CastResult lock.entry_id));
      begin
         (* Optimize local delivery *)
         let id = lock.entry_id.id_id in
            if id = Appl_outboard_client.endpt info.mp_ensemble then
               local_result info lock.entry_id x
            else
               send_message info "unlock" (Appl_outboard_client.Send (id, SendResult (lock.entry_id, x)))
      end;
      unlock_queue info

   (************************************************************************
    * SHARING                                                              *
    ************************************************************************)

   (*
    * Find a share by its value.
    *)
   let find_share info x =
      let numbers = info.mp_key_numbers in
      let length = Array.length numbers in
      let rec find_key id number i =
         if i = length then
            raise Not_found
         else if numbers.(i) = number then
            match Weak.get info.mp_keys i with
               Some key ->
                  key
             | None ->
                  raise Not_found
         else
            find_key id number (succ i)
      in
      let rec search = function
         { keyv_id = id; keyv_index = number; keyv_value = x' } :: t ->
            if x' == x then
               find_key id number 0
            else
               search t
       | [] ->
            raise Not_found
      in
         search info.mp_values

   (*
    * Remove all old keys from the key list.
    * Return the number of a free entry.
    *)
   let cleanup_shares info =
      let id = Appl_outboard_client.endpt info.mp_ensemble in
      let weak = info.mp_keys in
      let length = Weak.length weak in
      let rec remove number = function
         { keyv_id = id'; keyv_index =  number' } as h :: t ->
            if id' = id && number' = number then
               begin
                  send_message info "delete_share" (Appl_outboard_client.Cast (CastDeleteShare number));
                  t
               end
            else
               h :: remove number t
       | [] ->
            []
      in
      let rec cleanup i j =
         if i <> length then
            match Weak.get weak i with
               Some _ ->
                  cleanup (succ i) j
             | None ->
                  info.mp_values <- remove info.mp_key_numbers.(i) info.mp_values;
                  cleanup (succ i) i
         else
            j
      in
      let j = cleanup 0 (-1) in
         if j < 0 then
            None
         else
            Some j

   (*
    * Append an entry to the weak queue.
    *)
   let append_share info key =
      let length = Weak.length info.mp_keys in
      let weak' = Weak.create (succ length) in
      let numbers' = Array.create (succ length) 0 in
         Weak.blit info.mp_keys 0 weak' 0 length;
         Weak.set weak' length (Some key);
         Array.blit info.mp_key_numbers 0 numbers' 0 length;
         numbers'.(length) = key.key_index;
         info.mp_keys <- weak';
         info.mp_key_numbers <- numbers'

   (*
    * Broadcast a new entry.
    *)
   let share info debug x =
      lock_queue info;
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.share: %s%t" debug eflush;
            unlock_printer ()
         end;
      try
         let key = find_share info x in
            unlock_queue info;
            key
      with
         Not_found ->
            let id = Appl_outboard_client.endpt info.mp_ensemble in
            let number = info.mp_key_index in
            let key = { key_id = id; key_index = number } in
            let _ =
               match cleanup_shares info with
                  Some i ->
                     Weak.set info.mp_keys i (Some key);
                     Array.set info.mp_key_numbers i number
                | None ->
                     append_share info key
            in
            let keyv =
               { keyv_id = id;
                 keyv_index = number;
                 keyv_value = x;
                 keyv_local = x
               }
            in
               info.mp_key_index <- succ number;
               info.mp_values <- keyv :: info.mp_values;
               send_message info "new_share" (Appl_outboard_client.Cast (CastNewShare (number, x)));
               unlock_queue info;
               key

   (*
    * Change the local value.
    *)
   let share_local info { key_id = id; key_index = number } x =
      let rec search = function
         { keyv_id = id'; keyv_index = number' } as keyv :: t ->
            if id' = id && number' = number then
               keyv.keyv_local <- x
            else
               search t
       | [] ->
            raise Not_found
      in
         search info.mp_values

   (*
    * Get the value associated with a key.
    *)
   let arg_of_key info { key_id = id; key_index = number } =
      let rec search = function
         { keyv_id = id'; keyv_index = number'; keyv_local = x } :: t ->
            if id' = id && number' = number then
               x
            else
               search t
       | [] ->
            raise (Failure "arg_of_key")
      in
         lock_queue info;
         if !debug_share then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.arg_of_key%t" eflush;
               unlock_printer ()
            end;
         let values = info.mp_values in
            unlock_queue info;
            search values
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
