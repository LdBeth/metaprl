(*
 * Interface to ensemble.
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

open Mp_debug
open Thread_util

open Hsys
open Ensemble
open Ensemble.Util
open Ensemble.View
open Ensemble.Appl_handle

open Printf

(*
 * Debug variables.
 *)
let debug_ensemble =
   create_debug (**)
      { debug_name = "ensemble";
        debug_description = "Display NL Ensemble Application actions";
        debug_value = false
      }

let debug_share =
   create_debug (**)
      { debug_name = "share";
        debug_description = "Display NL Ensemble Application memory sharing";
        debug_value = false
      }

let debug_marshal =
   create_debug (**)
      { debug_name = "ensemble";
        debug_description = "Display NL Ensemble Application actions";
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
      { id_id : Endpt.id;
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
        entry_value : 'a
      }

   type 'a remote_entry =
      { remote_entry : 'a entry;
        remote_lock : Endpt.id
      }

   type ('a, 'b) handle = 'a entry
   type ('a, 'b) lock = 'a entry

   (*
    * Shared keys.  Make sure this is a separate cell,
    * so that the garbage collector can collect it.
    *)
   type 'c key =
      { key_id : Endpt.id;
        key_index : int
      }

   type 'c key_value =
      { keyv_id : Endpt.id;
        keyv_index : int;
        keyv_value : 'c;
        mutable keyv_local : 'c
      }

   type 'c key_info =
      { keyi_id : Endpt.id;
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
    | CastUnlock of id
    | CastEntry of int * 'a
    | CastDelete of int
    | CastResult of id * 'b
    | CastNewShare of int * 'c
    | CastDeleteShare of int
    | CastQueue of 'a entry list * 'a entry list * 'a remote_entry list * 'c key_info list
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
         *    remote: entries locked by a remote process
         *    index: the number of the last entry we locked
         *)
        mutable mp_unlocked : 'a entry list;
        mutable mp_local : 'a entry list;
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
         *   quick_lock: true if quick locking is eanbled
         *   pending_lock: the id of the unlocked event we want to lock
         *   upcalls: a list of messages to be sent as upcalls (in reverse order)
         *   upcall_chan: the channel for communicating the upcall
         *)
        mp_quick_lock : bool;
        mutable mp_pending_lock : ('a, 'b) lock option;
        mutable mp_upcalls : ('a, 'b) upcall list;
        mp_upcall_chan : ('a, 'b) upcall Thread_event.channel;

        (*
         * Server info:
         *    queue: list of messages to be sent to Ensemble on the heartbeat
         *    new_view: a flag indicating that a new view is in progress
         *    rank_flags: a flag indicating which ranks have sent their stack
         *       to the coordinator after a view change.
         *)
        mutable mp_queue : ('a, 'b, 'c) message New.naction list;
        mutable mp_new_view : bool;
        mutable mp_rank_flags : bool array;

        (*
         * Ensemble info:
         *    mp_global_state: state shared by all views
         *    mp_local_state: state local to this member
         *    mp_heartbeat: function to invoke a heartbeat
         *)
        mutable mp_global_state : View.state;
        mutable mp_local_state : View.local;
        mutable mp_handles : Appl_handle.handle Arrayf.t;
        mutable mp_hbeat : unit -> unit;
      }

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Printing.
    *)
   let print_entry_list out entries =
      let print { entry_id = { id_id = id; id_number = number } } =
         fprintf out " %s:%d" (Endpt.string_of_id id) number
      in
         List.iter print entries

   let print_remote_list out entries =
      let print { remote_entry = { entry_id = { id_id = id; id_number = number } } } =
         fprintf out " %s:%d" (Endpt.string_of_id id) number
      in
         List.iter print entries

   let print_share_list out values =
      let print { keyv_id = id; keyv_index = number } =
         fprintf out " %s:%d" (Endpt.string_of_id id) number
      in
         List.iter print values

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
      (mem_remote id info.mp_remote)

   (*
    * Remove entries from their queues.
    *)
   let rec remove_entry id = function
      entry :: entries ->
         if entry.entry_id = id then
            entry, entries
         else
            let entry', entries = remove_entry id entries in
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

   (*
    * Find the rank of a handle.
    *)
   let rank_of_handle view hand =
      let id = endpt_of_handle hand in
      let len = Arrayf.length view in
      let rec search i =
         if i = len then
            raise Not_found
         else
            let id' = Arrayf.get view i in
               if id' = id then
                  i
               else
                  search (succ i)
      in
         search 0

   let handle_of_rank info rank =
      Arrayf.get info.mp_handles rank

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
         Thread_event.sync 0 (Thread_event.send info.mp_upcall_chan upcall)
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
      if !debug_marshal then
         begin
            lock_printer ();
            begin
               match message with
                  Cast msg ->
                     eprintf "Ensemble_queue.send_message: %s: cast %d bytes%t" (**)
                        debug (String.length (Marshal.to_string msg [Marshal.Closures])) eflush
                | Send (_, msg) ->
                     eprintf "Ensemble_queue.send_message: %s: send %d bytes%t" (**)
                        debug (String.length (Marshal.to_string msg [Marshal.Closures])) eflush
                | Control _ ->
                     eprintf "Ensemble_queue.send_message: control%t" eflush
            end;
            unlock_printer ()
         end;
      info.mp_queue <- message :: info.mp_queue

   (*
    * Add a new entry to the unlocked queue.
    * Check that we have no prior knowledge of then entry--
    * This would occur if the entry was added locally.
    *)
   let handle_new_entry info srchand number x =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_new_entry: %d%t" number eflush;
            unlock_printer ()
         end;
      let id =
         { id_id = endpt_of_handle srchand;
           id_number = number
         }
      in
         if not (entry_exists info id) then
            let entry = { entry_id = id; entry_value = x } in
               send_upcall info UpcallView;
               info.mp_unlocked <- entry :: info.mp_unlocked

   (*
    * When a lock message arrives, check if it
    * is a response to a lock we requested.
    *)
   let remote_lock info srchand id =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.remote_lock: %s:%d%t" (**)
               (Endpt.string_of_id id.id_id) id.id_number eflush;
            unlock_printer ()
         end;
      try
         let entry, entries = remove_entry id info.mp_unlocked in
         let remote =
            { remote_entry = entry;
              remote_lock = endpt_of_handle srchand
            }
         in
            info.mp_unlocked <- entries;
            info.mp_remote <- remote :: info.mp_remote
      with
         Not_found ->
            ()

   let install_lock info id =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.install_lock: %s:%d%t" (**)
               (Endpt.string_of_id id.id_id) id.id_number eflush;
            unlock_printer ()
         end;
      let entry, entries = remove_entry id info.mp_unlocked in
         info.mp_unlocked <- entries;
         info.mp_local <- entry :: info.mp_local;
         info.mp_pending_lock <- None;
         if not info.mp_quick_lock then
            send_upcall info (UpcallLock entry)

   let cancel_lock info srchand entry =
      let id = entry.entry_id in
         if !debug_ensemble then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.cancel_lock: %s:%d%t" (**)
                  (Endpt.string_of_id id.id_id) id.id_number eflush;
               unlock_printer ()
            end;
         remote_lock info srchand id;
         info.mp_pending_lock <- None;
         if info.mp_quick_lock then
            send_upcall info (UpcallCancel entry)

   let handle_lock info srchand id =
      match info.mp_pending_lock with
         Some entry ->
            if entry.entry_id = id then
               if endpt_of_handle srchand = info.mp_local_state.endpt then
                  (* We got the lock *)
                  install_lock info id
               else
                  (* Someone else got the lock before us *)
                  cancel_lock info srchand entry
            else
               (* Some other entry *)
               remote_lock info srchand id
       | None ->
            remote_lock info srchand id

   (*
    * A previously locked entry is now unlocked.
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
      let _, entries = remove_entry id info.mp_local in
         info.mp_local <- entries

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
    * Handle a result for an entry.  The entry is
    * removed.  If the entry is locally owned, return
    * the value in an upcall.  If the entry is not
    * remote, then it will be local.
    *)
   let remote_result info id x =
      try
         let { remote_entry = entry }, remotes = remove_remote id info.mp_remote in
            info.mp_remote <- remotes;
            if entry.entry_id.id_id = info.mp_local_state.endpt then
               begin
                  if !debug_ensemble then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.remote_result: %d%t" (**)
                           entry.entry_id.id_number eflush;
                        unlock_printer ();
                     end;
                  send_upcall info (UpcallResult (entry, x))
               end;
            true
      with
         Not_found ->
            false

   let local_result info id x =
      try
         let entry, entries = remove_entry id info.mp_local in
            info.mp_local <- entries;
            if entry.entry_id.id_id = info.mp_local_state.endpt then
               begin
                  if !debug_ensemble then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.local_result: %d%t" (**)
                           entry.entry_id.id_number eflush;
                        unlock_printer ();
                     end;
                  send_upcall info (UpcallResult (entry, x))
               end
      with
         Not_found ->
            ()

   let handle_result info srchand id x =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_result%t" eflush;
            unlock_printer ()
         end;
      if (remote_result info id x) then
         local_result info id x

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

   let handle_delete info srchand number =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_delete: %d%t" number eflush;
            unlock_printer ()
         end;
      let id = { id_id = endpt_of_handle srchand; id_number = number } in
      let _ =
         (*
          * If a lock was being reqested, delete it.
          * The next lock request will restart it.
          *)
         match info.mp_pending_lock with
            Some entry ->
               if entry.entry_id = id then
                  begin
                     info.mp_pending_lock <- None;
                     if info.mp_quick_lock then
                        send_upcall info (UpcallCancel entry)
                  end
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

   let handle_new_share info srchand number x =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue: new_share%t" eflush;
            unlock_printer ()
         end;
      let id = endpt_of_handle srchand in
         if not (share_exists id number info.mp_values) then
            let keyv =
               { keyv_id = id;
                 keyv_index = number;
                 keyv_value = x;
                 keyv_local = x
               }
            in
               info.mp_values <- keyv :: info.mp_values

   let handle_delete_share info srchand number =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue: delete_share%t" eflush;
            unlock_printer ()
         end;
      let id = endpt_of_handle srchand in
      let rec remove = function
         { keyv_id = id'; keyv_index = number' } as h :: t ->
            if id' = id && number' = number then
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
            [||]
         end
      else
         let upcalls = List.rev info.mp_upcalls in
         let messages = Array.of_list (List.rev info.mp_queue) in
            info.mp_upcalls <- [];
            info.mp_queue <- [];
            Mutex.unlock info.mp_lock;
            issue_upcalls info upcalls;
            messages

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
            if Arrayf.mem remote.remote_entry.entry_id.id_id view then
               if Arrayf.mem remote.remote_lock view then
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
            if Arrayf.mem local.entry_id.id_id view then
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
            if Arrayf.mem entry.entry_id.id_id view then
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
            if not (Arrayf.mem entry.entry_id.id_id view) then
               begin
                  info.mp_pending_lock <- None;
                  if info.mp_quick_lock then
                     send_upcall info (UpcallCancel entry)
               end
       | None ->
            ()

   (*
    * Prune the shared info.
    *)
   let prune_shares info view =
      let rec prune = function
         { keyv_id = id } as h :: t ->
            if Arrayf.mem id view then
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
            mp_local_state = { rank = rank };
            mp_values = values
          } = info
      in
      let coord_handle = handle_of_rank info 0 in
         if !debug_ensemble then
            begin
               lock_printer ();
               eprintf "Ensemble_queue.send_stack: %s%t" (Endpt.string_of_id info.mp_local_state.endpt) eflush;
               unlock_printer ()
            end;
         [|Send ([|coord_handle|], (SendQueue (unlocked, local, remote, strip_share values)))|]

   (*
    * If the rank array is filled, broadcast the new stack.
    * This should be the first queued message to go out.
    *)
   let cast_stack info =
      if Array_util.all_true info.mp_rank_flags then
         let { mp_unlocked = unlocked;
               mp_local = local;
               mp_remote = remote;
               mp_values = values
             } = info
         in
            if !debug_ensemble then
               begin
                  lock_printer ();
                  eprintf "Ensemble_queue.cast_stack%t" eflush;
                  unlock_printer ()
               end;
            info.mp_rank_flags <- [||];
            info.mp_new_view <- false;
            info.mp_queue <- info.mp_queue @ [Cast (CastQueue (unlocked, local, remote, strip_share values))]

   (*
    * Merge the communicated stack with our stack.
    * Once all the entries have been received,
    * broadcast the new stack.
    *)
   let handle_queue_send info srchand unlocked local remote shares =
      let rank = rank_of_handle info.mp_global_state.view srchand in
         if !debug_ensemble then
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
            eprintf "Ensemble_queue.new_view: %s\n" (Endpt.string_of_id info.mp_local_state.endpt);
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
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.handle_view_change: %d%t" (**)
               (Arrayf.length info.mp_global_state.view) eflush;
            unlock_printer ()
         end;
      let view = info.mp_global_state.view in
      let length = Arrayf.length view in
      let rank = info.mp_local_state.rank in
         prune_remote info view;
         prune_local info view;
         prune_unlocked info view;
         prune_shares info view;
         if length > 1 then
            begin
               info.mp_new_view <- true;
               if rank = 0 then
                  let flags = Array.create length false in
                     flags.(0) <- true;
                     info.mp_rank_flags <- flags;
                     [||]
               else
                  send_stack info
            end
         else
            [||]

   (************************************************************************
    * ENSEMBLE INTERFACE                                                   *
    ************************************************************************)

   (*
    * Handle a received message.
    *)
   let receive info srchand blockp castp msg =
      lock_info info;
      begin
         match msg with
            CastEntry (number, x) ->
               handle_new_entry info srchand number x
          | CastDelete number ->
               handle_delete info srchand number
          | CastLock id ->
               handle_lock info srchand id
          | CastUnlock id ->
               handle_unlock info srchand id
          | CastResult (number, x) ->
               handle_result info srchand number x
          | CastNewShare (number, x) ->
               handle_new_share info srchand number x
          | CastDeleteShare number ->
               handle_delete_share info srchand number
          | SendQueue (unlocked, local, remote, shares) ->
               handle_queue_send info srchand unlocked local remote shares
          | CastQueue (unlocked, local, remote, shares) ->
               handle_queue_cast info unlocked local remote shares
      end;
      unlock_info info

   (*
    * The heartbeat sends all the messages in the queue.
    *)
   let heartbeat info now =
      lock_info info;
      if false && !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.heartbeat%t" eflush;
            unlock_printer ()
         end;
      unlock_info info

   (*
    * View changes.
    * We don't do much except recompute the ranks of the players.
    *)
   let block info () =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.block%t" eflush;
            unlock_printer ()
         end;
      [||]

   let disable info () =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.disable%t" eflush;
            unlock_printer ()
         end

   let make_handlers info =
      { New.block     = block info;
        New.heartbeat = heartbeat info;
        New.receive   = receive info;
        New.disable   = disable info
      }

   let install info (ls, vs) handles =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.install%t" eflush;
            unlock_printer ()
         end;
      lock_info info;
      info.mp_global_state <- vs;
      info.mp_local_state <- ls;
      info.mp_handles <- handles;
      let msgs = handle_view_change info in
      let handlers = make_handlers info in
      let msgs' = unlock_info info in
         Array.append msgs msgs', handlers

   let exit info () =
      if !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Mpapp.exit%t" eflush;
            unlock_printer ()
         end;
      ()

   (*
    * Create the initial state for the application.
    *)
   let open_nl quick ls vs =
      { mp_lock = Mutex.create ();

        mp_unlocked = [];
        mp_local = [];
        mp_remote = [];
        mp_index = 0;

        mp_keys = Weak.create 0;
        mp_key_index = 0;
        mp_key_numbers = [||];
        mp_values = [];

        mp_quick_lock = quick;
        mp_pending_lock = None;
        mp_upcalls = [];
        mp_upcall_chan = Thread_event.new_channel ();

        mp_queue = [];
        mp_new_view = false;
        mp_rank_flags = [||];

        mp_global_state = vs;
        mp_local_state = ls;
        mp_handles = Arrayf.empty;
        mp_hbeat = Appl.async ls.async
      }

   (*
    * Main loop is just the Appl main loop,
    * but we print exceptions.
    *)
   let main_loop_aux _ =
      Printexc.catch (Unix.handle_unix_error Appl.main_loop) ()

   let main_loop _ =
      Thread.create main_loop_aux ();
      ()

   (************************************************************************
    * QUEUE IMPLEMENTATION                                                 *
    ************************************************************************)

   (*
    * Return the Ensemble arguments.
    *)
   let args = Arge.args

   (*
    * Startup code.
    * The quick flag if PreLocks are to be delivered.
    *)
   let create quick =
      (*
       * Need total ordering of messages.
       * This also gives us local delivery.
       *)
      let properties = List.map Property.string_of_id Property.total in
      let properties = String.concat ":" properties in
      let _ = Arge.set_default Arge.properties properties in

      (*
       * Get default transport for this group,
       * and override the endpoint.
       *)
      let ls, vs = Appl.default_info "nlapp" in
      let endpt = Endpt.named "NLAPP" in
      let vs = View.set vs [Vs_view (Arrayf.create 1 endpt)] in
      let ls = View.local "NLAPP" endpt vs in
      let state = ls, vs in

      (*
       * Initialize the application interface.
       *)
      let info = open_nl quick ls vs in
      let handlers = make_handlers info in
      let interface =
         { New.heartbeat_rate = Time.of_int 60;
           New.install        = install info;
           New.exit           = exit info
         }
      in
      let _, interface = Appl_handle.New.f interface in

      (*
       * Initialize the protocol stack, using the interface and
       * view state chosen above.
       *)
      let interface = Appl_intf.New.debug_view "NLSERVER" interface in
      let interface = Appl_closure.full interface in
         Appl.config_new interface state;
         info

   (*
    * Queue locking.
    * When the queue is unlocked, request a heartbeat if the
    * send queue is nonempty.
    *)
   let lock_queue info =
      Mutex.lock info.mp_lock

   let unlock_queue info =
      let flag = info.mp_queue <> [] in
         Mutex.unlock info.mp_lock;
         if flag then
            info.mp_hbeat ()

   (*
    * Get the upcall event queue.
    *)
   let event_of_queue info =
      Thread_event.receive info.mp_upcall_chan

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
      let id = { id_id = info.mp_local_state.endpt; id_number = index } in
      let entry = { entry_id = id; entry_value = x } in
         info.mp_index <- succ index;
         info.mp_unlocked <- entry :: info.mp_unlocked;
         send_message info "add" (Cast (CastEntry (index, x)));
         unlock_queue info;
         entry

   (*
    * Delete an entry in the queue.
    * The entry is not actually deleted until the
    * broadcast message comes back.
    *)
   let delete info entry =
      lock_queue info;
      if true || !debug_ensemble then
         begin
            lock_printer ();
            eprintf "Ensemble_queue.delete: %d%t" entry.entry_id.id_number eflush;
            unlock_printer ()
         end;
      send_message info "delete" (Cast (CastDelete entry.entry_id.id_number));
      unlock_queue info

   (*
    * Try an lock an entry.
    * If a lock is already pending, don't do anything.
    * Otherwise, select a random entry from the unlocked queue
    * and try to lock it.
    *)
   let lock info =
      lock_queue info;
      begin
         let { mp_quick_lock = quick;
               mp_pending_lock = pending;
               mp_unlocked = unlocked
             } = info
         in
            if pending = None && unlocked <> [] then
               let entry = List.nth unlocked (Random.int (List.length unlocked)) in
               let id = entry.entry_id in
                  if !debug_ensemble then
                     begin
                        lock_printer ();
                        eprintf "Ensemble_queue.lock: attempting lock on %s:%d%t" (**)
                           (Endpt.string_of_id id.id_id) id.id_number eflush;
                        unlock_printer ()
                     end;
                  info.mp_pending_lock <- Some entry;
                  send_message info "lock" (Cast (CastLock id));
                  if quick then
                     send_upcall info (UpcallPreLock entry)
      end;
      unlock_queue info

   (*
    * Get the argument for a lock.
    *)
   let arg_of_lock { entry_value = x } =
      x

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
      send_message info "unlock" (Cast (CastUnlock lock.entry_id));
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
      send_message info "result" (Cast (CastResult (lock.entry_id, x)));
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
      let id = info.mp_local_state.endpt in
      let weak = info.mp_keys in
      let length = Weak.length weak in
      let rec remove number = function
         { keyv_id = id'; keyv_index =  number' } as h :: t ->
            if id' = id && number' = number then
               begin
                  send_message info "delete_share" (Cast (CastDeleteShare number));
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
            let id = info.mp_local_state.endpt in
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
               send_message info "new_share" (Cast (CastNewShare (number, x)));
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
