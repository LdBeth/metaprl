(*
 * Define an application interface for outboard Ensemble.
 * Communication with Ensemble is through a shared memory buffer.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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

open Appl_outboard_common

let debug_outboard =
   create_debug (**)
      { debug_name = "outboard";
        debug_description = "Ensemble outboard operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Identifier for an endpoint.
 *)
type id = string

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
and ('cast, 'send) appl_handlers =
   { install : ('cast, 'send) t -> ('cast, 'send) message list * ('cast, 'send) view_handlers }

(*
 * This is the Ensemble handle.
 * ens_mutex: lock on the data structure
 * ens_pipe: shared memory pipe
 *)
and ('cast, 'send) t =
   { ens_lock : Mutex.t;
     ens_pipe : Lm_mmap_pipe.t;
     mutable ens_queue : ('cast, 'send) message list;
     mutable ens_endpt : id;
     mutable ens_view : view_state;
     mutable ens_view_handlers : ('cast, 'send) view_handlers
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Install a new view.
 * Pass up the new view, the flush the queue.
 *)
let install info appl_handlers id view =
   info.ens_endpt <- id;
   info.ens_view <- view;
   let { install = install } = appl_handlers in
   let msgs, handlers = install info in
      info.ens_view_handlers <- handlers;
      msgs

(*
 * Handle a message from Ensemble.
 *)
let handle_event info appl_handlers code id msg =
   let msgs =
      if code = cast_code then
         info.ens_view_handlers.receive info id (CastData (Obj.magic msg))
      else if code = send_code then
         info.ens_view_handlers.receive info id (SendData (Obj.magic msg))
      else if code = new_view_code then
         install info appl_handlers id (Obj.magic msg)
      else if code = block_code then
         info.ens_view_handlers.block info
      else
         begin
            eprintf "Appl_outboard_client.handle_event: bogus event number %d%t" code eflush;
            []
         end
   in
      Mutex.lock info.ens_lock;
      info.ens_queue <- info.ens_queue @ msgs;
      Mutex.unlock info.ens_lock

(*
 * Try to receive a message from Ensemble.
 *)
let try_recv info appl_handlers =
   let raw_read buf off len =
      Marshal.from_string buf off
   in
      match Lm_mmap_pipe.read info.ens_pipe raw_read with
         Some (code, id, data) ->
            handle_event info appl_handlers code id data
       | None ->
            ()

(*
 * Try to send a message to Ensemble.
 *)
let try_send info =
   Mutex.lock info.ens_lock;
   begin
      match info.ens_queue with
         msg :: t ->
            begin
               match msg with
                  Cast data ->
                     let raw_write buf off len =
                        if !debug_outboard then
                           eprintf "Appl_outboard_client.to_buffer: Cast (0x%08x, %d, %d)%t" (Obj.magic buf) off len eflush;
                        Marshal.to_buffer buf off len data [Marshal.Closures]
                     in
                        if Lm_mmap_pipe.write info.ens_pipe cast_code "" raw_write then
                           info.ens_queue <- t
                | Send (id, data) ->
                     let raw_write buf off len =
                        if !debug_outboard then
                           eprintf "Appl_outboard_client.to_buffer: Send (0x%08x, %d, %d)%t" (Obj.magic buf) off len eflush;
                        Marshal.to_buffer buf off len data [Marshal.Closures]
                     in
                        if Lm_mmap_pipe.write info.ens_pipe send_code id raw_write then
                           info.ens_queue <- t
            end
       | [] ->
            ()
   end;
   Mutex.unlock info.ens_lock

(*
 * Pipe loop just keeping issuing events.
 *)
let rec reader_thread info appl_handlers =
   try_send info;
   try_recv info appl_handlers;
   let _ = Lm_mmap_pipe.block info.ens_pipe in
      reader_thread info appl_handlers

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Arguments.
 *)
let args () =
   []

(*
 * Create an Ensemble application.
 * We need:
 *    1. an endpoint name,
 *    2. an application name
 *    3. an application handler record
 *)
let create endpt_name appl_name create_handlers =
   let pipe = Lm_mmap_pipe.create_client shared_dir in
   let rec sync () =
      let raw_read buf off len =
         Marshal.from_string buf off
      in
         if !debug_outboard then
            begin
               lock_printer ();
               eprintf "Appl_outboard_client: waiting for new view%t" eflush;
               unlock_printer ()
            end;
         (let _ = Lm_mmap_pipe.block pipe in ());
         match Lm_mmap_pipe.read pipe raw_read with
            Some (code, id, view) ->
               if code = start_code then
                  begin
                     if !debug_outboard then
                        begin
                           lock_printer ();
                           eprintf "Appl_outboard_client: got view %s [" id;
                           List.iter (eprintf " %s") view;
                           eflush stderr;
                           unlock_printer ()
                        end;
                     id, view
                  end
               else
                  sync ()
          | None ->
               if !debug_outboard then
                  begin
                     lock_printer ();
                     eprintf "Appl_outboard_client: read failed%t" eflush;
                     unlock_printer ()
                  end;
               sync ()
   in
   let endpt, view = sync () in
   let info =
      { ens_lock = Mutex.create ();
        ens_pipe = pipe;
        ens_queue = [];
        ens_endpt = endpt;
        ens_view = view;
        ens_view_handlers =
           { block = (fun _ -> raise (Failure "Appl_outboard_client.create"));
             receive = (fun _ -> raise (Failure "Appl_outboard_client.create"))
           }
      }
   in
   let arg, handlers = create_handlers info in
      info.ens_queue <- install info handlers endpt view;
      (let _ = Thread.create (reader_thread info) handlers in ());
      arg

(*
 * Send a message in the current view.
 *)
let send info message =
   Mutex.lock info.ens_lock;
   let { ens_queue = queue; ens_pipe = pipe } = info in
      info.ens_queue <- queue @ [message];
      Mutex.unlock info.ens_lock;
      try_send info

(*
 * Get info about the current view.
 *)
let endpt { ens_endpt = id } =
   id

(*
 * Get the view.
 *)
let view { ens_view = view } =
   view

(*
 * String of id.
 *)
let string_of_id id =
   id

(*
 * No main loop.
 *)
let main_loop _ =
   ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
