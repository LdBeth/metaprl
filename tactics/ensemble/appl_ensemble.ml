(*
 * Define an application interface for Ensemble.  This differs
 * a little from Ensemble, because messages can be queued at
 * any time (so there is no hearbeat).  Also, the local state
 * and view state are just reduced.
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

open Hsys
open Ensemble
open Ensemble.Util
open Ensemble.View
open Ensemble.Appl_handle

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Identifier for an endpoint.
 *)
type id = Endpt.id

type view_state = id array

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
 *)
and ('cast, 'send) t =
   { (*
      * Server info:
      *    ens_queue: list of messages to be sent to Ensemble on the heartbeat
      *    ens_global_state: state shared by all views
      *    ens_local_state: state local to this member
      *    ens_heartbeat: function to invoke a heartbeat
      *)
      mutable ens_queue : ('cast, 'send) message list;
      mutable ens_global_state : View.state;
      mutable ens_local_state : View.local;
      mutable ens_handles : Appl_handle.handle Arrayf.t;
      mutable ens_heartbeat : unit -> unit
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Get the handle for an endpoint.
 *)
let handle_of_endpt info id =
   let view = info.ens_global_state.view in
   let length = Arrayf.length view in
   let rec search i =
      if i = length then
         raise Not_found
      else if Arrayf.get view i = id then
         Arrayf.get info.ens_handles i
      else
         search (succ i)
   in
      search 0

(*
 * Convert our messages to the appropriate actions.
 *)
let wrap_messages info msgs =
   let rec wrap = function
      h :: t ->
         begin
            match h with
               Cast x ->
                  Appl_handle.Cast (CastData x) :: wrap t
             | Send (id, x) ->
                  try
                     Appl_handle.Send ([|handle_of_endpt info id|], SendData x) :: wrap t
                  with
                     Not_found ->
                        wrap t
         end
    | [] ->
         []
   in
      Array.of_list (wrap msgs)

(*
 * On a heartbeat, send the messages in the queue.
 *)
let heartbeat info now =
   let msgs = info.ens_queue in
      info.ens_queue <- [];
      wrap_messages info msgs

let dequeue info msgs =
   let msgs' = info.ens_queue in
      info.ens_queue <- [];
      wrap_messages info (msgs' @ msgs)

(*
 * Pass up the block.
 *)
let block info block' () =
   wrap_messages info (block' info)

(*
 * Receive some messages.
 *)
let receive info receive' srchand blockp castp msg =
   let id = endpt_of_handle srchand in
   let msgs = receive' info id msg in
      dequeue info msgs

(*
 * Do nothing on disable.
 *)
let disable info () =
   ()

let exit info () =
   ()

(*
 * Install a new view.
 * Pass up the new view, the flush the queue.
 *)
let install info appl_handlers (ls, vs) handles =
   info.ens_global_state <- vs;
   info.ens_local_state <- ls;
   info.ens_handles <- handles;
   let { install = install } = appl_handlers in
   let msgs, handlers = install info in
   let { block = block'; receive = receive' } = handlers in
   let handlers =
      { New.block     = block info block';
        New.receive   = receive info receive';
        New.heartbeat = heartbeat info;
        New.disable   = disable info
      }
   in
      dequeue info msgs, handlers

(*
 * Arguments.
 *)
let args = Arge.args

(*
 * Create an Ensemble application.
 * We need:
 *    1. an endpoint name,
 *    2. an application name
 *    3. an application handler record
 *)
let create endpt_name appl_name create_handlers =
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
   let ls, vs = Appl.default_info endpt_name in
   let endpt = Endpt.named endpt_name in
   let vs = View.set vs [Vs_view (Arrayf.create 1 endpt)] in
   let ls = View.local endpt_name endpt vs in

   (*
    * Initialize the application interface.
    *)
   let info =
      { ens_queue = [];
        ens_global_state = vs;
        ens_local_state = ls;
        ens_handles = Arrayf.empty;
        ens_heartbeat = Appl.async ls.async
      }
   in
   let arg, handlers = create_handlers info in
   let interface =
      { New.heartbeat_rate = Time.of_int 600;
        New.install        = install info handlers;
        New.exit           = exit info
      }
   in
   let _, interface = Appl_handle.New.f interface in

   (*
    * Initialize the protocol stack, using the interface and
    * view state chosen above.
    *)
   let interface = Appl_intf.New.debug_view appl_name interface in
   let interface = Appl_closure.full interface in
      Appl.config_new interface (ls, vs);
      arg

(*
 * Send a message in the current view.
 *)
let send info message =
   let { ens_queue = queue; ens_heartbeat = heartbeat } = info in
      info.ens_queue <- queue @ [message];
      heartbeat ()

(*
 * Get info about the current view.
 *)
let endpt { ens_local_state = { endpt = id } } =
   id

(*
 * Get the view.
 *)
let view { ens_global_state = { view = view } } =
   Arrayf.to_list view

(*
 * String of id.
 *)
let string_of_id = Endpt.string_of_id

(*
 * Main loop is just the Appl main loop,
 * but we print exceptions.
 *)
let main_loop_aux _ =
   Unix.handle_unix_error Appl.main_loop ()

let main_loop _ =
   Thread.create main_loop_aux ();
   ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
