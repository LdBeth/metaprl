(*
 * Outboard Ensemble server.
 * Communication is through a shared pipe.
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
open Hsys
open Ensemble
open Ensemble.Util
open Ensemble.View
open Ensemble.Appl_handle

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
 * The server info:
 *    ens_pipe: the shared memory pipe (we are the server)
 *    ens_up_queue: queue of messages to send to client
 *    ens_dn_queue: queue of messages to send on next heartbeat
 *    ens_server: server socket to poll for client connections
 *    ens_client: client socket to poll for activity
 *)
type t =
   { ens_pipe : Lm_mmap_pipe.t;
     mutable ens_up_queue : (int * string * (string -> int -> int -> int)) list;
     mutable ens_dn_queue : Iovecl.t New.naction list;
     ens_server : Hsys.socket;
     mutable ens_client : Hsys.socket option;

     (* Ensemble stuff *)
     ens_alarm : Alarm.t;
     mutable ens_local_state : View.local;
     mutable ens_view_state : View.state;
     mutable ens_handles : Appl_handle.handle Arrayf.t;
     mutable ens_strings : string list;
     mutable ens_heartbeat : unit -> unit
   }

let iovec_name = "Appl_outboard_server"

(************************************************************************
 * PIPE HANDLER                                                         *
 ************************************************************************)

(*
 * Send a message to the client.
 *)
let send_data info code id vecl =
   let raw_write buf off len =
      Iovecl.flatten_buf iovec_name vecl buf off len
   in
      info.ens_up_queue <- info.ens_up_queue @ [code, id, raw_write]

(*
 * Marshal a value up.
 *)
let send_view info code =
   let id = Endpt.string_of_id info.ens_local_state.endpt in
   let view = List.map Endpt.string_of_id (Arrayf.to_list info.ens_view_state.view) in
   let raw_write buf off len =
      if !debug_outboard then
         eprintf "Appl_outboard_server.to_buffer: send_view (0x%08x, %d, %d)%t" (Obj.magic buf) off len eflush;
      Marshal.to_buffer buf off len view []
   in
      info.ens_up_queue <- info.ens_up_queue @ [code, id, raw_write]

let send_start info =
   let id = Endpt.string_of_id info.ens_local_state.endpt in
   let raw_write buf off len =
      if !debug_outboard then
         eprintf "Appl_outboard_server.to_buffer: (send_start 0x%08x, %d, %d/%d)%t" (Obj.magic buf) off len (String.length buf) eflush;
      Marshal.to_buffer buf off len [id] []
   in
      if not (Lm_mmap_pipe.write info.ens_pipe start_code id raw_write) then
         eprintf "Appl_outboard_server: can't write initial view%t" eflush

(*
 * Send a message.  Place it on the dn_queue,
 * and ask for a heartbeat.
 *)
let send_message info msg =
   info.ens_dn_queue <- info.ens_dn_queue @ [msg];
   info.ens_heartbeat ()

(*
 * Map a string id to a handle.
 *)
let handle_of_string info id =
   let rec search i = function
      id' :: t ->
         if id = id' then
            Arrayf.get info.ens_handles i
         else
            search (succ i) t
    | [] ->
         raise Not_found
   in
      search 0 info.ens_strings

(*
 * Handle an event from the client.
 *)
let handle_event info code name vecl =
   if code = cast_code then
      send_message info (Cast vecl)
   else if code = send_code then
      try
         send_message info (Send ([|handle_of_string info name|], vecl))
      with
         Not_found ->
            ()

(*
 * See if anything can be done with the client.
 *)
let msg_counter = ref 0
let size_counter = ref 0

let poll_client info =
   let { ens_pipe = pipe; ens_client = client; ens_up_queue = up_queue } = info in
      match client with
         Some _ ->
            let try_send () =
               match up_queue with
                  (code, name, raw_write) :: t ->
                     if Lm_mmap_pipe.write pipe code name raw_write then
                        info.ens_up_queue <- t
                | [] ->
                     ()
            in
            let try_recv () =
               let raw_read buf off len =
                  msg_counter := succ !msg_counter;
                  size_counter := !size_counter + len;
                  Mbuf.allocl iovec_name buf off len
               in
                  match Lm_mmap_pipe.read pipe raw_read with
                     Some (code, name, data) ->
                        eprintf "Msg: %d: %d%t" !msg_counter !size_counter eflush;
                        handle_event info code name data
                   | None ->
                        ()
            in
               try_send ();
               try_recv ()

       | None ->
            ()

(*
 * Handle a wakeup event from the client.
 *)
let handle_client_event info () =
   let { ens_pipe = pipe; ens_up_queue = up_queue } = info in
      if Lm_mmap_pipe.block pipe then
         poll_client info
      else
         match info.ens_client with
            Some client ->
               eprintf "Closing client connection%t" eflush;
               Alarm.rmv_sock_recv info.ens_alarm client;
               info.ens_client <- None;
               Lm_mmap_pipe.close_client pipe
          | None ->
               raise (Invalid_argument "Appl_outboard_server.handle_client_event")

(*
 * Handle a client connection.
 *)
let handle_server_event info () =
   let { ens_pipe = pipe } = info in
      if Lm_mmap_pipe.open_client pipe then
         let client = Hsys.socket_of_file_descr (Lm_mmap_pipe.client_socket pipe) in
            info.ens_client <- Some client;
            info.ens_heartbeat ();
            Alarm.add_sock_recv info.ens_alarm iovec_name (**)
               client (Hsys.Handler0 (handle_client_event info));
            send_start info

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * On a heartbeat, send the messages in the queue.
 *)
let dequeue info =
   let msgs = info.ens_dn_queue in
      info.ens_dn_queue <- [];
      Array.of_list msgs

let heartbeat info now =
   poll_client info;
   dequeue info

(*
 * Pass up the block.
 *)
let block info () =
   send_view info block_code;
   poll_client info;
   dequeue info

(*
 * Receive some messages.
 *)
let receive info srchand blockp castp msg =
   let id = Endpt.string_of_id (endpt_of_handle srchand) in
   let code =
      match castp with
         Appl_handle.New.C ->
            cast_code
       | Appl_handle.New.S ->
            send_code
   in
      send_data info code id msg;
      poll_client info;
      dequeue info

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
let install info (ls, vs) handles =
   info.ens_view_state <- vs;
   info.ens_local_state <- ls;
   info.ens_handles <- handles;
   info.ens_strings <- List.map Endpt.string_of_id (Arrayf.to_list vs.view);
   let handlers =
      { New.block     = block info;
        New.receive   = receive info;
        New.heartbeat = heartbeat info;
        New.disable   = disable info
      }
   in
      send_view info new_view_code;
      poll_client info;
      dequeue info, handlers

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Options for the program.
 *)
let appname = ref "MP"

let set_appname name =
   appname := name

let no_anon_arg _ =
   raise (Invalid_argument "no anonymous arguments")

let spec =
   ["-A", Arg.String set_appname, "set the application name"]

(*
 * Create an Ensemble application.
 * We need:
 *    1. an endpoint name,
 *    2. an application name
 *    3. an application handler record
 *)
let main () =
   (*
    * Don't care about SIGPIPE
    *)
   (let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore in ());

   (*
    * Need total ordering of messages.
    * This also gives us local delivery.
    *)
   let properties = List.map Property.string_of_id Property.total in
   let properties = String.concat ":" properties in
   let _ = Arge.set_default Arge.properties properties in
   let _ = Arge.parse spec no_anon_arg "Ensemble outboard server" in

   let endpt_name = !appname ^ "APP" in
   let appl_name = !appname ^ "SERVER" in

   (*
    * Build the pipe.
    *)
   let pipe = Lm_mmap_pipe.create_server shared_dir in
   let server = Hsys.socket_of_file_descr (Lm_mmap_pipe.server_socket pipe) in

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
      { ens_pipe = pipe;
        ens_up_queue = [];
        ens_dn_queue = [];
        ens_server = server;
        ens_client = None;

        ens_alarm = Alarm.get ();
        ens_local_state = ls;
        ens_view_state = vs;
        ens_handles = Arrayf.empty;
        ens_strings = [];
        ens_heartbeat = Appl.async ls.async
      }
   in
   let _ = Alarm.add_sock_recv info.ens_alarm iovec_name server (Hsys.Handler0 (handle_server_event info)) in
   let interface =
      { New.heartbeat_rate = Time.of_int 600;
        New.install        = install info;
        New.exit           = exit info
      }
   in
   let _, interface = Appl_handle.New.f interface in

   (*
    * Initialize the protocol stack, using the interface and
    * view state chosen above.
    *)
   let interface = Appl_intf.New.debug_view appl_name interface in
      Appl.config_new interface (ls, vs);
      Appl.main_loop ()

(*
 * Main loop is just the Appl main loop,
 * but we print exceptions.
 *)
let _ =
   Unix.handle_unix_error main ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
