(*
 * This is the raw interface to the HTML window system.
 * Output is multiplexed over a common channel.
 * Channels are tracked with weak pointers, so that
 * they can be re-used if they are collected.
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

(*
 * open Dform_print
 * open Refiner
 * open Theory
 * open Mp_resource
 *)
open Printf
open Mp_debug

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * This is the raw channel.
 *)
type t =
   { mux_lock : Mutex.t;
     mux_in    : in_channel;
     mux_out   : out_channel;
     mux_sessions : session Weak.t;
     mux_menus : channel Weak.t;
     mux_goals : channel Weak.t;
     mux_rules : channel Weak.t;
     mux_subgoals : channel Weak.t;
     mux_url : string option
   }

(*
 * A session is just a port number.
 *)
and session =
   { session_fd : t;
     session_port : int
   }

(*
 * A channel has a name and a port.
 *)
and channel =
   { chan_fd : t;
     chan_host : string;
     chan_port : int;
     mutable chan_callback : callback
   }

(*
 * Input uses a callback mechanism.
 *)
and callback = channel -> string list -> unit

(*
 * A proof has three channels.
 *)
type proof_channels =
   { goal_channel : channel;
     rule_channel : channel;
     subgoals_channel : channel
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Maximum number of ports.
 *)
let max_ports = 256

(*
 * Names of the hosts.
 *)
let menu_host = "NuprlMenu"
let goal_host = "NuprlGoal"
let rule_host = "NuprlRule"
let subgoals_host = "NuprlSubgoals"
let default_host = "NuprlCommand"

(*
 * Output is multiplexed over a socket.
 *)
let create fd url =
   { mux_lock     = Mutex.create ();
     mux_in       = Unix.in_channel_of_descr fd;
     mux_out      = Unix.out_channel_of_descr fd;
     mux_sessions = Weak.create max_ports;
     mux_menus    = Weak.create max_ports;
     mux_goals    = Weak.create max_ports;
     mux_rules    = Weak.create max_ports;
     mux_subgoals = Weak.create max_ports;
     mux_url      = url
   }

(*
 * Lock access to the port.
 *)
let synchronize fd f x =
   Mutex.lock fd.mux_lock;
   try
      let result = f x in
         Mutex.unlock fd.mux_lock;
         result
   with
      exn ->
         Mutex.unlock fd.mux_lock;
         raise exn

(*
 * Create a session by looking through all the weak arrays.
 * Port numbers start from 2.
 *)
let create_session =
   let alloc info =
      let { mux_sessions = sessions;
            mux_menus = menus;
            mux_goals = goals;
            mux_rules = rules;
            mux_subgoals = subgoals
          } = info
      in
      let len = Weak.length sessions in
      let rec search i =
         if i = len then
            raise (Failure "Mux_channel.new_session: all sessions have been allocated");
         match Weak.get sessions i,
               Weak.get menus i,
               Weak.get goals i,
               Weak.get rules i,
               Weak.get subgoals i
         with
            None, None, None, None, None ->
               let session = { session_fd = info; session_port = i } in
                  Weak.set sessions i (Some session);
                  session
          | _ ->
               search (succ i)
      in
         search 2
   in
      (fun info -> synchronize info alloc info)

let new_session { session_fd = fd } =
   create_session fd

let id_of_session { session_port = id } =
   id

let url_of_channel { chan_fd = { mux_url = url } } =
   url

let host_of_channel { chan_host = host; chan_port = port } =
   host, port

let default_callback _ _ =
   ()

(*
 * The standard menu is port 1.
 *)
let standard_menu =
   let alloc { session_fd = info } =
      match Weak.get info.mux_menus 1 with
         Some channel ->
            channel
       | None ->
            let channel =
               { chan_fd = info;
                 chan_host = menu_host;
                 chan_port = 1;
                 chan_callback = default_callback
               }
            in
               Weak.set info.mux_menus 1 (Some channel);
               channel
   in
      (fun session -> synchronize session.session_fd alloc session)

(*
 * Allocate a new channel.
 *)
let new_channel info port weak host =
   match Weak.get weak port with
      Some channel ->
         channel
    | None ->
         let channel =
            { chan_fd = info;
              chan_host = host;
              chan_port = port;
              chan_callback = default_callback
            }
         in
            Weak.set weak port (Some channel);
            channel

(*
 * Allocation of new menus starts from port 2.
 *)
let new_menu =
   let alloc { session_fd = info; session_port = port } =
      new_channel info port info.mux_menus menu_host
   in
      (fun session -> synchronize session.session_fd alloc session)

(*
 * Allocate proof windows.
 *)
let new_proof =
   let alloc { session_fd = info; session_port = port } =
      { goal_channel = new_channel info port info.mux_goals goal_host;
        rule_channel = new_channel info port info.mux_rules rule_host;
        subgoals_channel = new_channel info port info.mux_subgoals subgoals_host
      }
   in
      (fun session -> synchronize session.session_fd alloc session)

(*
 * Set a callback.
 *)
let set_callback channel callback =
   channel.chan_callback <- callback

(*
 * Print a string to a channel.
 *)
let output_string =
   let output chan s =
      let { chan_fd = fd; chan_host = host; chan_port = port } = chan in
      let { mux_out = out } = fd in
         fprintf out "\027{%s;%d}" host port;
         output_string out s;
         fprintf out "\027{%s;1}" default_host;
         flush out
   in
      (fun chan s -> synchronize chan.chan_fd (output chan) s)

(*
 * Read input off the channel.
 * Input is formatted on lines, with the host and port first.
 *)
let input_callback info host port command =
   try
      let weak =
         if host = menu_host then
            info.mux_menus
         else if host = goal_host then
            info.mux_goals
         else if host = rule_host then
            info.mux_rules
         else if host = subgoals_host then
            info.mux_subgoals
         else
            raise Not_found
      in
         if port >= 0 && port < Weak.length weak then
            match Weak.get weak port with
               Some ({ chan_callback = callback } as channel) ->
                  callback channel command
             | None ->
                  ()
   with
      Not_found ->
         ()

let rec input_line info =
   let line = Pervasives.input_line info.mux_in in
      eprintf "Command: %s%t" line eflush;
      match String_util.parse_args line with
         host :: port :: command ->
            let port =
               try int_of_string port with
                  Failure "int_of_string" ->
                     0
            in
               if host = default_host then
                  let weak = info.mux_sessions in
                     if port >= 0 && port < Weak.length weak then
                        match Weak.get info.mux_sessions port with
                           Some session ->
                              session, command
                         | None ->
                              (* Ignore the command *)
                              input_line info
                     else
                        input_line info
               else
                  begin
                     input_callback info host port command;
                     input_line info
                  end
       | _ ->
            input_line info

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
