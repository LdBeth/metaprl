(*
 * Convert an application interface to use the closure
 * marshaler.  This provides the same interface as
 * Appl_intf.New.full.
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
open Ensemble.Appl_intf

open Printf

(*
 * Debug variables.
 *)
let debug_marshal =
   create_debug (**)
      { debug_name = "ensemble";
        debug_description = "Display MP Ensemble Application actions";
        debug_value = false
      }

(************************************************************************
 * MARSHALING                                                           *
 ************************************************************************)

(*
 * Ensemble doesn't export this.
 *)
let ceil_word word =
   (word + 3) land (lnot 3)

(*
 * For now we do a copy of the marshaled data,
 * from a common buffer.
 *)
let marshal_buf = ref (String.create 65536)

(*
 * When marshaling, loop until the marshal buf is big enough.
 *)
let rec marshal x =
   let buf = !marshal_buf in
   let len = String.length buf in
      try
         let size = Marshal.to_buffer buf 0 len x [Marshal.Closures] in
         let size = ceil_word size in
         let vecl = Mbuf.allocl "MPSERVER:marshal" buf 0 size in
            if !debug_marshal then
               begin
                  lock_printer ();
                  eprintf "Ensemble_queue.marshal: object size: %d%t" size eflush;
                  unlock_printer ()
               end;
            vecl

      with
         Failure s ->
            lock_printer ();
            eprintf "Ensemble_queue.marshal: %s\n\tincreasing buffer size to %d%t" s (2 * len) eflush;
            unlock_printer ();
            marshal_buf := String.create (2 * len);
            marshal x

(*
 * When unmarshaling, we have to flatten the buffer.
 *)
let unmarshal vecl =
   let iov = Iovecl.flatten "MPSERVER:unmarshal" vecl in
   let read buf off len =
      Marshal.from_string buf off
   in
      Iovec.read "MPSERVER:unmarshal" iov read

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(*
 * Map some functions over the action array.
 *)
let action_map castf sendf = function
   Cast m ->
      Cast (castf m)
 | Send (d, m) ->
      Send (d, (sendf m))
 | Control o ->
      Control o

let action_array_map castf sendf =
   Array.map (action_map castf sendf)

let marshal_actions actions =
   action_array_map marshal marshal actions

(*
 * Wrap the application interface.
 *)
let full interface =
   (*
    * Wrap all the handlers when the view is installed.
    *)
   let install view_info =
      let actions, handlers = interface.New.install view_info in

      (*
       * Receive function calls unmarshaler on the message,
       * and marshals the resulting actions.
       *)
      let receive src blockp castp msg =
         marshal_actions (handlers.New.receive src blockp castp (unmarshal msg))
      in

      (*
       * For The other two handlers,
       * just marshal the returned actions.
       *)
      let block () =
         marshal_actions (handlers.New.block ())
      in

      let heartbeat time =
         marshal_actions (handlers.New.heartbeat time)
      in

      let handlers =
         { New.flow_block = handlers.New.flow_block;
           New.heartbeat = heartbeat;
           New.receive = receive;
           New.block = block;
           New.disable = handlers.New.disable
         }
      in
         marshal_actions actions, handlers
   in
      { New.heartbeat_rate = interface.New.heartbeat_rate;
        New.install = install;
        New.exit = interface.New.exit
      }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
