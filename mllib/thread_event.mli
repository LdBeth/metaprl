(*
 * This is a reimplementation of the standard OCaml Event
 * module.
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

(*
 * Events are delivered along a channel.
 *)
type 'a channel

val new_channel : unit -> 'a channel

(*
 * Events are the objects sent along the channel.
 * Events are higher order values that can be combined
 * before the actual communication.
 *)
type 'a event

(*
 * Event is always ready for synchronization.
 *)
val always : 'a -> 'a event

(*
 * Event that, when synchronized, sends a value along the channel.
 *)
val send : 'a channel -> 'a -> unit event

(*
 * When synchornized, receive gets a sent value along the channel.
 *)
val receive : 'a channel -> 'a event

(*
 * Wrap a function around the value returned by the event.
 *)
val wrap : 'a event -> ('a -> 'b) -> 'b event

(*
 * Choose among a list of events.
 *)
val choose : 'a event list -> 'a event

(*
 * Sequence a collection of events.
 * Wait for an event from the first to the last.
 * Once the list becomes empty, this
 * event will never happen.
 *)
val sequence : 'a event list -> 'a event

(*
 * Same as sequence, but once the list becomes a singleton,
 * repeat events from the singleton.
 *)
val sequence_final : 'a event list -> 'a event

(*
 * Sync on one of the events.
 *)
val select : int -> 'a event list -> 'a

(*
 * If the event can be syncronized immediately,
 * sync it and return the value.
 *)
val poll : 'a event -> 'a option

(*
 * Complete the action described by the event.
 *)
val sync : int -> 'a event -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
