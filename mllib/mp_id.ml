(*
 * Unique identifiers.
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
 * The identifier is just a string.
 *)
type t = string

(*
 * The identifier contains the:
 *    1. Machine name
 *    2. Process identifier
 *    3. A unique number in this process
 *)
let id_lock = Mutex.create ()
let id_value = ref 0

let create () =
   let hostname =
      let name = Unix.gethostname () in
         try
            let host = Unix.gethostbyname name in
            let addrs = host.Unix.h_addr_list in
               if addrs = [||] then
                  host.Unix.h_name
               else
                  String.concat host.Unix.h_name (Array.to_list (Array.map Unix.string_of_inet_addr addrs))
         with
            Not_found ->
               name
   in
   let pid = Unix.getpid () in
   let index =
      Mutex.lock id_lock;
      let index = !id_value in
         incr id_value;
         Mutex.unlock id_lock;
         index
   in
      Printf.sprintf "%s:%d:%d" hostname pid index

(*
 * Get a printable representation.
 *)
let string_of_id x =
   x

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
