(*
 * Simplfiied socket interface.
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

open Printf

open Mp_debug

let debug_inet =
   create_debug (**)
      { debug_name = "inet";
        debug_description = "Display internet socket operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A server socket.
 *)
type server = Unix.file_descr

(*
 * A client socket.
 * Include the remote port, and the remote address.
 *)
type client = Unix.file_descr * Unix.inet_addr * int

(************************************************************************
 * IMPLMENTATION                                                        *
 ************************************************************************)

(*
 * Get the string name of an addr.
 *)
let string_of_inet_addr addr =
   try (Unix.gethostbyaddr addr).Unix.h_name with
      Not_found ->
         Unix.string_of_inet_addr addr

(*
 * Open a server socket bound to a particular address.
 *)
let serve port =
   let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   let port =
      match port with
         Some port ->
            Unix.setsockopt fd Unix.SO_REUSEADDR true;
            port
       | None ->
            0
   in
      Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
      Unix.listen fd 10;
      fd

(*
 * Close the server.
 *)
let close_server = Unix.close

(*
 * Get local server info.
 *)
let get_server_host fd =
   match Unix.getsockname fd with
      Unix.ADDR_INET (addr, port) ->
         let host =
            if addr = Unix.inet_addr_any then
               Unix.gethostname ()
            else
               string_of_inet_addr addr
         in
            host, port
    | Unix.ADDR_UNIX _ ->
         raise (Failure "Mp_inet.get_client_host")


(*
 * Accept a connection on the server socket.
 *)
let accept fd =
   let fd, addr = Unix.accept fd in
      match addr with
         Unix.ADDR_INET (addr, port) ->
            fd, addr, port
       | Unix.ADDR_UNIX _ ->
            Unix.close fd;
            raise (Invalid_argument "Mp_inet.accept_connect")

(*
 * Open a client connection.
 *)
let connect machine port =
   let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   let addr =
      try
         let addr = Unix.inet_addr_of_string machine in
            Unix.connect fd (Unix.ADDR_INET (addr, port));
            addr
      with
         Failure _ ->
            let addrs = (Unix.gethostbyname machine).Unix.h_addr_list in
            let len = Array.length addrs in
            let rec search exn i =
               if i = len then
                  raise exn
               else
                  try
                     let addr = addrs.(i) in
                        Unix.connect fd (Unix.ADDR_INET (addr, port));
                        addr
                  with
                     exn ->
                        search exn (succ i)
            in
            let exn = Unix.Unix_error (Unix.EAFNOSUPPORT, "connect", machine) in
               search exn 0
   in
      fd, addr, port

(*
 * Get the name of the peer machine.
 *)
let file_descr_of_client (fd, _, _) =
   fd

let get_client_peer (_, addr, port) =
   string_of_inet_addr addr, port

let get_client_host (fd, _, _) =
   match Unix.getsockname fd with
      Unix.ADDR_INET (addr, port) ->
         string_of_inet_addr addr, port
    | Unix.ADDR_UNIX _ ->
         raise (Failure "Mp_inet.get_client_host")

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
