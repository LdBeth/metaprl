(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
 * Authors: Lori Lorigo, Richard Eaton
 *)
open Lm_debug

let _ =
   show_loading "Loading SocketIo%t"

let open_client port host =
  let addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
   Unix.open_connection (Unix.ADDR_INET (addr, port))

(*
let open_client port host = Unix.open_connection (Unix.ADDR_INET(Unix.inet_addr_of_string "128.84.254.214", port))
*)
let close_client (in_channel, out_channel) =
	 (close_in  in_channel ;
	 close_out out_channel )

let make_socket port =
   let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0  in
   let addr = (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0) in
   let address = Unix.ADDR_INET (addr, port) in
      Unix.bind fd address ;
      Unix.listen fd 1;
      fd

(*
let make_socket port host =
   let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
   in
   let address = Unix.ADDR_INET (Unix.inet_addr_of_string "128.84.254.214", port)
   in Unix.bind fd address ;
   Unix.listen fd 1;
   fd;;
*)

let destroy_socket fd =
   Unix.shutdown fd Unix.SHUTDOWN_ALL

let accept_client fd =
   fst (Unix.accept fd)
