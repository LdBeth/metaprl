(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

open Printf
open Lm_debug

let _ =
   show_loading "Loading SocketIo%t"


let open_client port host =
  let {Unix.h_name = name; Unix.h_aliases = a; Unix.h_addrtype = atype; Unix.h_addr_list = l} =
    Unix.gethostbyname host
  in
  Unix.open_connection (Unix.ADDR_INET (l.(0), port))

(*
let open_client port host = Unix.open_connection (Unix.ADDR_INET(Unix.inet_addr_of_string "128.84.254.214", port))
*)
let close_client (in_channel, out_channel) =
	 (close_in  in_channel ;
	 close_out out_channel )

let make_socket port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 and
      {Unix.h_name = name; Unix.h_aliases = a; Unix.h_addrtype = atype; Unix.h_addr_list = l} =
    Unix.gethostbyname (Unix.gethostname ())
  in
  let address = Unix.ADDR_INET (l.(0), port)
  in
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

let destroy_socket fd = Unix.shutdown fd Unix.SHUTDOWN_ALL ;;

let accept_client fd =
  let (fd2, addr) = Unix.accept fd in
  fd2
