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


open Refiner.Refiner.Term


type link

val connect_with_callback	: string (* remote hostname *) -> int (* remote socket *)
				-> int (* local socket *) -> link (*not complete*)
val connect_callback 		: link (*not complete*) -> link (*complete*)
				
val disconnect		: link -> unit

val send		: link -> term -> unit

(*blocks on read*)
val recv		: link -> term

(*returns None if nothing on input channel*)
 val recv_nohang		: link -> term option


(*nuprl5 terms*)

val iconnect_term	: int -> string -> term

val idisconnect_term	: bool (* error-p *) -> term


(*testing purposes*)
val cautious_in		: Unix.file_descr ref
val cautious_out	: Unix.file_descr ref
val cautious_socket	: Unix.file_descr ref

