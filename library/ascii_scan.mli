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

open Stream
open Mp_num

  type scanner		

  val isemicolon : int
  val icolon	: int
  val icomma	: int
  val idot	: int
  val ibar	: int
  val irparen	: int
  val ilparen	: int
  val ircurly	: int
  val ilcurly	: int
  val irsquare	: int
  val ilsquare	: int

  val make_scanner	: string (* escape *) -> string (* whitespace *) -> char t -> scanner

  (*
   *	scan_next : advances scanner.
   *	scan_bump : advances scanner to next byte.
   *
   *	Difference is that if next byte is escape then scan_next sets escape
   *	bit and advances again.
   *)
  val scan_next		: scanner -> unit
  val scan_bump		: scanner -> unit

  val scan_at_eof_p	: scanner -> bool


  (* twould be better if scanner were last arg in following funcs to allow for
     partial ap without lambdas
     EG: let scan_lparen = scan_char '(
     not let scan_lparen s = scan_char s '('
   *)

  (* should fail if cur byte is not char *)
  val scan_cur_char	: scanner -> char
  val scan_at_char_p	: scanner -> char -> bool
  val scan_char		: scanner -> char -> unit

  (* should fail if cur byte is not byte *)
  val scan_byte		: scanner -> int (* byte *) -> unit
  val scan_cur_byte	: scanner -> int
  val scan_at_byte_p	: scanner -> int -> bool

  val scan_at_digit_p	: scanner -> bool
  val scan_whitespace	: scanner -> unit

  val scan_string	: scanner -> string
  val scan_num		: scanner -> num

  (* twould probably be better if item scanner took scanner as arg and was first
     arg to scan delimited list to allow definition of list scanners without having scanner
     EG: let scan_foo_list = scan_char_delimited_list (function s -> scan_foo s) '(' ';' ')'
   *)
  val scan_char_delimited_list	: scanner
					 -> (unit -> 'a)	(* item scanner		*)
					 -> char		(* left delimiter	*)
					 -> char		(* right delimiter	*)
					 -> char		(* seperator		*)
					 -> 'a list

  val scan_delimited_list	: scanner
					 -> (unit -> 'a)	(* item scanner		*)
					 -> int			(* left delimiter	*)
					 -> int			(* right delimiter	*)
					 -> int			(* seperator		*)
					 -> 'a list


