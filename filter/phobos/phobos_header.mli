(*
 * Utilities for headers.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2003 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

type digest

type header

(* Version *)
val version_string : string

(* Digests *)
val create_digest : string -> digest

(* Headers *)
val version_of_header : header -> string
val digest_of_header : header -> digest
val timestamp_of_header : header -> float
val sizestamp_of_header : header -> int

val create_header : digest -> float -> int -> header
