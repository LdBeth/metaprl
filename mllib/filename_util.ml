(*
 * Utilities on filenames.
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

open Printf
open Mp_debug
(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filename_util%t" eflush

(*
 * Pathname separator chars.
 *)
let separators = "/\\"

(*
 * Split a pathname.
 *)
let split = String_util.split_set separators

(*
 * Get the Last part of the filename.
 *)
let tail s =
   try
      let index = String_util.rindex_set s separators in
         String_util.sub "Filename_util.tail" s index (String.length s - index)
   with
      Not_found ->
         s

let head s =
   try
      let index = String_util.rindex_set s separators in
         String_util.sub "Filename_util.head" s 0 index
   with
      Not_found ->
         s

let root s =
   try
      let index = String.rindex s '.' in
         String_util.sub "Failename_util.root" s 0 index
   with
      Not_found ->
         s

let suffix s =
   try
      let index = String.rindex s '.' in
         String_util.sub "Filename_util.suffix"  s index (String.length s - index)
   with
      Not_found ->
         ""

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
