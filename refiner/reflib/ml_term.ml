(*
 * Use Obj marshaler to convert between strings and terms.
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
open Refiner_io

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Term_io

(*
 * Header.
 *)
let magic = "MP-Caml3.02 term:"
let magic_len = String.length magic

(*
 * Convert to a string.
 *)
let string_of_term t =
   let s = Marshal.to_string (denormalize_term t) [] in
      magic ^ s

(*
 * Convert from a string.
 *)
let term_of_string s =
   let len = String.length s in
      if len < magic_len || String.sub s 0 magic_len <> magic then
         raise (Invalid_argument "term_of_string");
      let s = String.sub s magic_len (len - magic_len) in
      let t = (Marshal.from_string s 0 : Refiner_io.TermType.term) in
         normalize_term t

(*
 * Convert to a string.
 *)
let string_of_mterm t =
   let s = Marshal.to_string (denormalize_meta_term t) [] in
      magic ^ s

(*
 * Convert from a string.
 *)
let mterm_of_string s =
   let len = String.length s in
      if len < magic_len || String.sub s 0 magic_len <> magic then
         raise (Invalid_argument "mterm_of_string");
      let s = String.sub s magic_len (len - magic_len) in
      let t = (Marshal.from_string s 0 : Refiner_io.TermType.meta_term) in
         normalize_meta_term t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
