(*
 * Use Obj marshaler to convert between strings and terms.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Opname
open Refiner_io

open Term_io

(*
 * Header.
 *)
let magic = "MP-OCaml terms:"
let magic_len = String.length magic

let check_magic s =
   if String.length s < magic_len || String.sub s 0 magic_len <> magic then
      raise (Invalid_argument "Ml_term.get_string")

(*
 * Convert to a string.
 *)
let string_of_term_lists ts mts opnames nums =
   let terms =
      List.map denormalize_term ts,
      List.map denormalize_meta_term mts,
      opnames,
      nums
   in
      magic ^ Marshal.to_string terms []

(*
 * Convert from a string.
 *)
let term_arrays_of_string s =
   check_magic s;
   let ts, mts, opnames, nums =
      (Marshal.from_string s magic_len : Refiner_io.TermType.term list * Refiner_io.TermType.meta_term list * opname list * Lm_num.num list) in
      Array.of_list (List.map normalize_term ts),
      Array.of_list (List.map normalize_meta_term mts),
      Array.of_list (List.map normalize_opname opnames),
      Array.of_list nums

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
