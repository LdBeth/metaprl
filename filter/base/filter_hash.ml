(*
 * Hash functions on caml expressions.
 * We just want to remove location info.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Lm_debug

open Filter_util

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_hash%t"

(*
 * Basic hash functions.
 *)
let hash index i =
   (index lsl 4) lxor (index lsr 4) lxor i

let hash_max x =
   Hashtbl.hash_param max_int max_int x

(*
 * We will relocate everything to 0
 *)
let loc = dummy_loc
let reloc = fun _ -> loc

(*
 * Compute a hash value from the struct.
 *)
let hash_expr index expr =
   hash index (hash_max (Pcaml.expr_reloc reloc Lexing.dummy_pos expr))

let hash_patt index patt =
   hash index (hash_max (Pcaml.patt_reloc reloc Lexing.dummy_pos patt))

let hash_type index ctyp =
   hash_expr index <:expr< ( (assert False) : $ctyp$ ) >>
   (* hash index (hash_max (Pcaml.ctyp_reloc reloc Lexing.dummy_pos ctyp)) *)

let hash_str_item index item =
   hash index (hash_max (Pcaml.str_item_reloc reloc Lexing.dummy_pos item))

let hash_sig_item index item =
   hash index (hash_max (Pcaml.sig_item_reloc reloc Lexing.dummy_pos item))

let hash_module_type index mt =
   hash index (hash_max (Pcaml.module_type_reloc reloc Lexing.dummy_pos mt))

let hash_module_expr index me =
   hash index (hash_max (Pcaml.module_expr_reloc reloc Lexing.dummy_pos me))

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
