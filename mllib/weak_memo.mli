(* This file is an interface for memoize function based on weak
 * array of results
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 *)

open Memo_sig
open Weak_memo_sig

(*
 * Data used for garbage collection
 *)
type gc_info = Weak_memo_sig.gc_info

(*
 * Empty instance of gc_info
 *)
val empty_gci : gc_info

module WeakMemo (Hash : Hash_with_gc_sig.HashWithGCSig) : WeakMemoSig

module MakeMemo (Hash : Hash_with_gc_sig.HashWithGCSig) : WeakMemoSig

module TheWeakMemo : WeakMemoSig

module TheMemo : WeakMemoSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
