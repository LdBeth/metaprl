(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
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
 * Modified By: Aleksey Nogin: <nogin@cs.caltech.edu>
 *)

open Filter_type
open Filter_summary_type

(*
 * Build a cache with a particular proof type.
 *)
module MakeCaches (Convert : ConvertProofSig) :
   CachesSig with type t = Convert.t and type cooked = Convert.cooked

(*
 * Force the output format.
 *)
val set_raw : unit -> unit
val set_file : unit -> unit
val set_lib : unit -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
