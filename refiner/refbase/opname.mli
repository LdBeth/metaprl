(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

type atom
type opname

(* Constructors *)
val nil_opname     : opname
val mk_opname      : string -> opname -> opname
val make_opname    : string list -> opname

(* Atoms are always equal iff they are pointer equal *)
val intern : opname -> atom

(* Opnames should always be compared with this equality *)
val eq : opname -> opname -> bool

(* Opnames can be normalized when they are unmarshaled *)
val normalize_opname : opname -> opname

(* Destructors *)
val dst_opname : opname -> string * opname
val dest_opname : opname -> string list
val string_of_opname : opname -> string

(*
 * A few "special" opnames.
 * These must be common to all the term modules, so they are defined here.
 *)
val var_opname     : opname
val context_opname : opname
val xperv          : opname
val sequent_opname : opname
val xnil_opname    : opname
val xcons_opname   : opname
val xconcl_opname  : opname

(*
 * Debugging.
 *)
val debug_opname : bool ref

(*
 * This table provides associations between symbols
 * and values.
 *)
val compare : opname -> opname -> int

module OpnameSet    : Lm_set_sig.LmSet with type elt = opname
module OpnameTable  : Lm_map_sig.LmMap with type key = opname
module OpnameMTable : Lm_map_sig.LmMapList with type key = opname

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
