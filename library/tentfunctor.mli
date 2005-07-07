(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

module type TENT =
 sig

 open Basic

 type oid
 type 'a tent

 val new_tent	: unit -> 'a tent

 val tent_lookup 	: 'a tent -> stamp -> 'a

 (*
 (* not sure we want these in sig. *)
 val tent_contains_committed_oid_p	: tent -> stamp -> oid -> bool
 val tent_contains_oid_p		: tent -> stamp -> oid -> bool
 val tent_contains_pending_p		: tent -> stamp -> int -> bool
 val tent_contains_visible_p		: tent -> stamp -> bool
 *)

 val tent_insert 	: 'a tent -> stamp -> int -> oid -> 'a -> unit
 val tent_delete 	: 'a tent -> stamp -> int -> oid -> unit
 val tent_undo		: 'a tent -> stamp -> int -> (oid * 'a option)
 val tent_commit	: 'a tent -> stamp -> int -> unit

 val tent_collect 	: 'a tent -> stamp list -> unit

 end

module type OID_TYPE =
 sig
  type t
  val equal: t -> t -> bool
 end

module Tent : functor (Oid : OID_TYPE) -> (TENT with type oid = Oid.t)

