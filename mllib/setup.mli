(*
 * Values for MetaPRL environment variables.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2004 MetaPRL Group, Caltech
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
 * Authors: Jason Hickey <jyh@cs.caltech.edu>
 *          Aleksey Nogin <nogin@cs.caltech.edu>
 *)

val environ_prefix : string  (* MP *)
val root : unit -> string    (* $(MP_ROOT); must be defined *)
val lib : unit -> string     (* $(MPLIB) - when defined, or $(MP_ROOT)/lib - when directory exists *)

(*
 * $(HOME)/.metaprl directory (uses C:\metaprl or /tmp/metaprl-<uid>, when
 * $(HOME) is not defined)
 *
 * The first time this function is called, it will make sure that
 * the directory exists and is readable and writable
 *)
val home : unit -> string    (* $(HOME)/.metaprl *)

(*
 * Read the editor from the $HOME/.metaprl/editor file.
 *)
val editor : unit -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
