(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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


 open Basic
 open Refiner.Refiner.Term

 type 'a oidtable

 val make_oidtable	: unit -> 'a oidtable

 val insert	: 'a oidtable -> stamp -> object_id -> int -> 'a -> unit
 val delete	: 'a oidtable -> stamp -> object_id -> int -> unit

 val commit	: 'a oidtable -> stamp -> object_id -> int -> unit
 val undo	: 'a oidtable -> stamp -> object_id -> int -> unit

 val lookup	: 'a oidtable -> stamp -> object_id -> 'a

 val oidtable_unit_map	: 'a oidtable -> stamp -> (object_id -> 'a -> unit) -> unit
 val oidtable_map	: 'a oidtable -> stamp -> (object_id -> 'a -> 'b option) -> 'b list


