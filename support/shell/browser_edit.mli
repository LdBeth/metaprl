(*
 * Editing commands.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

(*
 * Complete edit info.
 *)
type edit_info =
   { edit_point    : int;
     edit_modified : bool;
     edit_new      : bool;
     edit_rootname : string
   }

(*
 * Name of the edit file.
 *)
val editname : string -> string

(*
 * Get the edit info.
 *)
val get_edit_info : string -> edit_info

(*
 * Filename translation.
 *)
val proxyedit_of_filename : string -> string
val filename_of_proxyedit : string -> string

val save_file   : string -> bool -> int -> string -> bool
val backup_file : string -> bool -> int -> string -> bool
val cancel_file : string -> bool -> int -> string -> bool

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
