(*
 * Some utilities for the shell.
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
 * Options to the "ls" command.
 *)
type ls_option =
   LsRewrites
 | LsRules
 | LsUnjustified
 | LsDisplay
 | LsDefault
 | LsFormal
 | LsInformal
 | LsParent
 | LsAll
 | LsNone

   (*
    * Browser-only modes:
    *   LsHandles: display handles to allow selection of arbitrary subterms.
    *)
 | LsHandles
 | LsExternalEditor

module LsOptionSet : Lm_set_sig.LmSet with type elt = ls_option

val string_of_ls_options : LsOptionSet.t -> string
val ls_options_of_string : string -> LsOptionSet.t
val ls_options_add : LsOptionSet.t -> string -> LsOptionSet.t
val ls_options_clear : LsOptionSet.t -> string -> LsOptionSet.t
val ls_options_default : LsOptionSet.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
