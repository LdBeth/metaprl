(*
 * Utilities on tables.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 * jyh@cs.cornell.edu
 *)

open Set_sig

module MakeTable (Create : TableCreateSig) (Base : TableBaseSig) =
struct
   type elt = Base.elt
   type data = Base.data
   type t = (elt, data) Create.t

   (*
    * Get the methods.
    *)
   let methods = Create.create Base.print Base.compare Base.append

   (*
    * Now project them.
    *)
   let empty = methods.empty
   let add = methods.add
   let union = methods.union
   let mem = methods.mem
   let find = methods.find
   let find_all = methods.find_all
   let remove = methods.remove
   let iter = methods.iter
   let map = methods.map
   let print = methods.print
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
