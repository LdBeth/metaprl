(*
 * Extra functions on hashtables.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *)

open Hashtbl

(*
 * Convert to an association list.
 * Order is random, but the is only one entry
 * for each key.
 *)
let to_list tbl =
   let l = ref [] in
      Hashtbl.iter (fun key value ->
            if not (List.mem_assoc key !l) then
               l := (key, value) :: !l) tbl;
      !l

(*
 * Add a list to a hashtbl.
 *)
let add_list tbl l =
   List.iter (fun (key, value) -> Hashtbl.add tbl key value) l

(*
 * Add all the entries from the second table.
 * The original entries are removed.
 *)
let add_hashtbl dst src =
   Hashtbl.iter (fun key value ->
         (try Hashtbl.remove dst key with
             Not_found ->
                ());
         Hashtbl.add dst key value) src

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
