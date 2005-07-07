(*
 * This file is an interface for hash table with GC feature
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

module type HashWithGCSig =
sig
   (*
    * Hash-code type
    *)
   type ('key, 'value) hash

   (*
    * Hash-table type
    *)
   type ('key, 'value) t

   exception GC_Not_Finished
   exception GC_Not_Started
   exception Expand_During_GC

   (*
    * Create a new table.
    *)
   val create :
      int ->                            (* default size of the table *)
      int ->                            (* gc_critical (jyh: don't know what this really is *)
      ('key -> int) ->                  (* hash function *)
      ('key -> 'key -> bool) ->         (* comparison function on arguments *)
      ('key, 'value) t                  (* hash table *)

   (*
    * Get a hash index.
    * This returns a key into the table that can be used for
    * lookup later.
    *)
   val hash : ('key, 'value) t -> 'key -> ('key, 'value) hash

   (*
    * Get the value associated with the hash key.
    *)
   val seek : ('key, 'value) t -> ('key, 'value) hash  -> 'key -> 'value option

   (*
    * Add a new value to the table.
    *)
   val insert : ('key, 'value) t -> ('key, 'value) hash -> 'key -> 'value -> unit

   (*
    * Apply an function to all the entries in the
    * table in some arbitrary order.
    *)
   val iter : ('key * 'value -> unit) -> ('key, 'value) t -> unit

   (*
    * GC sequence:
    *    gc_start: notify that GC is being started
    *    is_gc: has GC been started without calling gc_iter?
    *    gc_iter: find the next item marked by the
    *       predicate, remove it from the table and return it.
    *)
   val gc_start : ('key, 'value) t -> unit

   val is_gc : ('key, 'value) t -> bool

   val gc_iter : (('key * 'value) -> bool) -> ('key, 'value) t -> ('key * 'value) option
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
