(* This file is an interface for hash table with GC feature
 * 
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

   val create : int -> int -> ('key -> int) -> ('key -> 'key -> bool) -> ('key, 'value) t

   val hash : ('key, 'value) t -> 'key -> ('key, 'value) hash

   val seek : ('key, 'value) t -> ('key, 'value) hash  -> 'key -> 'value option

   val insert : ('key, 'value) t -> ('key, 'value) hash -> 'key -> 'value -> unit

   val iter : ('key * 'value -> unit) -> ('key, 'value) t -> unit

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
