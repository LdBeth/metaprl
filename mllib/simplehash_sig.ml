(* This file is an interface for simple hash table
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

module type SimpleHashSig =
sig

(*
 * Hash-table type
 *)
    type ('key, 'value) t

    val create : int -> ('key -> 'key -> bool) -> ('key, 'value) t

    val seek : ('key, 'value) t -> int -> 'key -> 'value option

    val insert : ('key, 'value) t -> int -> 'key -> 'value -> unit

    val extr : ('key, 'value) t -> ( ('key * 'value) list array * int )

    val iter : ('key * 'value -> unit) -> ('key, 'value) t -> unit

    val gc : ('key -> int) -> ('key * 'value -> bool) -> ('key, 'value) t -> unit

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "simplehashtbl, bi_memo, weak_memo"
 * End:
 * -*-
 *)
