(*
 * Bounded buffers for the shell.
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
 * A simple bounded buffer.
 *)
module type LineBufferSig =
sig
   type 'a t

   val create      : unit -> 'a t
   val clone       : 'a t -> ('a -> 'b) -> 'b t
   val add         : 'a t -> 'a -> unit
   val iter        : ('a -> unit) -> 'a t -> unit
   val fold        : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val last        : 'a t -> 'a option
   val remove_last : 'a t -> unit
   val length      : 'a t -> int
end

(*
 * A bounded table.
 *)
module type LineTableSig =
sig
   type 'a t

   val empty  : 'a t
   val mem    : 'a t -> string -> bool
   val add    : 'a t -> string -> 'a -> 'a t
   val iter   : (string -> 'a -> unit) -> 'a t -> unit
   val fold   : ('a -> string -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

(*
 * The implementation.
 *)
module LineBuffer : LineBufferSig
module LineTable : LineTableSig

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
