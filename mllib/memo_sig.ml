(*
 * Generic memoize function.
 * It looks just like a function.
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
 * Copyright (C) 1998 Jason Hickey, Cornell University
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

module type MemoSig =
sig
   (*
    * This is a memoizer function.
    * Types:
    *   'param:        uninterpreted argument (usually used for recursion on memo functions).
    *   'arg:          the type of arguments to the memo function
    *   'header:       intermediate values that are compared with an equality function.
    *           Usually, these intermediate values have the same structure as the
    *           argument, but subfields have been memo-ized.
    *   'weak_header:  this may be the same as 'header.
    *           In weak versions of the table, the weak_header will be
    *           designed not to include references to values that
    *           are allowed to be collected during GC.
    *   'result: result value of the function.
    *)
   type ('param, 'arg, 'header, 'weak_header, 'result) t

   (*
    * This creates a memoizer, which acts like a function.
    * The function can be applied using the apply function.
    *
    * The objective is that (apply (create f g eq)) looks just like
    * f, but it is more efficient because it remembers previous
    * values that have been computed.
    *)
   val create :
      int ->                                            (* Initial size of the table *)
      int ->                                            (* For weak tables, this is a GC parameter *)
      string ->                                         (* Name of this table for debugging *)
      ('param -> 'arg -> 'header) ->                    (* Create the actual argument *)
      ('param -> 'header -> 'weak_header) ->            (* Delete all collectable values *)
      ('weak_header -> 'weak_header -> bool) ->         (* Compare arguments *)
      ('param -> 'header -> 'result) ->                 (* Compute the value of the function *)
      ('param, 'arg, 'header, 'weak_header, 'result) t

   (*
    * Apply the function to the argument and remember the result.
    *)
   val apply : ('param, 'arg, 'header, 'weak_header, 'result) t -> 'param -> 'arg -> 'result
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
