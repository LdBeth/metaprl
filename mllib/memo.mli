(*
 * Generic memoize function.
 * It looks just like a function.
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

type ('a, 'b, 'c, 'd) t

(*
 * This is a memoizer function.
 * Types:
 *   'a: the type of arguments that are not remembered
 *   'b: the type of arguments that are remembered by pointer equality
 *   'c: arguments to be remembered, computed from 'a
 *   'd: values to be remembered
 *
 * create arguments:
 * arg0: compute the actual argument
 *   however, if we ever lookup a value that is pointer
 *   equal to 'a that we have remembered, we return the remembered
 *   value directly.
 * arg1: compute the value of the function
 *   'b is the argument, and if we have ever computed this value
 *   before according to arg2, then we return the previous value.
 * arg2: compare values in 'b
 *   this function may be somewhat finer that normal equality,
 *   but it cannot be coarser
 *
 * This creates a memoizer, which acts like a function.
 * The function can be applied using the apply function.
 *
 * The objective is that (apply (create f g eq)) looks just like
 * (fun arg x -> g arg (f arg x)), but it is more efficient because
 * it remembers the arguments x (by pointer equality), and values
 * of f arg x and g arg (f arg x) (by the eq function).
 *)
val create :
   ('a -> 'b -> 'c) ->          (* Create the actual argument *)
   ('a -> 'c -> 'd) ->          (* Compute the value of the function *)
   ('c -> 'c -> bool) ->        (* Compare arguments *)
   ('a, 'b, 'c, 'd) t
val apply : ('a, 'b, 'c, 'd) t -> 'a -> 'b -> 'd

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
