(*
 * Utilities for the rewriter.
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

module type RewriteUtilSig =
sig
   type term
   type rstack

   (*
    * Precomputed exceptions.
    *)
   val redex_params_iter_exn : exn

   (*
    * List operations that throw refiner errors.
    *)
   val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
   val rev_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

   (*
    * Membership in the stack.
    *)
   val rstack_mem : string -> rstack list -> bool
   val rstack_so_mem : string -> rstack list -> bool
   val rstack_pattern_mem : string -> rstack list -> bool
   val rstack_fo_mem : string -> rstack list -> bool
   val rstack_p_mem : string -> rstack list -> bool
   val rstack_c_mem : string -> rstack list -> bool

   val array_rstack_mem : string -> rstack array -> bool
   val array_rstack_so_mem : string -> rstack array -> bool
   val array_rstack_fo_mem : string -> rstack array -> bool
   val array_rstack_p_mem : string -> rstack array -> bool
   val array_rstack_c_mem : string -> rstack array -> bool

   (*
    * Location in the stack.
    *)
   val rstack_index : string -> rstack list -> int
   val rstack_so_index : string -> rstack list -> int
   val rstack_fo_index : string -> rstack list -> int
   val rstack_p_index : string -> rstack list -> int
   val rstack_c_index : string -> rstack list -> int

   val array_rstack_index : string -> rstack array -> int
   val array_rstack_so_index : string -> rstack array -> int
   val array_rstack_fo_index : string -> rstack array -> int
   val array_rstack_p_index : string -> rstack array -> int
   val array_rstack_c_index : string -> rstack array -> int

   (*
    * Consistency in the stack.
    *)
   val rstack_check_arity : string -> int -> rstack list -> unit

   (*
    * Stack operations.
    *)
   val rstack_upgrade : string -> rstack list -> rstack list

   (*
    * Assoc.
    *)
   val var_index : (string * int) list -> term -> int
   val svar_index : (string * int) list -> string -> int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
