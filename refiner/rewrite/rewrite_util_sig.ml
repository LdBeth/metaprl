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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_symbol

open Term_shape_sig

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
   val iter2_3 : ('c -> 'd -> 'e -> 'a -> 'b -> unit) -> 'c -> 'd -> 'e -> 'a list -> 'b list -> unit
   val iter2_1 : ('c -> 'a -> 'b -> unit) -> 'c -> 'a list -> 'b list -> unit
   val rev_iter2_3 : ('c -> 'd -> 'e -> 'a -> 'b -> unit) -> 'c -> 'd -> 'e -> 'a list -> 'b list -> unit

   (*
    * Membership in the stack.
    *)
   val rstack_var : rstack -> var
   val rstack_mem : var -> rstack list -> bool
   val rstack_so_mem : var -> rstack list -> bool
   val rstack_pattern_mem : var -> rstack list -> bool
   val rstack_freefo_mem : var -> rstack list -> bool
   val rstack_fo_mem : var -> rstack list -> bool
   val rstack_p_mem : shape_param -> var -> rstack list -> bool
   val rstack_c_mem : var -> rstack list -> bool

   val array_rstack_mem : var -> rstack array -> bool
   val array_rstack_so_mem : var -> rstack array -> bool
   val array_rstack_freefo_mem : var -> rstack array -> bool
   val array_rstack_fo_mem : var -> rstack array -> bool
   val array_rstack_p_mem : shape_param -> var -> rstack array -> bool
   val array_rstack_c_mem : var -> rstack array -> bool

   (*
    * Location in the stack.
    *)
   val rstack_index : var -> rstack list -> int
   val rstack_so_index : var -> rstack list -> int
   val rstack_freefo_index : var -> rstack list -> int
   val rstack_fo_index : var -> rstack list -> int
   val rstack_p_index : shape_param -> var -> rstack list -> int
   val rstack_c_index : var -> rstack list -> int

   val array_rstack_index : var -> rstack array -> int
   val array_rstack_so_index : var -> rstack array -> int
   val array_rstack_freefo_index : var -> rstack array -> int
   val array_rstack_fo_index : var -> rstack array -> int
   val array_rstack_p_index : shape_param -> var -> rstack array -> int
   val array_rstack_c_index : var -> rstack array -> int

   (*
    * Consistency in the stack.
    *)
   val check_arity : var -> var list -> int -> rstack -> unit
   val rstack_check_arity : var -> var list -> int -> rstack list -> unit

   (*
    * Stack operations.
    *)
   val rstack_upgrade : var -> rstack list -> rstack list

   (*
    * Assoc.
    *)
   val var_index : (var * int) list -> term -> int
   val svar_index : (var * int) list -> var -> int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
