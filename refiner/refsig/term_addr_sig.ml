(*
 * Addressed operations on terms.
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

module type TermAddrSig =
sig
   type term
   type address

   (*
    * Constructors.
    *)
   val make_address : int list -> address
   val compose_address : address -> address -> address
   val is_null_address : address -> bool

   (* Works only on sequent addresses and their subaddresses. *)
   val clause_address_of_address : address -> address

   (*
    * Destructors.
    *)
   val string_of_address : address -> string

   (*
    * Addressed operations.
    *)
   val term_subterm :  term -> address -> term
   val term_subterm_count : term -> address -> int
   val replace_subterm : term -> address -> term -> term
   val replace_bound_subterm : term -> address -> string list list -> (string list list -> term) -> term
   val apply_fun_at_addr : (term -> term) -> address -> term -> term
   val apply_fun_arg_at_addr : (term -> term * 'a) -> address -> term -> term * 'a
   val apply_var_fun_at_addr : (string list list -> term -> term) -> address -> string list list -> term -> term
   val apply_var_fun_arg_at_addr : (string list list -> term -> term * 'a) -> address -> string list list -> term -> term * 'a

   (*
    * higherC low-level implementation
    *)
   val apply_fun_higher : (term -> term * 'a) -> term -> term * 'a list
   val apply_var_fun_higher : (string list list -> term -> term * 'a) ->
      string list list -> term -> term * 'a list

   (*
    * The nth_*_addr functions are used to
    * compute addreses for parts of a sequent.
    * The indexing starts from 1.  Clause 0
    * refers to the conclusion. Negative numbers in nth_clause_addr
    * count hypotheses from the end of the list.
    *)
   val nth_hyp_addr : term -> int -> address
   val nth_concl_addr : term -> int -> address
   val nth_clause_addr : term -> int -> address

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
