(*
 * Operations on "manifest" terms.  These are
 * terms that have a pervasive definition.
 *
 * Sequents are manifest only for efficiency.
 * The refiner does not use them, but they are
 * included because many logics use sequents.
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

module type TermManSig =
sig
   type term
   type operator
   type level_exp
   type address
   type esequent

   (************************************************************************
    * Simplified operations on manifest terms                              *
    ************************************************************************)

   (* Level expression operations *)
   val mk_const_level_exp : int -> level_exp
   val mk_var_level_exp : string -> level_exp
   val max_level_exp : level_exp -> level_exp -> int -> level_exp
   val incr_level_exp : level_exp -> level_exp

   val level_le : level_exp -> level_exp -> bool
   val level_lt : level_exp -> level_exp -> bool

   (*
    * Sequents.
    * This should be visible only to sequents, but oh well.
    *)
   val is_sequent_term : term -> bool
   val mk_sequent_term : esequent -> term
   val explode_sequent : term -> esequent
   val args_of_sequent : term -> term

   val nth_hyp : term -> int -> term
   val nth_binding : term -> int -> string
   val nth_concl : term -> int -> term
   val num_hyps : term -> int
   val declared_vars : term -> string list
   val get_decl_number : term -> string -> int
   val is_free_seq_var : int -> string -> term -> bool

   (*
    * The nth_*_addr functions are used to
    * compute addreses for parts of a sequent.
    * The indexing starts from 1.  Clause 0
    * refers to the conclusion.
    *
    * The range functions provide addresses
    * that are used for contexts in sequent
    * rewriting.
    *)
   val nth_hyp_addr : term -> int -> address
   val nth_concl_addr : term -> int -> address
   val nth_clause_addr : term -> int -> address
   val hyp_range_addr : term -> int -> address
   val hyp_indices_addr : term -> int -> address * address
   val concl_range_addr : term -> int -> address
   val replace_goal : term -> term -> term          (* One concl *)

   (* The following function is only intended for use from MP top-loop *)
   val clause_address : int -> address

   val is_xrewrite_term : term -> bool
   val mk_xrewrite_term : term -> term -> term
   val dest_xrewrite : term -> term * term

   (*
    * Primitive lists.
    *)
   val is_xnil_term : term -> bool
   val xnil_term : term

   val is_xcons_term : term -> bool
   val mk_xcons_term : term -> term -> term
   val dest_xcons : term -> term * term

   val is_xlist_term : term -> bool
   val dest_xlist : term -> term list
   val mk_xlist_term : term list -> term

   (*
    * Primitive strings.
    *)
   val is_xstring_term : term -> bool
   val mk_xstring_term : string -> term
   val dest_xstring : term -> string

   val is_xstring_dep0_term : term -> bool
   val mk_xstring_dep0_term : string -> term -> term
   val dest_xstring_dep0_term : term -> string * term

   (*
    * Primitive abstractions.
    *)
   val is_xbind_term : term -> bool
   val mk_xbind_term : string -> term -> term

   (*
    * Construct a redex out of some vars, params, and other terms.
    *)
   val construct_redex : string array -> term list -> term list -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
