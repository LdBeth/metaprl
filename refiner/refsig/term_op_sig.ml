(*
 * Pulling terms apart.
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

open Opname

module type TermOpSig =
sig
   type term
   type operator
   type level_exp

   (* Map a function over the term *)
   val map_down : (term -> term) -> term -> term
   val map_up : (term -> term) -> term -> term

   (* Special cases *)
   val is_no_subterms_term : opname -> term -> bool

   val is_dep0_term : opname -> term -> bool
   val mk_dep0_term : opname -> term -> term
   val dest_dep0_term : opname -> term -> term
   val one_subterm : term -> term
   val one_subterm_opname : opname -> term -> term

   val is_dep0_dep0_term : opname -> term -> bool
   val mk_dep0_dep0_term : opname -> term -> term -> term
   val dest_dep0_dep0_term : opname -> term -> term * term
   val two_subterms : term -> term * term
   val two_subterms_opname : opname -> term -> term * term

   val is_dep0_dep0_dep0_term : opname -> term -> bool
   val mk_dep0_dep0_dep0_term : opname -> term -> term -> term -> term
   val dest_dep0_dep0_dep0_term : opname -> term -> term * term * term

   val is_two_subterm : opname -> term -> bool
   val is_three_subterm : opname -> term -> bool
   val is_five_subterm : opname -> term -> bool
   val three_subterms : term -> term * term * term
   val four_subterms : term -> term * term * term * term
   val five_subterms : term -> term * term * term * term * term
   val six_subterms : term -> term * term * term * term * term * term

   val is_dep1_term : opname -> term -> bool
   val mk_dep1_term : opname -> string -> term -> term
   val dest_dep1_term : opname -> term -> string * term
   val is_dep2_term : opname -> term -> bool
   val mk_dep2_term : opname -> string -> string -> term -> term
   val dest_dep2_term : opname -> term -> string * string * term

   val is_dep0_dep1_term : opname -> term -> bool
   val is_dep0_dep1_any_term : term -> bool
   val mk_dep0_dep1_term : opname -> string -> term -> term -> term
   val mk_dep0_dep1_any_term : operator -> string -> term -> term -> term
   val dest_dep0_dep1_term : opname -> term -> string * term * term
   val dest_dep0_dep1_any_term : term -> string * term * term

   val is_dep0_dep2_term : opname -> term -> bool
   val mk_dep0_dep2_term : opname -> string -> string -> term -> term -> term
   val dest_dep0_dep2_term : opname -> term -> string * string * term * term

   val is_dep0_dep3_term : opname -> term -> bool
   val mk_dep0_dep3_term : opname -> string -> string -> string -> term -> term -> term
   val dest_dep0_dep3_term : opname -> term -> string * string * string * term * term

   val is_dep2_dep0_term : opname -> term -> bool
   val mk_dep2_dep0_term : opname -> string -> string -> term -> term -> term
   val dest_dep2_dep0_term : opname -> term -> string * string * term * term

   val is_dep0_dep0_dep1_term : opname -> term -> bool
   val mk_dep0_dep0_dep1_term : opname -> term -> term -> string -> term -> term
   val dest_dep0_dep0_dep1_term : opname -> term -> term * term * string * term

   val is_dep0_dep0_dep1_any_term : term -> bool
   val mk_dep0_dep0_dep1_any_term : operator -> term -> term -> string -> term -> term
   val dest_dep0_dep0_dep1_any_term : term -> term * term * string * term

   val is_dep0_dep0_dep2_term : opname -> term -> bool
   val mk_dep0_dep0_dep2_term : opname -> term -> term -> string -> string -> term -> term
   val dest_dep0_dep0_dep2_term : opname -> term -> term * term * string * string * term

   val is_dep0_dep0_dep2_any_term : term -> bool
   val mk_dep0_dep0_dep2_any_term : operator -> term -> term -> string -> string -> term -> term
   val dest_dep0_dep0_dep2_any_term : term -> term * term * string * string * term

   val is_dep0_dep1_dep1_term : opname -> term -> bool
   val mk_dep0_dep1_dep1_term : opname -> term -> string -> term -> string -> term -> term
   val dest_dep0_dep1_dep1_term : opname -> term -> term * string * term * string * term

   val is_dep0_dep2_dep2_term : opname -> term -> bool
   val mk_dep0_dep2_dep2_term : opname -> term -> string -> string -> term -> string -> string -> term -> term
   val dest_dep0_dep2_dep2_term : opname -> term -> term * string * string * term * string * string * term

   val is_dep0_dep2_dep0_dep2_term : opname -> term -> bool
   val mk_dep0_dep2_dep0_dep2_term : opname -> term -> string -> string -> term -> term -> string -> string -> term -> term
   val dest_dep0_dep2_dep0_dep2_term : opname -> term -> term * string * string * term * term * string * string * term

   val is_dep0_dep0_dep3_term : opname -> term -> bool
   val mk_dep0_dep0_dep3_term : opname -> term -> term -> string -> string -> string -> term -> term
   val dest_dep0_dep0_dep3_term : opname -> term -> term * term * string * string * string * term

   val is_string_term : opname -> term -> bool
   val mk_string_term : opname -> string -> term
   val dest_string_term : opname -> term -> string
   val dest_string_param : term -> string

   val is_string_dep0_term : opname -> term -> bool
   val mk_string_dep0_term : opname -> string -> term -> term
   val dest_string_dep0_term : opname -> term -> string * term

   val is_string_string_dep0_term : opname -> term -> bool
   val mk_string_string_dep0_term : opname -> string -> string -> term -> term
   val dest_string_string_dep0_term : opname -> term -> string * string * term
   val dest_string_string_dep0_any_term : term -> string * string * term

   val is_number_number_dep0_term : opname -> term -> bool
   val mk_number_number_dep0_term : opname -> Mp_num.num -> Mp_num.num -> term -> term
   val dest_number_number_dep0_term : opname -> term -> Mp_num.num * Mp_num.num * term
   val dest_number_number_dep0_any_term : term -> Mp_num.num * Mp_num.num * term

   val is_number_number_string_dep0_term : opname -> term -> bool
   val mk_number_number_string_dep0_term : opname -> Mp_num.num -> Mp_num.num -> string -> term -> term
   val dest_number_number_string_dep0_term : opname -> term -> Mp_num.num * Mp_num.num * string * term
   val dest_number_number_string_dep0_any_term : term -> Mp_num.num * Mp_num.num * string * term

   val is_string_string_dep0_dep0_term : opname -> term -> bool
   val mk_string_string_dep0_dep0_term : opname -> string -> string -> term -> term -> term
   val dest_string_string_dep0_dep0_term : opname -> term -> string * string * term * term
   val dest_string_string_dep0_dep0_any_term : term -> string * string * term * term

   val is_number_term : opname -> term -> bool
   val mk_number_term : opname -> Mp_num.num -> term
   val dest_number_term : opname -> term -> Mp_num.num
   val dest_number_any_term : term -> Mp_num.num

   val is_univ_term : opname -> term -> bool
   val mk_univ_term : opname -> level_exp -> term
   val dest_univ_term : opname -> term -> level_exp

   val is_token_term : opname -> term -> bool
   val mk_token_term : opname -> string -> term
   val dest_token_term : opname -> term -> string

   val is_token_simple_term : opname -> term -> bool
   val mk_token_simple_term : opname -> string -> term list -> term
   val dest_token_simple_term : opname -> term -> string * term list
end

