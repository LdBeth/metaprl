(*
 * Rewrite functions.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Phobos_type
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.Rewrite
open Opname
open Tactic_type.Conversionals

val empty_term : mp_term
val unique_var_term : (string * pos) -> mp_term

val term_of_token : pos -> psymbol -> mp_term
val term_of_token_string : pos -> string -> mp_term
val token_term : unit -> mp_term
val prod_term : mp_term list -> mp_term
(*
val breakup_term : term -> (opname * param' list * bound_term list)
val breakup_bterm : bound_term -> (opname * param' list * bound_term list)
*)
val compile_pattern : mp_pre_term list -> mp_pre_term -> mp_rewrite
val apply_rewrite : mp_rewrite -> mp_term list -> mp_term
val apply_first_rewrite : pos -> mp_rewrite list -> mp_term list(* -> conv*) -> mp_term

val compile_lexer_rewrites : lexer_rewrite_table -> lexer_crewrite_table
val compile_parser_rewrites : rewrite_table -> crewrite_table
