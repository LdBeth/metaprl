(*
 * Managing reflected terms.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
open Lm_symbol

open Opname
open Term_sig
open Term_ty_sig
open Term_shape_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape

open Filter_shape
open Filter_base_type

(*
 * Hooks.
 *)
val is_xquote_term    : term -> bool
val is_xquote0_term   : term -> bool

(*
 * Reflection processing.
 *)
val dest_xquote_term  : parse_state -> term -> term
val dest_xquote0_term : parse_state -> term -> term

(*
 * Constructing rules and theorems.
 *)
type parse_info

type var_info = var * var list * int

type socvars_info =
   { cvars_info  : var_info list;
     sovars_info : var_info list
   }

val create_parse_info : parse_state -> parse_info

(*
 * Term processing.
 *)
val quote_term        : parse_info -> ?depth: term -> term -> term

(*
 * Rule generation.
 *)
val mk_rule_term      : parse_info -> meta_term -> term
val mk_rule_wf_thm    : parse_info -> term -> meta_term
val mk_logic_wf_thm   : parse_info -> term -> meta_term
val mk_intro_thm      : parse_info -> term -> provable_kind -> meta_term -> term_param list -> socvars_info * meta_term * term_param list
val mk_mem_logic_thm  : parse_info -> term -> term -> meta_term
val mk_type_check_thm : parse_info -> (term, term) poly_ty_term -> meta_term
val mk_sequent_concl_check_thm : parse_info -> (term, term) poly_ty_term -> term -> term -> meta_term
val mk_sequent_step_check_thm  : parse_info -> (term, term) poly_ty_term -> term -> term -> term -> term * meta_term

(*
 * Elimination theorems.
 *)
val mk_elim_thm             : parse_info -> term -> meta_term list -> (* parents *) term list -> var * meta_term

val mk_elim_start_thm       : parse_info -> term -> var * meta_term
val mk_simple_step_elim_thm : parse_info -> term -> (* rules *) term list -> (* parents *) term list -> var * meta_term
val mk_proof_check_elim_thm : parse_info -> term -> meta_term -> var * meta_term

(*
 * Various terms.
 *)
val mk_reflect_df1_term : parse_info -> term -> term
val mk_reflect_df2_term : parse_info -> term -> term -> term

val mk_empty_logic_term : parse_info -> term
val mk_rules_logic_term : parse_info -> term list -> term -> term
val mk_union_logic_term : parse_info -> term -> term -> term

val is_meta_type_term   : parse_info -> term -> bool

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
