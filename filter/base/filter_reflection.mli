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
open Opname
open Term_sig
open Term_ty_sig
open Term_shape_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape

(*
 * For expanding quotations.
 *)
type parse_state =
   { parse_quotation : string -> string -> term;
     parse_opname : op_kind -> string list -> shape_param list -> int list -> Opname.opname;
     parse_param : term -> param
   }

(*
 * Hooks.
 *)
val is_xquote_term : term -> bool
val is_xquote0_term : term -> bool
val is_xrulequote_term : term -> bool

(*
 * Reflection processing.
 *)
val dest_xquote_term : parse_state -> term -> term
val dest_xquote0_term : parse_state -> term -> term
val dest_xrulequote_term : parse_state -> term -> term
val dest_xrulequote_term_raw : parse_state -> term -> term

(*
 * Constructing rules and theorems.
 *)
val mk_rule_wf_thm : parse_state -> term -> meta_term
val mk_infer_thm : parse_state -> meta_term -> meta_term
val mk_type_check_thm : parse_state -> (term, term) poly_ty_term -> meta_term

val mk_logic_info : parse_state ->
   term ->        (* The name that will be given to the logic *)
   term list ->   (* The rules in the logic *)
   term ->        (* The argument 'p to the Provable{'p} predicate *)
   term ->        (* The Provable{'p} term *)

   (* List of rules, logic_wf rule, Provable{Sequent; logic; p}, provable_wf *)
   term * meta_term * term * meta_term

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
