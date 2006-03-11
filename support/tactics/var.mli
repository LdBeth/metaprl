(*
 * Utilities for generating variable names.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and
 * California Institute of Technology
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_symbol
open Tactic_type.Tactic
open Refiner.Refiner.TermType

(* Generate a new var different from any in the list *)
val new_var           : var -> var list -> var
val maybe_new_var     : var -> var list -> var
val maybe_new_var_set : var -> SymbolSet.t -> var
val maybe_new_vars    : var list -> var list -> var list

(* var_subst_to_bind (v) 'A[t] t = bind{v.'A['v]} *)
val var_subst_to_bind : ?var:var -> term -> term -> term
(*
 * var_subst_to_bind2 (v) 'A[t;s] t s = bind{v,w.'A['v;'w]}
 * note that t is replaced with variable first;
 *	it's important if t is a subterm of s or vice versa
 *)
val var_subst_to_bind2 : ?var:var -> term -> term -> term -> term
val get_bind_from_arg_or_concl_subst : tactic_arg -> term -> term
val get_bind_from_arg_or_hyp_subst : tactic_arg -> int -> term -> term

(* New variable generation *)
val new_symbol_term : var -> term
val new_symbol_string_term : string -> term

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
