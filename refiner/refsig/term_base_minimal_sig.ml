(*
 * Minimal term module: Basic term operations.
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)
open Opname

(*
 * We use read-only arrays for sequents.
 *)
module type MinLinSet = sig
   type elt
   type t
   type index = int
   val length : t -> int
   val init : int -> (index -> elt) -> t
   val get : t -> index -> elt
   val of_list : elt list -> t
   val to_list : t -> elt list
end

module type TermBaseMinimalSig =
sig

   DEFTOPMACRO TERM_BASE_MIN_SIG =
   
      (************************************************************************
       * TYPES                                                                *
       ************************************************************************)
   
      type level_exp_var
      type level_exp
      type param
      type operator
      type term
      type bound_term
      type seq_hyps
      type seq_goals
   
      type hypothesis
      type level_exp_var'
      type level_exp'
      type param'
      type operator'
      type term'
      type bound_term'
   
      module SeqHyp : MinLinSet with type elt = hypothesis with type t = seq_hyps
      module SeqGoal : MinLinSet with type elt = term with type t = seq_goals
   
      (************************************************************************
       * De/Constructors                                                      *
       ************************************************************************)
   
      (*
       * General interface.
       *)
      val make_term : term' -> term
      val dest_term : term -> term'
      val make_op : operator' -> operator
      val dest_op : operator -> operator'
      val make_bterm : bound_term' -> bound_term
      val dest_bterm : bound_term -> bound_term'
      val make_param : param' -> param
      val dest_param : param -> param'
      val make_level : level_exp' -> level_exp
      val dest_level : level_exp -> level_exp'
      val make_level_var : level_exp_var' -> level_exp_var
      val dest_level_var : level_exp_var -> level_exp_var'
   
   END

   USETOPMACRO TERM_BASE_MIN_SIG END
end

(*
 * We use read-only arrays for sequents.
 *)
module type TermBaseInternalSig =
sig
   USETOPMACRO TERM_BASE_MIN_SIG END

   (*
    * This function is not exported by the refiner,
    * and it may not even be implemented.
    *)
   val mk_descriptor_term : term Weak_memo.TheWeakMemo.descriptor -> term
   val dest_descriptor : term -> term Weak_memo.TheWeakMemo.descriptor option
end
