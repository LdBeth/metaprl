(*
 * Define terms.  This is the abstract term interface.
 * There may be several implementations of terms.
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
open Refine_error_sig

module type TermSig =
sig
   (************************************************************************
    * Types                                                                *
    ************************************************************************)

   (*
    * Operators have a name and parameters.
    *
    * When this file is copied to term_std.mli, the
    * following for type definitions become abstract.
    *)
   type level_exp_var
   and level_exp
   and param
   and operator

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
   and term
   and bound_term

   (************************************************************************
    * Interface types                                                      *
    ************************************************************************)

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   and level_exp_var' = { le_var : string; le_offset : int }

   and level_exp' = { le_const : int; le_vars : level_exp_var list }

   (*
    * Parameters have a number of simple types.
    *)
   and object_id = param list

   and param' =
      Number of Mp_num.num
    | String of string
    | Token of string
    | Var of string
    | MNumber of string
    | MString of string
    | MToken of string
    | MLevel of level_exp
    | MVar of string

      (* Special Nuprl5 values *)
    | ObId of object_id
    | ParamList of param list

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   and operator' = { op_name : opname; op_params : param list }

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
   and term' = { term_op : operator; term_terms : bound_term list }
   and bound_term' = { bvars : string list; bterm : term }

   (*
    * The terms in the framework include
    * a meta-implication and met-iff.
    *)
   type meta_term =
      MetaTheorem of term
    | MetaImplies of meta_term * meta_term
    | MetaFunction of term * meta_term * meta_term
    | MetaIff of meta_term * meta_term
    | MetaLabeled of string * meta_term

   (************************************************************************
    * SEQUENTS                                                             *
    ************************************************************************)

   type hypothesis =
      HypBinding of string * term
    | Hypothesis of term
    | Context of string * term list

   type seq_hyps
   type seq_goals

   type esequent =
      { sequent_args : term;
        sequent_hyps : seq_hyps;
        sequent_goals : seq_goals
      }
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)