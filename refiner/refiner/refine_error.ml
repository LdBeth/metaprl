(*
 * Make a generic error module.
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
open Term_sig

(*
 * Error module.
 *)
module MakeRefineError (**)
   (TermType : TermSig)
   (AddressType : TypeSig) =
struct
   (*
    * Types.
    *)
   type level_exp = TermType.level_exp
   type param = TermType.param
   type term = TermType.term
   type bound_term = TermType.bound_term
   type meta_term = TermType.meta_term
   type address = AddressType.t
   type seq_hyps = TermType.seq_hyps
   type seq_goals = TermType.seq_goals

   (*
    * Match errors in the rewriter.
    *)
   type match_type =
      ParamMatch of param
    | VarMatch of string
    | TermMatch of term
    | BTermMatch of bound_term
    | HypMatch of seq_hyps
    | GoalMatch of seq_goals

   (*
    * We declare the general exception type for all the
    * modules in the refiner.
    *
    * GenericError is used when the specific error is not desired.
    *)
   type refine_error =
      (* Generic error is raised by simplified refiners *)
      GenericError

      (* Mptop toploop ignore this error *)
    | ToploopIgnoreError

      (* General refinement errors *)
    | StringError of string
    | IntError of int
    | TermError of term
    | StringIntError of string * int
    | StringStringError of string * string
    | StringTermError of string * term
    | GoalError of string * refine_error
    | SecondError of string * refine_error
    | SubgoalError of int * string * refine_error
    | PairError of string * refine_error * string * refine_error

      (* Addressing errors *)
    | NodeError of string * term * (string * refine_error) list
    | AddressError of address * term

      (* Term module errors *)
    | TermMatchError of term * string
    | TermPairMatchError of term * term
    | MetaTermMatchError of meta_term

      (* Rewriter errors *)
    | RewriteBoundSOVar of string
    | RewriteFreeSOVar of string
    | RewriteSOVarArity of string
    | RewriteBoundParamVar of string
    | RewriteFreeParamVar of string
    | RewriteBadRedexParam of param
    | RewriteNoRuleOperator
    | RewriteBadMatch of match_type
    | RewriteAllSOInstances of string
    | RewriteMissingContextArg of string
    | RewriteStringError of string
    | RewriteStringOpnameOpnameError of string * opname * opname
    | RewriteAddressError of address * string * refine_error
    | RewriteFreeContextVars of string list

   (*
    * Every error is paired with the name of
    * the function that raised it.
    *)
   exception RefineError of string * refine_error

   (*
    * A generic refiner error.
    *)
   let generic_refiner_exn = RefineError ("generic", GenericError)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
