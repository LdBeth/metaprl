(*
 * Make a generic error module.
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
open Lm_symbol

open Opname

open Refine_error_sig
open Term_sig

(*
 * Error module.
 *)
module MakeRefineError (TermType : TermSig) (ParamType : ErrParamSig) =
struct
   module Types = TermType;;
   module Params = ParamType;;
   open Types
   open Params

   (*
    * Match errors in the rewriter.
    *)
   type match_type =
      ParamMatch of param
    | VarMatch of var
    | TermMatch of term
    | TermMatch2 of term * term
    | BTermMatch of bound_term
    | HypMatch of seq_hyps

   (*
    * We declare the general exception type for all the
    * modules in the refiner.
    *
    * The RefineError parameters are only meant to be used for user
    * interaction and debugging purposes. But in the code that does not directly
    * produce any output, try ... with ... expressions should
    * only mention generic  RefineError _  pattern and should not
    * try to match specific parameters.
    *
    * This limitation makes it possible to use RefineError(GenericError)
    * instead of any other RefineError when we do not care about printed
    * error messages and want to reduce the RefineError allocation
    * overhead.
    *)
   type refine_error =
      (* Generic error is used in simplified refiners for space saving reasons *)
      GenericError

      (* Mptop toploop ignore this error *)
    | ToploopIgnoreError

      (* General refinement errors *)
    | StringError of string
    | IntError of int
    | TermError of term
    | StringIntError of string * int
    | StringStringError of string * string
    | StringVarError of string * var
    | StringTermError of string * term
    | StringWrapError of string * refine_error
    | SubgoalError of int * string * refine_error
    | PairError of string * refine_error * string * refine_error

      (* Addressing errors *)
    | NodeError of string * term * (string * refine_error) list
    | AddressError of address * term

      (* Term module errors *)
    | TermMatchError of term * string
    | TermPairError of term * term
    | MetaTermMatchError of meta_term

      (* Rewriter errors *)
    | RewriteFreeSOVar of var
    | RewriteSOVarArity of var
    | RewriteBoundParamVar of var
    | RewriteFreeParamVar of var
    | RewriteBadRedexParam of param
    | RewriteNoRuleOperator
    | RewriteBadMatch of match_type
    | RewriteAllSOInstances of var
    | RewriteMissingContextArg of var
    | RewriteStringError of string
    | RewriteStringOpnameOpnameError of string * opname * opname
    | RewriteAddressError of address * string * refine_error
    | RewriteFreeContextVar of var * var

      (* Type errors *)
    | VarError of var
    | OpnameError of opname
    | Opname2Error of opname * opname
    | ParamError of param
    | Param2Error of param * param
    | ParamTyParamError of param * ty_param
    | ShapeError of shape
    | Shape2Error of shape * shape
    | Term2Error of term * term
    | VarTermError of var * term
    | IntTermError of int * term

      (* Wrapped errors *)
    | StringErrorError of string * refine_error
    | VarErrorError of var * refine_error
    | IntErrorError of int * refine_error
    | TermErrorError of term * refine_error
    | OpnameErrorError of opname * refine_error
    | ShapeErrorError of shape * refine_error
    | MetaTermErrorError of meta_term * refine_error

   (*
    * Every error is paired with the name of
    * the function that raised it.
    *)
   exception RefineError of string * refine_error
   exception RefineForceError of string * string * refine_error

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
