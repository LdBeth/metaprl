(*
 * Make a generic error module.
 *)

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

   (*
    * Match errors in the rewriter.
    *)
   type match_type =
      ParamMatch of param
    | VarMatch of string
    | TermMatch of term
    | BTermMatch of bound_term

   (*
    * We declare the general exception type for all the
    * modules in the refiner.
    *
    * GenericError is used when the specific error is not desired.
    *)
   type refine_error =
      (* Generic error is raised by simplified refiners *)
      GenericError

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
 * $Log$
 * Revision 1.2  1998/07/03 22:05:37  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.1  1998/07/02 18:35:24  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
