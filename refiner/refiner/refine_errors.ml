(************************************************************************
 * REFINER ERRORS                                                       *
 ************************************************************************)

open Printf
open Debug
open Term_sig
open Term_addr_sig
open Term_meta_sig
open Refine_errors_sig

let _ =
   if !debug_load then
      eprintf "Loading Refine_errors%t" eflush

module RefineErrors
   (Term : TermSig)
   (TermAddr : TermAddrSig
    with type term = Term.term) 
   (TermMeta : TermMetaSig
    with type term = Term.term) =
struct
   type term = Term.term
   type bound_term = Term.bound_term
   type address = TermAddr.address
   type param = Term.param
   type meta_term = TermMeta.meta_term
   type level_exp = Term.level_exp

   type stack =
      StackVoid
    | StackNumber of Num.num
    | StackString of string
    | StackLevel of level_exp
    | StackBTerm of term * string list
    | StackITerm of (term * string list * string list * term list) list
    | StackContext of string list * term * address

   type match_type =
      ParamMatch of param
    | VarMatch of string
    | TermMatch of term
    | BTermMatch of bound_term

   (* Detailed exceptions *)
   type rewrite_error =
      BoundSOVar of string
    | FreeSOVar of string
    | BoundParamVar of string
    | FreeParamVar of string
    | BadRedexParam of param
    | NoRuleOperator
    | BadMatch of match_type
    | AllSOInstances of string
    | MissingContextArg of string
    | StackError of stack
    | RewriteStringError of string

   exception RewriteErr of rewrite_error

   exception FreeContextVars of string list

   (*
    * Unfortunately, we need to declare the general TacticException
    * type here, because the following combinators need to
    * collect exceptions of their subtactics.
    *)
   type refine_error_info =
      StringError of string
    | IntError of int
    | TermError of term
    | StringIntError of string * int
    | StringStringError of string * string
    | StringTermError of string * term
    | GoalError of refine_error
    | SecondError of refine_error
    | SubgoalError of int * refine_error
    | PairError of refine_error * refine_error
    | RewriteAddressError of address * refine_error
    | RewriteError of rewrite_error
    | NodeError of string * term * refine_error list
    | TermMatchError of string * term * string
    | TermPairMatchError of term * term
    | AddressError of address * term
    | MetaTermMatchError of meta_term

   and refine_error = string * refine_error_info

   exception RefineError of refine_error
end

