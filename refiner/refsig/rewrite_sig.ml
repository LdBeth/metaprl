(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
 *
 *)

module type RewriteSig =
sig
   (* Import the term types *)
   type term
   type level_exp
   type param
   type operator
   type bound_term
   type address

   (* Packaged rewrite rule *)
   type rewrite_rule
   
   (* Separated forms *)
   type rewrite_redex
   type rewrite_contractum
   type rewrite_stack
   
   (*
    * Types for redex matching.
    *)
   type rewrite_type =
      RewriteTermType of string
    | RewriteFunType of string
    | RewriteContextType of string
    | RewriteStringType of string
    | RewriteIntType of string
    | RewriteLevelType of string
   
   type rewrite_item =
      RewriteTerm of term
    | RewriteFun of (term list -> term)
    | RewriteContext of (term -> term list -> term)
    | RewriteString of string
    | RewriteInt of int
    | RewriteLevel of level_exp
   
   type stack
   
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
    | StringError of string
   
   exception RewriteError of rewrite_error
   
   (*
    * Separate analysis.
    *)
   val compile_redex : string array -> term -> rewrite_redex
   val compile_redices : string array -> term list -> rewrite_redex
   val compile_contractum : rewrite_redex -> term -> rewrite_contractum
   val extract_redex_types : rewrite_redex -> rewrite_type list
   val apply_redex :
      rewrite_redex -> address array ->
      term list -> rewrite_stack
   val apply_redex' :
      rewrite_redex -> address array ->
      term list -> rewrite_stack * rewrite_item list
   val make_contractum : rewrite_contractum -> rewrite_stack -> term
   
   (* Rewrite constructor/destructors *)
   val term_rewrite : string array * string array ->
      term list -> term list -> rewrite_rule
   val fun_rewrite : term -> (term -> term) -> rewrite_rule
   
   (* Apply a rewrite to a term *)
   val apply_rewrite : rewrite_rule -> address array * string array ->
      term list -> term list * string array
   
   (*
    * See if a rule may apply to a particular term
    * described by its operator and it arities.
    *)
   val relevant_rule : operator -> int list -> rewrite_rule -> bool
   
   (*
    * Get some info for the evaluator.
    *)
   val rewrite_operator : rewrite_rule -> operator
   val rewrite_eval_flags : rewrite_rule -> (int * bool) list
end

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:01:40  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:10  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

