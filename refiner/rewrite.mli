(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
 *
 *)

open Term

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
exception BoundSOVar of string
exception FreeSOVar of string
exception BoundParamVar of string
exception FreeParamVar of string
exception BadRedexParam of param
exception NoRuleOperator
exception BadMatch of match_type
exception AllSOInstances of string
exception MissingContextArg of string
exception StackError of stack
exception RewriteError of string

(*
 * Separate analysis.
 *)
val compile_redex : string array -> term -> rewrite_redex
val compile_contractum : rewrite_redex -> term -> rewrite_contractum
val extract_redex_types : rewrite_redex -> rewrite_type list
val apply_redex :
   rewrite_redex -> address array ->
   term -> rewrite_stack
val apply_redex' :
   rewrite_redex -> address array ->
   term -> rewrite_stack * rewrite_item list
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

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:36  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.12  1996/03/25 20:50:51  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.11  1996/03/11 18:34:33  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.10  1996/03/08 22:04:59  jyh
 * This rewriter contains the pattern analysis.
 *
 * Revision 1.9  1996/03/08 15:40:53  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.8  1996/03/05 19:48:40  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.7  1996/02/18 23:32:33  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.6  1996/02/13 21:32:36  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.5  1996/02/07 23:41:22  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.4  1996/02/07 20:25:01  jyh
 * Partial checkin whil I change filenames to lowercase.
 *
 * Revision 1.3  1996/02/05 18:15:02  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.2.4.1  1996/02/05 06:09:55  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.2  1996/01/26 20:15:03  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:42:57  jyh
 * This is an ML version of a term rewriting system.
 * This checkin is partial, and provides a rewriter on
 * regular terms.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

