(*
 * Define terms.
 * This file includes definitions for operators,
 * DeBruijn terms, and regular terms.
 *
 *)

open Opname

(************************************************************************
 * Types                                                                *
 ************************************************************************)

(*
 * Operators have a name and parameters.
 *)
type level_exp_var
type level_exp
type param
type operator

(*
 * A term has an operator, and a finite number of subterms
 * that may be bound.
 *)
type term
type bound_term

(************************************************************************
 * Interface types                                                      *
 ************************************************************************)

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
type level_exp_var' = { le_var : string; le_offset : int }

type level_exp' = { le_const : int; le_vars : level_exp_var list }

(*
 * Parameters have a number of simple types.
 *)
type param' =
   Number of Num.num
 | String of string
 | Token of string
 | Level of level_exp
 | Var of string
 | MNumber of string
 | MString of string
 | MToken of string
 | MLevel of string
 | MVar of string

   (* Special Nuprl5 values *)
 | ObId of object_id
 | ParmList of param list
   
   (* Num operations *)
 | MSum of param * param
 | MDiff of param * param
 | MProduct of param * param
 | MQuotient of param * param
 | MRem of param * param
 | MLessThan of param * param
   
   (* Comparisons *)
 | MEqual of param * param
 | MNotEqual of param * param

and object_id = param list

(*
 * An operator combines a name with a list of parameters.
 * The order of params is significant.
 *)
type operator' = { op_name : opname; op_params : param list }


(*
 * A term has an operator, and a finite number of subterms
 * that may be bound.
 *)
type term' = { term_op : operator; term_terms : bound_term list }
and bound_term' = { bvars : string list; bterm : term }

(*
 * Address of a subterm.
 *)
type address

(************************************************************************
 * De/Constructors                                                      *
 ************************************************************************)

(* Errors during matching *)
exception TermMatch of string * term * string

(*
 * General interface.
 *)
val mk_term : operator -> bound_term list -> term
val make_term : term' -> term
val dest_term : term -> term'
val mk_op : opname -> param list -> operator
val make_op : operator' -> operator
val dest_op : operator -> operator'
val mk_bterm : string list -> term -> bound_term
val make_bterm : bound_term' -> bound_term
val dest_bterm : bound_term -> bound_term'
val make_param : param' -> param
val dest_param : param -> param'
val mk_level : int -> level_exp_var list -> level_exp
val make_level : level_exp' -> level_exp
val dest_level : level_exp -> level_exp'
val mk_level_var : string -> int -> level_exp_var
val make_level_var : level_exp_var' -> level_exp_var
val dest_level_var : level_exp_var -> level_exp_var'

val make_object_id : param list -> object_id 
val dest_object_id : object_id  ->  param list

val normalize_term : term -> term

(* Addresses *)
val string_of_address : address -> string
val make_address : int list -> address
val make_seq_address : int -> address
val nth_cdr_addr : int -> address

(* Projections *)
val opname_of_term : term -> opname
val subterms_of_term : term -> term list

(*
 * Simple terms have no paramaters and
 * all subterms have no binding vars.
 *)
val mk_any_term : operator -> term list -> term
val mk_simple_term : opname -> term list -> term
val dest_simple_term : term -> (opname * term list)

(* Special cases *)
val is_dep0_term : opname -> term -> bool
val mk_dep0_term : opname -> term -> term
val dest_dep0_term : opname -> term -> term
val one_subterm : term -> term

val is_dep0_dep0_term : opname -> term -> bool
val mk_dep0_dep0_term : opname -> term -> term -> term
val dest_dep0_dep0_term : opname -> term -> term * term
val two_subterms : term -> term * term

val is_dep0_dep0_dep0_term : opname -> term -> bool
val mk_dep0_dep0_dep0_term : opname -> term -> term -> term -> term
val dest_dep0_dep0_dep0_term : opname -> term -> term * term * term
val three_subterms : term -> term * term * term
val four_subterms : term -> term * term * term * term
val five_subterms : term -> term * term * term * term * term

val is_dep1_term : opname -> term -> bool
val mk_dep1_term : opname -> string -> term -> term
val dest_dep1_term : opname -> term -> string * term

val is_dep0_dep1_term : opname -> term -> bool
val is_dep0_dep1_any_term : term -> bool
val mk_dep0_dep1_term : opname -> string -> term -> term -> term
val mk_dep0_dep1_any_term : operator -> string -> term -> term -> term
val dest_dep0_dep1_term : opname -> term -> string * term * term
val dest_dep0_dep1_any_term : term -> string * term * term

val is_dep0_dep2_term : opname -> term -> bool
val mk_dep0_dep2_term : opname -> string -> string -> term -> term -> term
val dest_dep0_dep2_term : opname -> term -> string * string * term * term

val is_dep2_dep0_term : opname -> term -> bool
val mk_dep2_dep0_term : opname -> string -> string -> term -> term -> term
val dest_dep2_dep0_term : opname -> term -> string * string * term * term

val is_dep0_dep0_dep1_term : opname -> term -> bool
val mk_dep0_dep0_dep1_term : opname -> term -> term -> string -> term -> term
val dest_dep0_dep0_dep1_term : opname -> term -> term * term * string * term

val is_dep0_dep0_dep1_any_term : term -> bool
val mk_dep0_dep0_dep1_any_term : operator -> term -> term -> string -> term -> term
val dest_dep0_dep0_dep1_any_term : term -> term * term * string * term

val is_dep0_dep1_dep1_term : opname -> term -> bool
val mk_dep0_dep1_dep1_term : opname -> term -> string -> term -> string -> term -> term
val dest_dep0_dep1_dep1_term : opname -> term -> term * string * term * string * term

val is_dep0_dep2_dep2_term : opname -> term -> bool
val mk_dep0_dep2_dep2_term : opname -> term -> string -> string -> term -> string -> string -> term -> term
val dest_dep0_dep2_dep2_term : opname -> term -> term * string * string * term * string * string * term

val is_dep0_dep2_dep0_dep2_term : opname -> term -> bool
val mk_dep0_dep2_dep0_dep2_term : opname -> term -> string -> string -> term -> term -> string -> string -> term -> term
val dest_dep0_dep2_dep0_dep2_term : opname -> term -> term * string * string * term * term * string * string * term

val is_dep0_dep0_dep3_term : opname -> term -> bool
val mk_dep0_dep0_dep3_term : opname -> term -> term -> string -> string -> string -> term -> term
val dest_dep0_dep0_dep3_term : opname -> term -> term * term * string * string * string * term

val is_string_term : opname -> term -> bool
val mk_string_term : opname -> string -> term
val dest_string_term : opname -> term -> string
val dest_string_param : term -> string

val is_string_dep0_term : opname -> term -> bool
val mk_string_dep0_term : opname -> string -> term -> term
val dest_string_dep0_term : opname -> term -> string * term

val is_string_string_dep0_term : opname -> term -> bool
val mk_string_string_dep0_term : opname -> string -> string -> term -> term
val dest_string_string_dep0_term : opname -> term -> string * string * term
val dest_string_string_dep0_any_term : term -> string * string * term

val is_number_number_dep0_term : opname -> term -> bool
val mk_number_number_dep0_term : opname -> Num.num -> Num.num -> term -> term
val dest_number_number_dep0_term : opname -> term -> Num.num * Num.num * term
val dest_number_number_dep0_any_term : term -> Num.num * Num.num * term

val is_string_string_dep0_dep0_term : opname -> term -> bool
val mk_string_string_dep0_dep0_term : opname -> string -> string -> term -> term -> term
val dest_string_string_dep0_dep0_term : opname -> term -> string * string * term * term
val dest_string_string_dep0_dep0_any_term : term -> string * string * term * term

val is_number_term : opname -> term -> bool
val mk_number_term : opname -> Num.num -> term
val dest_number_term : opname -> term -> Num.num
val dest_number_any_term : term -> Num.num

val is_univ_term : opname -> term -> bool
val mk_univ_term : opname -> level_exp -> term
val dest_univ_term : opname -> term -> level_exp

val is_token_term : opname -> term -> bool
val mk_token_term : opname -> string -> term
val dest_token_term : opname -> term -> string

(************************************************************************
 * Operations                                                           *
 ************************************************************************)

(*
 * Subterm addressing.
 *)
exception IncorrectAddress of address * term
exception BadAddressPrefix of address * address

val term_subterm :  term -> address -> term
val replace_subterm : term -> address -> term -> term
val apply_fun_at_addr : (term -> term) -> address -> (term -> term)
val remove_addr_prefix : address -> address -> address
val subterm_arities : term -> int list

(*
 * Term operations.
 * subst: simultaneous subst of terms for vars.
 * var_subst: subst of var for a term.
 *)
val subst : term -> term list -> string list -> term
val var_subst : term -> term -> string -> term
val alpha_equal : term -> term -> bool
val alpha_equal_vars : (term * string list) -> (term * string list) -> bool
val alpha_equal_match : (term * string list) ->
       (term * string list * string list * term list) ->
       bool

(*
 * Rename the binding variables so they are all distinct.
 *)
val standardize_apart : term -> term

(*
 * Get the list of free variables.
 *)
val is_free_var : string -> term -> bool
val free_vars : term -> string list
val free_vars_terms : term list -> string list
val context_vars : term -> string list
val binding_vars : term -> string list

(*
 * Comparisons.
 *)
exception BadParamMatch of param * param

val compare_level_exps : level_exp -> level_exp -> int
val compare_params : param -> param -> int
val compare_pattern_params : param -> param -> int
val compare_operators : operator -> operator -> int
val compare_pattern_operators : operator -> operator -> int

type term_subst = (string * term) list

exception BadMatch of term * term

val unify : term_subst -> term -> term -> term_subst

(************************************************************************
 * Simplified operations on manifest terms                              *
 ************************************************************************)

(* Level expression operations *)
val mk_const_level_exp : int -> level_exp
val mk_var_level_exp : string -> level_exp
val incr_level_exp : level_exp -> level_exp
val max_level_exp : level_exp -> level_exp -> level_exp

val level_cumulativity : level_exp -> level_exp -> bool

(*
 * A variable is a term with opname "var", and a single
 * var parameter that is the name of the variable.
 *)
val is_var_term : term -> bool
val dest_var : term -> string
val mk_var_term : string -> term
val mk_var_op : string -> operator

val is_so_var_term : term -> bool
val dest_so_var : term -> string * term list
val mk_so_var_term : string -> term list -> term

val is_context_term : term -> bool
val dest_context : term -> string * term * term list
val mk_context_term : string -> term -> term list -> term

(*
 * Sequents.
 * This should be visible only to sequents, but oh well.
 *)
val is_sequent_term : term -> bool

val is_hyp_term : term -> bool
val dest_hyp : term -> string * term * term
val mk_hyp_term : string -> term -> term -> term

val is_concl_term : term -> bool
val dest_concl : term -> term * term
val mk_concl_term : term -> term -> term
val null_concl : term

val is_sequent_term : term -> bool
val dest_sequent : term -> term list
val goal_of_sequent : term -> term
val mk_sequent_term : term list -> term

val nth_hyp : term -> int -> string * term
val nth_concl : term -> int -> term
val num_hyps : term -> int
val declared_vars : term -> string list
val declarations : term -> (string * term) list
val get_decl_number : term -> string -> int
val is_free_seq_var : int -> string -> term -> bool

val concl_addr : term -> int * int
val replace_concl : term -> term -> term
val replace_goal : term -> term -> term          (* One subgoal *)

(*
 * Primitive lists.
 *)
val is_xnil_term : term -> bool
val xnil_term : term

val is_xcons_term : term -> bool
val mk_xcons_term : term -> term -> term
val dest_xcons : term -> term * term

val is_xlist_term : term -> bool
val dest_xlist : term -> term list
val mk_xlist_term : term list -> term

(*
 * Primitive strings.
 *)
val is_xstring_term : term -> bool
val mk_xstring_term : string -> term
val dest_xstring : term -> string

(*
 * Primitive abstractions.
 *)
val mk_xlambda_term : string -> term -> term

(************************************************************************
 * EFFICIENCY                                                           *
 ************************************************************************)

(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.  
 *)
type shape 

val shape_of_term : term -> shape
val print_shape : shape -> unit

(*
 * ``Special'' terms to be used in reduction rules
 *
 * "canon_var" plays the same role as "var" in reduction rules but 
 * the correspondig subterm should be evaluated before the reduction
 * ("call by value" instead of "call by name")
 *
 * subst (v1,v2,v3,...,vm.T;t1;t2;t3;...;tn)
 * it is an error if m!=n
 * if n=m then subst(...) is T with v1 substituted to t2, v2 - to t2, etc.
 *
 *)

val is_canon_var_term : term -> bool
val dest_canon_var : term -> string
val mk_canon_var_term : string -> term

val is_subst_term : term -> bool
val dest_subst : term -> term * (string list * term list)
val mk_subst_term : term -> (string * term) list -> term
val make_subst_term : term -> string list -> term list -> term
val make_1subst_term : term -> string -> term -> term
val make_2subst_term : term -> string -> string -> term -> term -> term

(*
 * $Log$
 * Revision 1.12  1998/04/21 19:54:33  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.11  1998/03/20 22:16:22  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.10  1998/02/21 20:58:23  jyh
 * Two phase parse/extract.
 *
 * Revision 1.9  1998/02/19 17:13:39  jyh
 * Splitting filter_parse.
 *
 * Revision 1.8  1998/02/12 23:34:53  jyh
 * Modifed term module.
 *
 * Revision 1.7  1998/01/27 23:04:26  jyh
 * Adding OCaml1.07 syntax.
 *
 * Revision 1.6  1997/09/12 17:21:46  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.5  1997/08/16 00:18:59  nogin
 *  * Added several functions (used in evaluator)
 *  * Fixed mk_dep0_dep2_dep0_dep2_term
 *
 * Revision 1.4  1997/08/07 19:43:50  jyh
 * Updated and added Lori's term modifications.
 * Need to update all pattern matchings.
 *
 * Revision 1.3  1997/08/07 19:08:20  lolorigo
 * added ObId and ParmList parameter types
 *
# Revision 1.2  1997/08/06  16:18:15  jyh
# This is an ocaml version with subtyping, type inference,
# d and eqcd tactics.  It is a basic system, but not debugged.
#
 * Revision 1.1  1997/04/28 15:51:44  jyh
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
 * Revision 1.25  1996/11/13 22:58:11  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.24  1996/10/23 15:17:59  jyh
 * First working version of dT tactic.
 *
 * Revision 1.23  1996/09/25 22:52:04  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.22  1996/09/02 19:43:28  jyh
 * Semi working package management.
 *
 * Revision 1.21  1996/05/21 02:14:24  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.20  1996/04/11 13:29:48  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.19  1996/04/07 18:24:57  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.18  1996/03/30 01:37:57  jyh
 * Initial version of ITT.
 *
 * Revision 1.17  1996/03/28 02:58:27  jyh
 * Prelim checkin for an partial version of the refiner document in the
 * first version of README.tex.
 *
 * Revision 1.16  1996/03/25 20:51:05  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.15  1996/03/11 18:34:46  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.14  1996/03/08 15:41:03  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.13  1996/03/05 19:48:47  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.12  1996/02/25 15:16:25  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.11  1996/02/19 18:47:16  jyh
 * Updating format.prl
 *
 * Revision 1.10  1996/02/18 23:32:40  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.9  1996/02/13 21:33:15  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.8  1996/02/10 20:20:04  jyh
 * Initial checkin of filter (prlcomp).
 *
 * Revision 1.7  1996/02/07 23:41:54  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.6  1996/02/07 20:25:14  jyh
 * Partial checkin whil I change filenames to lowercase.
 *
 * Revision 1.5  1996/02/07 17:34:17  jyh
 * This is Version 0 of the refiner in Caml-Light.  At this point,
 * Caml-Light becomes a branch, and main development will be
 * in Caml-Special-Light.
 *
 * Revision 1.4  1996/02/05 18:15:16  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.3.4.1  1996/02/05 06:09:59  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.3  1996/01/31 20:03:01  jyh
 * Generalizing rewriter to work on Sequents.
 *
 * Revision 1.2  1996/01/26 20:15:18  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:43:25  jyh
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
