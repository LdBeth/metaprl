(*
 * This file includes definitions for operators,
 * terms with delayed substitution, and regular terms.
 *
 * Note: many functions in this module (most dest_* and
 * is_* functions, shape, etc) have side-effect:
 * if the term is a Subst, they push substitutions one step down
 * and _replace_ the referenced term with the resulting Term term_single.
 * alpha_equal* functions may push down and eliminate all the Substs, not
 * only the top ones.
 *)

open Opname
open Term_sig

module Term :
sig

   (************************************************************************
    * Types                                                                *
    ************************************************************************)

   module StringSet : ( Splay_set.S with type elt = string )

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   type level_exp_var' = { le_var : string; le_offset : int }
   type level_exp_var = level_exp_var'

   type level_exp' = { le_const : int; le_vars : level_exp_var list }
   type level_exp = level_exp'

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
    | ParamList of param list

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
   and param = param'

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   type operator' = { op_name : opname; op_params : param list }
   type operator = operator'

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *
    * free_vars - set of the free variables
    *
    * Subst (BSubst) - delayed simultanious substitution
    *)

   type term_subst = (string * term) list
   and term_core = 
      Term of term'
    | Subst of term * term_subst
   and term = { free_vars : StringSet.t; mutable core : term_core }
   and bound_term_core = 
      BTerm of bound_term'
    | BSubst of bound_term * term_subst
   and bound_term = { bfree_vars : StringSet.t; mutable bcore: bound_term_core }
   and term' = { term_op : operator; term_terms : bound_term list }
   and bound_term' = { bvars : string list; bterm : term }

   (************************************************************************
    * De/Constructors                                                      *
    ************************************************************************)

   (* Errors during matching *)
   exception TermMatch of string * term * string
   exception BadMatch of term * term
   exception Not_var

   (*
    * simultaneous delayed substitution
    *)
   val do_term_subst : term_subst -> term -> term
   val do_bterm_subst : term_subst -> bound_term -> bound_term

   val dest_term : term -> term'
   val make_term : term' -> term
   val mk_op : opname -> param list -> operator
   val mk_term : operator -> bound_term list -> term
   val mk_bterm : string list -> term -> bound_term
   val make_bterm : bound_term' -> bound_term
   val dest_bterm : bound_term -> bound_term'
   val mk_level : int -> level_exp_var list -> level_exp
   val mk_level_var : string -> int -> level_exp_var

   val no_bvars : bound_term list -> bool
   val mk_simple_bterm : term -> bound_term

   (* term is only necessary to raise the appropriate exception *)
   val dest_simple_bterm : term -> bound_term -> term
   val dest_simple_bterms : term -> bound_term list -> term list

   val normalize_term : term -> term

   (* Projections *)
   val opname_of_term : term -> opname
   val subterms_of_term : term -> term list
   val subterm_count : term -> int
   val subterm_arities : term -> int list

   (* These are trivial identity functions *)
   val make_op : operator' -> operator
   val dest_op : operator -> operator'
   val make_param : param' -> param
   val dest_param : param -> param'
   val make_level : level_exp' -> level_exp
   val dest_level : level_exp -> level_exp'
   val make_level_var : level_exp_var' -> level_exp_var
   val dest_level_var : level_exp_var -> level_exp_var'
   val make_object_id : param list -> object_id
   val dest_object_id : object_id  ->  param list

   (*
    * A variable is a term with opname "var", and a single
    * var parameter that is the name of the variable.
    *)

   val var_opname : opname
   val context_opname : opname

   val is_var_term_nods : term' -> bool
   val is_var_term : term -> bool
   val dest_var_nods : term' -> string
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
    * Simple terms have no paramaters and
    * all subterms have no binding vars.
    *)
   val mk_any_term : operator -> term list -> term
   val mk_simple_term : opname -> term list -> term
   val dest_simple_term : term -> (opname * term list)
   val is_simple_term_opname : opname -> term -> bool
   val dest_simple_term_opname : opname -> term -> term list
end

