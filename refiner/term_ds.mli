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

(************************************************************************
 * Types                                                                *
 ************************************************************************)

module StringSet : ( Set.S with type elt = string )

(*
 * Level expression have offsets from level expression
 * vars, plus a constant offset.
 *)
type level_exp_var = { le_var : string; le_offset : int }

type level_exp = { le_const : int; le_vars : level_exp_var list }

(*
 * Parameters have a number of simple types.
 *)
type param =
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
type operator = { op_name : opname; op_params : param list }


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
   Term of term_nods |
   Subst of term * term_subst
and term = { free_vars : StringSet.t; mutable core : term_core }
and bound_term_core = 
   BTerm of bound_term_nods |
   BSubst of bound_term * term_subst
and bound_term = { bfree_vars : StringSet.t; mutable bcore: bound_term_core }
and term_nods = { term_op : operator; term_terms : bound_term list } 
and bound_term_nods = { bvars : string list; bterm : term }

(*
 * Address of a subterm.
 *)
type address =
   Path of int list
 | NthPath of int * bool

(************************************************************************
 * De/Constructors                                                      *
 ************************************************************************)

(* Errors during matching *)
exception TermMatch of string * term * string

(*
 * General interface.
 *)

(*
 * subst: simultaneous subst of terms for vars.
 * var_subst: subst of var for a term.
 *)
val do_term_subst : term_subst -> term -> term
val subst : term -> term list -> string list -> term
val do_bterm_subst : term_subst -> bound_term -> bound_term
val var_subst : term -> term -> string -> term

val dest_term : term -> term_nods
val make_term : term_nods -> term
val mk_op : opname -> param list -> operator
val mk_term : operator -> bound_term list -> term
val mk_bterm : string list -> term -> bound_term
val make_bterm : bound_term_nods -> bound_term
val dest_bterm : bound_term -> bound_term_nods
val mk_level : int -> level_exp_var list -> level_exp
val mk_level_var : string -> int -> level_exp_var

val normalize_term : term -> term

(* Addresses *)
val string_of_address : address -> string
val make_address : int list -> address
val make_seq_address : int -> address
val nth_cdr_addr : int -> address

(* Projections *)
val opname_of_term : term -> opname
val subterms_of_term : term -> term list

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
 * Term equality
 *)
val equal_params : param -> param -> bool
val alpha_equal : term -> term -> bool
val alpha_equal_vars : (term * string list) -> (term * string list) -> bool
val alpha_equal_match : (term * string list) ->
       (term * string list * string list * term list) ->
       bool

(*
 * Get the list of free variables.
 *)
val is_free_var : string -> term -> bool
val free_vars : term -> string list
val free_vars_terms : term list -> string list
val context_vars : term -> string list
val binding_vars : term -> string list

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
val is_var_term_nods : term_nods -> bool
val is_var_term : term -> bool
val dest_var_nods : term_nods -> string
val dest_var : term -> string
val mk_var_term : string -> term
val mk_var_op : string -> operator

val is_so_var_term : term -> bool
val dest_so_var : term -> string * term list
val mk_so_var_term : string -> term list -> term

val is_context_term : term -> bool
val dest_context : term -> string * term * term list
val mk_context_term : string -> term -> term list -> term

(************************************************************************
 * Shapes                                                               *
 ************************************************************************)

(*
 * "Shape" of the term is useful for indexing purposes.  
 *)
type shape =
   { shape_opname : opname;
     shape_params : shape_param list;
     shape_arities : int list
   }

and shape_param =
   ShapeNumber
 | ShapeString
 | ShapeToken
 | ShapeLevel
 | ShapeVar

val shape_of_term : term -> shape
val print_shape : out_channel -> shape -> unit

