(*
 * This is the module that implements delayed substitution,
 * keeps track of free variables and does some sharing.
 *)

open Printf
open Nl_debug
open Opname

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_ds%t" eflush

module TermType =
struct
   (************************************************************************
    * Sets of strings                                                      *
    ************************************************************************)

   module OrderedString =
      struct
         type t = string
         let compare = Pervasives.compare
       end

   module StringSet = Splay_set.Make (OrderedString)

   (************************************************************************
    * Types                                                                *
    ************************************************************************)

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
   type operator =  operator'

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

   (*
    * The terms in the framework include
    * a meta-implication and met-iff.
    *)
   type meta_term =
      MetaTheorem of term
    | MetaImplies of meta_term * meta_term
    | MetaFunction of term * meta_term * meta_term
    | MetaIff of meta_term * meta_term

   type hypothesis =
      Hypothesis of string * term
    | Context of string * term list

   type esequent =
      { sequent_args : term;
        sequent_hyps : hypothesis array;
        sequent_goals : term array
      }

   module SeqHyp =
   struct
      type elt = hypothesis
      type t = hypothesis array
      let length = Array.length
      let get = Array.get
      let create = Array.create
      let make = Array.make
      let to_list = Array.to_list
      let of_list = Array.of_list
   end

   module SeqGoal =
   struct
      type elt = term
      type t = term array
      let length = Array.length
      let create = Array.create
      let get = Array.get
      let make = Array.make
      let to_list = Array.to_list
      let of_list = Array.of_list
   end

end
