(*
 * This is the simple term module, where the
 * implementation of the term mirrors the interface.
 * Destructors are identity functions.
 *)

open Printf

open Nl_debug
open Opname
open Refine_error_sig

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term%t" eflush

(*
 * Type of terms.
 *)
module TermType =
struct
   (*
    * The type are just the naive types.
    *)
   type level_exp_var = level_exp_var'
   and level_exp = level_exp'
   and param = param'
   and operator = operator'

   and term = term'
   and bound_term = bound_term'

   (*
    * Level expression have offsets from level expression
    * vars, plus a constant offset.
    *)
   and level_exp_var' = { le_var : string; le_offset : int }

   and level_exp' = { le_const : int; le_vars : level_exp_var list }

   (*
    * Parameters have a number of simple types.
    *)
   and param' =
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

   (*
    * An operator combines a name with a list of parameters.
    * The order of params is significant.
    *)
   and object_id = param list

   and operator' = { op_name : opname; op_params : param list }

   (*
    * A term has an operator, and a finite number of subterms
    * that may be bound.
    *)
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
      let init = Array.init
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
      let init = Array.init
      let to_list = Array.to_list
      let of_list = Array.of_list
   end

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
