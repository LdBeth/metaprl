(*
 * This is the simple term module, where the
 * implementation of the term mirrors the interface.
 * Destructors are identity functions.
 *)

open Printf

open Debug
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
end

(*
 * $Log$
 * Revision 1.13  1998/07/02 22:25:15  jyh
 * Created term_copy module to copy and normalize terms.
 *
 * Revision 1.12  1998/07/02 18:36:37  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.11  1998/06/22 19:45:57  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
 * Revision 1.10  1998/06/15 22:53:56  nogin
 * Use == for comparing opnames
 *
 * Revision 1.9  1998/06/15 21:57:21  jyh
 * Added a few new functions.
 *
 * Revision 1.8  1998/06/12 13:47:10  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.7  1998/06/03 22:19:37  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.6  1998/06/03 15:23:58  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.5  1998/06/02 21:52:00  nogin
 * Use == for comparing opnames
 *
 * Revision 1.4  1998/06/01 19:53:51  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.3  1998/06/01 13:55:32  jyh
 * Proving twice one is two.
 *
 * Revision 1.2  1998/05/30 19:18:48  nogin
 * Eliminated white space in empty lines.
 *
 * Revision 1.1  1998/05/28 15:02:41  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:54  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
