(*
 * Define terms.  This is the standard straighforward definition.
 *)

open Opname
open Term_sig

module Term :
sig
   (************************************************************************
    * Type definitions                                                     *
    ************************************************************************)

   (*
    * The type are just the naive types.
    *)
   type level_exp_var = level_exp_var'
   and level_exp = level_exp'
   and param = param'
   and operator =  { mutable imp_op_name : opname; imp_op_params : param list }

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

   (* Errors during matching *)
   exception TermMatch of string * term * string

   exception BadMatch of term * term

   (************************************************************************
    * De/Constructors                                                      *
    ************************************************************************)

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

   val normalize_term : term -> unit

   (* Projections *)
   val opname_of_term : term -> opname
   val subterms_of_term : term -> term list
   val subterm_count : term -> int
   val subterm_arities : term -> int list

   (*
    * A variable is a term with opname "var", and a single
    * var parameter that is the name of the variable.
    *)
   val var_opname : opname
   val context_opname : opname

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
    * Simple terms have no paramaters and
    * all subterms have no binding vars.
    *)
   val mk_any_term : operator -> term list -> term
   val mk_simple_term : opname -> term list -> term
   val dest_simple_term : term -> (opname * term list)
   val is_simple_term_opname : opname -> term -> bool
   val dest_simple_term_opname : opname -> term -> term list

   val mk_simple_bterm : term -> bound_term
   val dest_simple_bterm : term -> bound_term -> term
end

(*
 * $Log$
 * Revision 1.6  1998/06/15 21:57:22  jyh
 * Added a few new functions.
 *
 * Revision 1.5  1998/06/03 22:19:38  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.4  1998/06/03 15:23:59  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.3  1998/06/01 19:53:52  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.2  1998/05/30 19:18:49  nogin
 * Eliminated white space in empty lines.
 *
 * Revision 1.1  1998/05/28 15:02:43  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:56  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
