(*
 * Define terms.  This is the abstract term interface.
 * There may be several implementations of terms.
 *)

open Opname

module type TermSig =
sig
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
   type object_id = param list

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
   type bound_term' = { bvars : string list; bterm : term }

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

   val normalize_term : term -> term

   (* Projections *)
   val opname_of_term : term -> opname
   val subterms_of_term : term -> term list

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
    * Simple terms have no paramaters and
    * all subterms have no binding vars.
    *)
   val mk_any_term : operator -> term list -> term
   val mk_simple_term : opname -> term list -> term
   val dest_simple_term : term -> (opname * term list)
   val is_simple_term_opname : opname -> term -> bool
   val dest_simple_term_opname : opname -> term -> term list
end

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:14:50  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
