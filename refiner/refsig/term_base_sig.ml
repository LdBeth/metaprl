(*
 * Basic term operations.
 *)
open Opname

(*
 * We use read-only arrays for sequents.
 *)
module type ROArraySig =
sig
   type elt
   type t
   val length : t -> int
   val get : t -> int -> elt
   val make : int -> elt -> t
   val create : int -> elt -> t
   val init : int -> (int -> elt) -> t
   val mapi : (int -> elt -> elt) -> t -> t
   val append_array : t -> elt array -> t
   val append_list : t -> elt list -> t
   val to_list : t -> elt list
   val of_list : elt list -> t
   val iter : (elt -> unit) -> t -> unit

   val sub_map : (elt -> elt) -> t -> int -> int -> t
   val collect : (elt, t) Array_util.array_part list -> t
end

module type TermBaseSig =
sig
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type level_exp_var
   type level_exp
   type param
   type operator
   type term
   type bound_term
   type seq_hyps
   type seq_goals

   type hypothesis
   type level_exp_var'
   type level_exp'
   type object_id
   type param'
   type operator'
   type term'
   type bound_term'

   module SeqHyp : ROArraySig with type elt = hypothesis with type t = seq_hyps
   module SeqGoal : ROArraySig with type elt = term with type t = seq_goals

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
   val is_var_term : term -> bool
   val dest_var : term -> string
   val mk_var_term : string -> term
   val mk_var_op : string -> operator

   val is_so_var_term : term -> bool
   val dest_so_var : term -> string * term list
   val mk_so_var_term : string -> term list -> term

   val context_opname : opname
   val is_context_term : term -> bool
   val dest_context : term -> string * term * term list
   val mk_context_term : string -> term -> term list -> term

   val xperv : opname
   val sequent_opname : opname

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

   (*
    * We allow a term printer to be injected.
    *)
   val debug_print : out_channel -> term -> unit
   val install_debug_printer : (out_channel -> term -> unit) -> unit
end

