open Lm_printf

open Refiner.Refiner.TermType

module type RingSig =
sig
   type ring

   val ringUnit : ring
   val ringZero : ring
   val abs : ring -> ring
   val mul : ring -> ring -> ring
   val div : ring -> ring -> ring
   val rem : ring -> ring -> ring
   val add : ring -> ring -> ring
   val neg : ring -> ring
   val sub : ring -> ring -> ring
   val compare : ring -> ring -> int
   val equal : ring -> ring -> bool
   val isNegative : ring -> bool
   val gcd : ring -> ring -> ring

   val term_of : ring -> term
   val mul_term : term -> term -> term
   val add_term : term -> term -> term
   val neg_term : term -> term
   val sub_term : term -> term -> term
   val ge_term : term -> term -> term

   val print : out_channel -> ring -> unit
end

module VarType :
sig
   type t=int
   val compare : t -> t -> int

   val print : out_channel -> t -> unit
end

module type Index_Sig =
sig
   type t
   val create : int -> t
   val length : t -> int
   val lookup : t -> term -> int
   val print : out_channel -> t -> unit
   val invert : t -> term array
   val restore : term array -> int -> term
end

module type AF_Sig =
sig
   module VI : Index_Sig

   type ring
   type vars=int
   type af

   val constvar : vars

   val dim : af -> int
   val mk_number: int -> ring -> af
   val mk_var: int -> vars -> af
   val grow: int -> af -> af
   val scale: ring -> af -> af
   val div: af -> ring -> af
   val add: af -> af -> af
   val sub: af -> af -> af
   val add_scaled: af -> ring -> af -> af
   val sub_scaled: af -> ring -> af -> af
   val sub_number : af -> ring -> af

   val coef: af -> vars -> ring
   val get: af -> vars -> ring
   val remove: af -> vars -> af
   val split: af -> (ring * vars * af)
   val any_var : af -> vars
   val isNumber: af -> bool
   val gcd: af -> ring

   val value_of : af -> ring
   val term_of : (term array) -> af -> term

   val print : out_channel -> af -> unit
   val print_var : out_channel -> vars -> unit
end

module MakeAF(Var : Hashtbl.HashedType with type t = term)(Ring : RingSig)
   : AF_Sig with
   type ring=Ring.ring and
   type vars=VarType.t
