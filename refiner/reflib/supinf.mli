open Lm_printf
open Refiner.Refiner.TermType

module type BoundFieldSig =
sig
   type bfield

   val fieldUnit : bfield
   val fieldZero : bfield
   val plusInfinity : bfield
   val minusInfinity : bfield
   val mul : bfield -> bfield -> bfield
   val add : bfield -> bfield -> bfield
   val neg : bfield -> bfield
   val sub : bfield -> bfield -> bfield
   val inv : bfield -> bfield
   val div : bfield -> bfield -> bfield
   val compare : bfield -> bfield -> int
   val isInfinite : bfield -> bool

   val term_of : bfield -> term
   val mul_term : term -> term -> term
   val add_term : term -> term -> term
   val neg_term : term -> term
   val sub_term : term -> term -> term
   val inv_term : term -> term
   val div_term : term -> term -> term
   val ge_term : term -> term -> term
   val max_term : term -> term -> term
   val min_term : term -> term -> term

   val print : out_channel -> bfield -> unit
end

module VarType :
sig
   type t=int
   val compare : t -> t -> int

   val print : out_channel -> t -> unit
end

module type AF_Sig =
sig
   type vars=int
   type af
   type bfield

   val constvar : vars

   val mk_number: bfield -> af
   val mk_var: vars -> af
   val scale: bfield -> af -> af
   val add: af -> af -> af

   val coef: af -> vars -> bfield
   val remove: af -> vars -> af
   val split: af -> (bfield * vars * af)
   val isNumber: af -> bool
   val isInfinite: af -> bool

   val minusInfinity : af
   val plusInfinity : af

   val term_of : (term array) -> af -> term

   val print : out_channel -> af -> unit
   val print_var : out_channel -> vars -> unit
end

module MakeAF(BField : BoundFieldSig)
	: AF_Sig with type bfield=BField.bfield and type vars=VarType.t

module type SAF_Sig =
sig
   type bfield
   type vars
   type af
   type saf = Affine of af | Max of saf*saf | Min of saf*saf
   type 'a step =
      Assert of string * saf * saf * 'a
    |	Transitive of string * saf * saf * saf
    | Tactic of string * 'a

   val affine: af -> saf
   val min: saf -> saf -> saf
   val max: saf -> saf -> saf

   val scale: bfield -> saf -> saf
   val add: saf -> saf -> saf

   val occurs: vars -> saf -> bool
   val isInfinite: saf -> bool
   val isAffine: saf -> bool

   val term_of: (term array) -> saf -> term

   val print : out_channel -> saf -> unit
end

module MakeSAF(BField : BoundFieldSig)(AF : AF_Sig  with type bfield=BField.bfield)
	: SAF_Sig with type bfield=BField.bfield and type vars=AF.vars and type af=AF.af
