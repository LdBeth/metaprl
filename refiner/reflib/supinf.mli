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

module type SourceSig =
sig
   type bfield
   type vars

	type source =
		Signore
	 | Shypothesis of int
	 | Sleft of source
	 | Sright of source
	 | Smin of source * source
	 | Smax of source * source
	 | StransitiveLeft of source * source * vars
	 | StransitiveRight of vars * source * source
	 | Sextract2left of vars * source
	 | Sextract2right of vars * source
	 | StrivialConst of bfield
	 | StrivialVar of vars
	 | Scontradiction of source
	 | Sscale of bfield * source
	 | SaddVar of bfield * vars * source
	 | Ssum of source * source

	exception IncompatibleSources of source * source * string

	val print : out_channel -> source -> unit

end

module MakeSource(BField : BoundFieldSig)
	: SourceSig with type bfield=BField.bfield and type vars=VarType.t

module Tree :
sig
	type 'a tree = Ignore | Leaf of 'a | Left of 'a tree | Right of 'a tree | Pair of ('a tree) * ('a tree)

	val string_of_tree : 'a tree -> string

	val treeMap : ('a -> 'a) -> 'a tree -> 'a tree
	val leftBranch : 'a tree -> 'a tree
	val rightBranch : 'a tree -> 'a tree
	val treeProduct : ('a -> 'a -> 'a) -> 'a tree -> 'a tree -> 'a tree
	val treeMergeLeft : ('a -> 'a -> 'a) -> 'a tree -> 'a tree -> 'a tree
	val treeMergeRight : ('a -> 'a -> 'a) -> 'a tree -> 'a tree -> 'a tree
	val treeFlatten : 'a tree -> 'a list
end

module type AF_Sig =
sig
   type bfield
   type vars=int
   type af
	type source

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
   val isMinusInfinity: af -> bool
   val isPlusInfinity: af -> bool

   val minusInfinity : af
   val plusInfinity : af

   val term_of : (term array) -> af -> term

	val setSource : source -> af -> af
	val getSource : af -> source
	val extract2leftSource : vars -> af -> af
	val extract2rightSource : vars -> af -> af
(*	val trivialConstSource : bfield -> af
	val trivialVarSource : vars -> af
*)
	val contrSource : af -> af -> af
	val hypSource : int -> af -> af
(*	val scaleSource : bfield -> af -> af*)
	val addVarSource : bfield -> vars -> af -> af
	val sumSource : af -> af -> af

	val print : out_channel -> af -> unit
   val print_var : out_channel -> vars -> unit
end

module MakeAF(BField : BoundFieldSig)
      (Source: SourceSig with
         type bfield=BField.bfield and
         type vars=VarType.t)
   : AF_Sig with
	type bfield=BField.bfield and
	type vars=VarType.t and
	type source=Source.source

module type SAF_Sig =
sig
   type bfield
   type vars
	type source

   type af

   type saf' = Affine of af | Max of saf*saf | Min of saf*saf
	and saf = source * saf'

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
   val isMinusInfinity: saf -> bool
   val isPlusInfinity: saf -> bool
   val isAffine: saf -> bool

   val term_of: (term array) -> saf -> term

	val getSource : saf -> source
	val setSource : source -> saf -> saf
	val transitiveLeftSource : saf -> saf -> vars -> saf
	val transitiveRightSource : vars -> saf -> saf -> saf
	val addVarSource : bfield -> vars -> saf -> saf
(*	val scaleSource : bfield -> saf -> saf
	val sumSource : saf -> saf -> saf
*)
   val print : out_channel -> saf -> unit
end

module MakeSAF(BField : BoundFieldSig)
      (Source: SourceSig with
      	type bfield=BField.bfield and
      	type vars=VarType.t)
		(AF : AF_Sig with type bfield=BField.bfield and
								type source=Source.source)
	: SAF_Sig with
		type bfield=BField.bfield and
		type vars=AF.vars and
		type af=AF.af and
		type source=Source.source
