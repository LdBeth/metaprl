open Refiner.Refiner
open Term
open TermOp
open TermType
open TermSubst
open Opname



module type JLogicSig =
sig
	val is_all_term	: term -> bool
	val dest_all : term -> string * term * term
	val is_exists_term : term -> bool
	val dest_exists : term -> string * term * term
	val is_and_term : term -> bool
	val dest_and : term -> term * term
	val is_or_term : term -> bool
	val dest_or : term -> term * term
	val is_implies_term : term -> bool
	val dest_implies : term -> term * term
	val is_not_term : term -> bool
	val dest_not : term -> term 
end
