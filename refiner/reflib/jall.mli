open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermType


open Jlogic_sig

module JProver(JLogic: JLogicSig) :
sig
  val test :  Refiner.Refiner.Term.term -> string -> string -> unit  
             (* sequent calculus, another argumnet for proof reconstruction *)
  val prover :  Refiner.Refiner.Term.term -> (string * Refiner.Refiner.Term.term * Refiner.Refiner.Term.term) list 
end

   

