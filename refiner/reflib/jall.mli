open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermSubst


open Jlogic_sig

module JProver(JLogic: JLogicSig) :
sig
  val test : Refiner.Refiner.Term.term -> string -> string -> unit  
             (* sequent calculus, another argumnet for proof reconstruction *)
  val seqtest : Refiner.Refiner.Term.term -> string -> string -> unit  
   (* input a list_term of hyps,concl, where hyps is a list of terms and concl is the goal *)
  val prover : Refiner.Refiner.Term.term list * Refiner.Refiner.Term.term 
                -> (string * Refiner.Refiner.Term.term * Refiner.Refiner.Term.term) list 
end

   

