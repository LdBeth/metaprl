open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermSubst

open Jlogic_sig

val ruletable : rule -> string

module JProver(JLogic: JLogicSig) :
sig
  
  val test : term -> string -> string -> unit  

  (* sequent calculus, another argumnet for proof reconstruction *)
  val seqtest : term -> string -> string -> unit  

  (* input a list_term of hyps,concl, where hyps is a list of terms and concl is the goal *)
  val prover : term list -> term -> JLogic.inference
end

   

