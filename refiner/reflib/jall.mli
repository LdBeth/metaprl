open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermType


open Jlogic_sig

module JProver(JLogic: JLogicSig) :
sig
  val test :  Refiner.Refiner.TermSubst.term -> string -> unit
  val prove : Refiner.Refiner.TermSubst.term ->  string -> 
    (int * (string * string list) list) * (string * string) list * (string * string) list
end
