(*
 * Pretty printer for terms.
 *
 *)

open Rformat
open Opname
open Refiner_sig
open Simple_print_sig

module MakeSimplePrint
   (Refiner : RefinerSig)
   : SimplePrintSig 
     with type term = Refiner.TermType.term
     with type level_exp = Refiner.TermType.level_exp
     with type param = Refiner.TermType.param
     with type bound_term = Refiner.TermType.bound_term
     with type address = Refiner.TermAddr.address
     with type meta_term = Refiner.TermMeta.meta_term

module SimplePrint : SimplePrintSig
                     with type term = Refiner.Refiner.TermType.term
                     with type level_exp = Refiner.Refiner.TermType.level_exp
                     with type param = Refiner.Refiner.TermType.param
                     with type bound_term = Refiner.Refiner.TermType.bound_term
                     with type address = Refiner.Refiner.TermAddr.address
                     with type meta_term = Refiner.Refiner.TermMeta.meta_term
   
