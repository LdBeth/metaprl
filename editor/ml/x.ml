(*
 * Testing.
 *)

open Printf;;
open Debug;;

open Refiner.Refiner.Term;;
open Refiner.Refiner.TermAddr;;
open Refiner.Refiner.Refine;;

open Tacticals;;
open Conversionals;;

open Base_rewrite;;
open Itt_rfun;;
open Itt_int;;

open Shell;;
open Test;;

load "test";;
cd "test.test";;

(*
refine rw (higherC (betaReduction andthenC reduceAdd)) 0;;
down 0;;
*)

(*
 * $Log$
 * Revision 1.2  1998/06/12 13:45:20  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.1  1998/06/09 20:51:22  jyh
 * Propagated refinement changes.
 * New tacticals module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
