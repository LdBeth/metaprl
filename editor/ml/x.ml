(*
 * Testing.
 *)

open Printf;;
open Debug;;

open Refiner.Refiner.Term;;
open Refiner.Refiner.TermAddr;;
open Refiner.Refiner.Refine;;

open Tacticals;;

open Base_rewrite;;
open Itt_rfun;;
open Itt_int;;

open Shell;;
open Test;;

load "test";;
cd "test.test";;

(*
 refine rwtactic (rwaddr (make_address [0;0;0;0]) (andthenrw betaReduction reduceAdd));; 
 down 0;;
*)

(*
 * $Log$
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
