(*
 * Testing.
 *)

open Printf;;
open Nl_debug;;

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

set_writeable ();;

refine rw (higherC reduceFact) 0;;
down 0;;

(*
refine rw (repeatC (higherC redexC)) 0;;
down 0;;

refine dT 0;;
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
