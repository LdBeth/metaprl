(*
 * Testing.
 *)

open Printf
open Nl_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.Refine

open Tacticals
open Conversionals

open Base_rewrite
open Base_dtactic
open Base_auto_tactic
open Itt_rfun
open Itt_int
open Itt_equal
open Itt_struct
open Itt_logic
open Itt_dprod
open Itt_prod

open Nl
open Test

let _ = set_debug "prove_int" true

let zi i =
  (let z = goal() in
     if (i=0) then Sequent.concl z
     else          snd(Sequent.nth_hyp z i))

let _ = load "test"
let _ = cd "test"

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
