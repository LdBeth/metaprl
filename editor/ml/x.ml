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
open Itt_equal
open Itt_struct
open Itt_logic
open Itt_dprod
open Itt_prod

open Nl
open Test

let zT, z =
   let pf = ref None in
   let zT p =
      pf := Some p;
      idT p
   in
   let z () =
      match !pf with
         Some p ->
            p
       | None ->
            raise Not_found
   in
      zT, z

(*
let _ = load "test"
let _ = cd "test.test_and_elim"
        *)
let _ = load "itt_logic"
let _ = cd "itt_logic.cor_elim"

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
