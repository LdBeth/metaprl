(*
 * Testing.
 *)

open Printf
open Debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.Refine

open Tacticals
open Conversionals

open Base_dtactic
open Base_rewrite
open Itt_rfun
open Itt_fun
open Itt_int
open Itt_union
open Itt_equal
open Itt_struct
open Itt_w

open Shell
open Test

open Czf_itt_small
open Czf_itt_set
(*
open Czf_itt_true
open Czf_itt_false
open Czf_itt_or
open Czf_itt_and
open Czf_itt_implies
open Czf_itt_all
open Czf_itt_exists
open Czf_itt_dall
open Czf_itt_dexists
open Czf_itt_set_ind
*)

(*
 * Proof saving.
 *)
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

let _ = load "czf_itt_set"
let _ = cd "czf_itt_set.set_elim"
let _ = set_writeable ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
