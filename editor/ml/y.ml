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

open Typeinf
open Base_dtactic
open Base_auto_tactic
open Itt_rfun
open Itt_fun
open Itt_int
open Itt_logic
open Itt_dprod
open Itt_union
open Itt_equal
open Itt_struct
open Itt_w
open Itt_derive

open Shell
open Test

open Czf_itt_set
open Czf_itt_eq
open Czf_itt_member
open Czf_itt_sep
open Czf_itt_all

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

(*
let _ = load "itt_logic_var"
let _ = cd "itt_logic_var.pairFormation"
let _ = load "itt_derive"
let _ = cd "itt_derive.independentApplyIntro2"
*)
let _ = load "czf_itt_all"
let _ = cd "czf_itt_all.dfun_res2"
let _ = set_writeable ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
