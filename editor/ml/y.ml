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

open Nl
open Test

open Czf_itt_set
open Czf_itt_eq
open Czf_itt_member
open Czf_itt_sep
open Czf_itt_union
open Czf_itt_map
open Czf_itt_all
open Czf_itt_exists
open Czf_itt_dall
open Czf_itt_dexists
open Czf_itt_sall
open Czf_itt_sexists
open Czf_itt_rel

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

let _ = load "czf_itt_axioms"
let _ = cd "czf_itt_axioms.subset_collection"
let _ = set_writeable ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
