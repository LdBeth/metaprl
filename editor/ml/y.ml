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

open Tptp
open Tptp_prove

open Nl

load "tptp_prove";;
cd "tptp_prove";;
set_writeable ();;
(*
create_tptp "BOO008-3";;
cd "BOO008-3";;
 *)
create_tptp "GEN";;
cd "GEN";;

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
