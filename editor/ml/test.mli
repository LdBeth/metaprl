(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Itt_logic
open Itt_struct
open Tacticals
open Base_auto_tactic
open Base_dtactic


val proveIntT : tactic

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
