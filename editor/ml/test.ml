(*
 * Display all the elements in a particular theory.
 *)

include Itt_prop_decide

open Printf
open Nl_debug

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Itt_logic
open Itt_struct
open Itt_prop_decide
open Tacticals
open Base_auto_tactic
open Base_dtactic
open Conversionals
open Var

interactive test1 'H : :
   sequent ['ext] { 'H >- (('A or 'B) => 'C) => (('A => 'C) & ('B => 'C)) }

interactive test2 'H : :
   sequent ['ext] { 'H >- (('A or 'B) => 'C) => (('A => 'C) & ('B => 'C)) }

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
