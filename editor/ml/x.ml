(*
 * Testing.
 *)

open Printf;;
open Debug;;

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
 * $Log$
 * Revision 1.5  1998/06/16 16:14:13  nogin
 * Added set_writable ()
 *
 * Revision 1.4  1998/06/12 20:46:02  jyh
 * Switched to term_ds.
 *
 * Revision 1.3  1998/06/12 18:36:18  jyh
 * Working factorial proof.
 *
 * Revision 1.2  1998/06/12 13:45:20  jyh
 * D tactic works, added itt_bool.
 *
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
