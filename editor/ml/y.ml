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

open Base_rewrite
open Itt_rfun
open Itt_int
open Itt_union
open Itt_equal
open Itt_struct

open Shell
open Test

open Czf_itt_set
open Czf_itt_wf
open Czf_itt_true
open Czf_itt_false
open Czf_itt_or
open Czf_itt_and
open Czf_itt_implies
open Czf_itt_all
open Czf_itt_exists
open Czf_itt_dall
open Czf_itt_dexists

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

let _ = load "czf_itt_split"
let _ = cd "czf_itt_split"
(*
let _ = load "czf_itt_axioms"
let _ = cd "czf_itt_axioms.set_induction"
*)
let _ = set_writeable ()

(*
 * $Log$
 * Revision 1.2  1998/06/23 22:12:08  jyh
 * Improved rewriter speed with conversion tree and flist.
 *
 * Revision 1.1  1998/06/22 19:45:15  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
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
