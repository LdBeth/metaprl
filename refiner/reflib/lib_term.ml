(*
 * Term operations for library.
 * Convert sequents to regular terms.
 *)

open Opname
open Term_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term

module TermManGen = Term_man_gen.TermMan(Refiner.Refiner.TermType)(Refiner.Refiner.Term)(Refiner.Refiner.TermOp)(Refiner.Refiner.TermSubst)(Refiner.Refiner.RefineError)

let ugly_term_of_sequent_term t =
   TermManGen.mk_sequent_term (TermMan.explode_sequent t)

let sequent_term_of_ugly_term t =
   TermMan.mk_sequent_term (TermManGen.explode_sequent t)

let mk_term op bterms =
   if Opname.eq (dest_op op).op_name sequent_opname then
      sequent_term_of_ugly_term (mk_term op bterms)
   else
      Term.mk_term op bterms

let make_term { term_op = op; term_terms = bterms } =
   mk_term op bterms

let dest_term t =
   if TermMan.is_sequent_term t then
      dest_term (ugly_term_of_sequent_term t)
   else
      dest_term t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
