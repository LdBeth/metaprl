(*
 * Term operations for library.
 * Convert sequents to regular terms.
 *)

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term

module TermManGen = Term_man_gen.TermMan(Refiner.Refiner.TermType)(Refiner.Refiner.Term)(Refiner.Refiner.TermOp)(Refiner.Refiner.TermSubst)(Refiner.Refiner.RefineError)

let mk_term op bterms =
   let { op_name = opname } = dest_op op in
      if Opname.eq opname sequent_opname then
         let t = mk_term op bterms in
         let eseq = TermManGen.explode_sequent t in
            TermMan.mk_sequent_term eseq
      else
         Term.mk_term op bterms

let make_term { term_op = op; term_terms = bterms } =
   mk_term op bterms

let dest_term t =
   if TermMan.is_sequent_term t then
      dest_term (TermManGen.mk_sequent_term(TermMan.explode_sequent t))
   else
      dest_term t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
