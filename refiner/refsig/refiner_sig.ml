(*
 * This is a complete refiner.
 *)

open Term_sig
open Term_op_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_shape_sig
open Term_eval_sig
open Term_meta_sig
open Rewrite_sig
open Refine_sig

module type RefinerSig =
sig
   (*
    * Terms and operations on terms.
    *)
   module Term : TermSig
   module TermOp : TermOpSig
                   with type term = Term.term
                   with type operator = Term.operator
                   with type level_exp = Term.level_exp
   module TermMan : TermManSig
                    with type term = Term.term
                    with type operator = Term.operator
                    with type level_exp = Term.level_exp
   module TermAddr : TermAddrSig
                     with type term = Term.term
   module TermSubst : TermSubstSig
                      with type term = Term.term
                      with type param = Term.param
   module TermShape : TermShapeSig
                      with type term = Term.term
   module TermMeta : TermMetaSig
                     with type term = Term.term
   module TermEval : TermEvalSig
                     with type term = Term.term

   (*
    * Rewriting and refinement.
    *)
   module Rewrite : RewriteSig
                    with type term = Term.term
                    with type level_exp = Term.level_exp
                    with type param = Term.param
                    with type operator = Term.operator
                    with type bound_term = Term.bound_term
                    with type address = TermAddr.address

   module Refine : RefineSig
                   with type term = Term.term
                   with type address = TermAddr.address
                   with type meta_term = TermMeta.meta_term
                   with type rewrite_error = Rewrite.rewrite_error
end

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:01:37  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:02  jyh
 * Functorized the refiner over the Term module.
 *
 *)

