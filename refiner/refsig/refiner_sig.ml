(*
 * This is a complete refiner.
 *)

open Term_sig
open Term_base_sig
open Term_op_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_shape_sig
open Term_eval_sig
open Term_meta_sig
open Refine_error_sig
open Rewrite_sig
open Refine_sig

module type RefinerSig =
sig
   (*
    * Terms and operations on terms.
    *)
   module TermType : TermSig
   module Term : TermBaseSig
      with type level_exp_var = TermType.level_exp_var
      with type level_exp = TermType.level_exp
      with type param = TermType.param
      with type operator = TermType.operator
      with type term = TermType.term
      with type bound_term = TermType.bound_term

      with type level_exp_var' = TermType.level_exp_var'
      with type level_exp' = TermType.level_exp'
      with type object_id = TermType.object_id
      with type param' = TermType.param'
      with type operator' = TermType.operator'
      with type term' = TermType.term'
      with type bound_term' = TermType.bound_term'

   module TermOp : TermOpSig
                   with type term = TermType.term
                   with type operator = TermType.operator
                   with type level_exp = TermType.level_exp
   module TermAddr : TermAddrSig
                     with type term = TermType.term
   module TermMan : TermManSig
                    with type term = TermType.term
                    with type operator = TermType.operator
                    with type level_exp = TermType.level_exp
                    with type address = TermAddr.address
                    with type esequent = TermType.esequent
   module TermSubst : TermSubstSig
                      with type term = TermType.term
                      with type param = TermType.param
   module TermShape : TermShapeSig
                      with type term = TermType.term
   module TermMeta : TermMetaSig
                     with type term = TermType.term
                     with type meta_term = TermType.meta_term
   module TermEval : TermEvalSig
                     with type term = TermType.term

   (*
    * Rewriting and refinement.
    *)
   module RefineError : RefineErrorSig
                        with type level_exp = TermType.level_exp
                        with type param = TermType.param
                        with type term = TermType.term
                        with type bound_term = TermType.bound_term
                        with type address = TermAddr.address
                        with type meta_term = TermMeta.meta_term

   module Rewrite : RewriteSig
                    with type term = TermType.term
                    with type level_exp = TermType.level_exp
                    with type operator = TermType.operator
                    with type address = TermAddr.address

   module Refine : RefineSig
                   with type term = TermType.term
                   with type address = TermAddr.address
                   with type meta_term = TermMeta.meta_term
end

