(*
 * This is a complete refiner.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Authors: Jason Hickey, Alexey Nogin
 * jyh@cs.cornell.edu
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
open Term_hash_sig
open Term_norm_sig
open Refine_error_sig
open Rewrite_sig
open Refine_sig
open Termmod_sig

module type RefinerSig =
sig
   (*
    * Terms and operations on terms.
    *)
   module TermType : TermSig
   module Term : TermBaseSig with module TermTypes = TermType
   module TermOp : TermOpSig with module OpTypes = TermType
   module TermAddr : TermAddrSig with module AddrTypes = TermType
   module TermMan : TermManSig with module ManTypes = TermType
   module TermSubst : TermSubstSig with module SubstTypes = TermType
   module TermShape : TermShapeSig
                      with type term = TermType.term
                      with type param = TermType.param
   module TermMeta : TermMetaSig with module MetaTypes = TermType
   module TermEval : TermEvalSig with type term = TermType.term

   (*
    * Rewriting and refinement.
    *)
   module RefineError : RefineErrorSig
                        with type level_exp = TermType.level_exp
                        with type param = TermType.param
                        with type term = TermType.term
                        with type bound_term = TermType.bound_term
                        with type meta_term = TermType.meta_term
                        with type seq_hyps = TermType.seq_hyps
                        with type seq_goals = TermType.seq_goals
                        with type address = TermAddr.address

   module Rewrite : RewriteSig
                    with type term = TermType.term
                    with type level_exp = TermType.level_exp
                    with type operator = TermType.operator
                    with type address = TermAddr.address

   module Refine : RefineSig
                   with type term = TermType.term
                   with type meta_term = TermType.meta_term
                   with type address = TermAddr.address

   (*
    * Hashing of terms.
    *)
   module TermHash : TermHashSig
      with type param = TermType.param
      with type param' = TermType.param'
      with type term = TermType.term
      with type meta_term = TermType.meta_term
      with type msequent = Refine .msequent

   module TermNorm : TermNormSig
      with type t = TermHash.t
      with type term = TermType.term
      with type term_index = TermHash.term_index
      with type meta_term = TermType.meta_term
      with type meta_term_index = TermHash.meta_term_index
      with type msequent = Refine .msequent
      with type msequent_index = TermHash.msequent_index

   module TermHeaderConstr (FromTerm : TermModuleSig) :
   sig
      val make_term_header : TermHash.t -> FromTerm.TermType.term -> TermHash.term_header
      val make_meta_term_header : TermHash.t -> FromTerm.TermType.meta_term -> TermHash.meta_term_header
      val make_msequent_header : TermHash.t -> FromTerm.Refine .msequent -> TermHash.msequent_header
   end
end
