(*
 * This file (along with the tm_*_sig.mlz files)
 * describes the minimal term module functionality
 * necessary for the Term_copy module
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)

open Term_sig
open Term_base_minimal_sig
open Term_subst_minimal_sig
open Term_man_minimal_sig
open Refine_minimal_sig
open Term_hash_sig
open Term_norm_sig
open Weak_memo
open Termmod_sig

module type TermModuleHashSig =
sig
   (*
    * Terms and operations on terms.
    *)
   module TermType : TermSig
   module Term : TermBaseMinimalSig
      with type level_exp_var = TermType.level_exp_var
      with type level_exp = TermType.level_exp
      with type param = TermType.param
      with type operator = TermType.operator
      with type term = TermType.term
      with type bound_term = TermType.bound_term
      with type seq_hyps = TermType.seq_hyps
      with type seq_goals = TermType.seq_goals

      with type hypothesis = TermType.hypothesis
      with type level_exp_var' = TermType.level_exp_var'
      with type level_exp' = TermType.level_exp'
      with type param' = TermType.param'
      with type operator' = TermType.operator'
      with type term' = TermType.term'
      with type bound_term' = TermType.bound_term'

   module TermSubst : TermSubstMinimalSig
      with type param = TermType.param

   module TermMan : TermManMinimalSig
      with type term = TermType.term
      with type esequent = TermType.esequent

   module Refine : RefineMinimalSig
      with type term = TermType.term

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

