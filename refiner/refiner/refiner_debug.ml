(*
 * Run two refiners in parallel for debugging purposes.
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
 * Author: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Term_sig
open Refiner_sig
open Termmod_sig

open Lm_symbol
open Opname

module MakeRefinerDebug (Refiner1 : RefinerSig) (Refiner2 : RefinerSig) = struct
   module TermType = struct
      type term = Refiner1.TermType.term * Refiner2.TermType.term
      type bound_term = Refiner1.TermType.bound_term * Refiner2.TermType.bound_term
      type operator = Refiner1.TermType.operator * Refiner2.TermType.operator
      type param = Refiner1.TermType.param * Refiner2.TermType.param
      type address = Refiner1.TermAddr.address * Refiner2.TermAddr.address
      type level_exp_var = Refiner1.TermType.level_exp_var * Refiner2.TermType.level_exp_var
      type level_exp = Refiner1.TermType.level_exp * Refiner2.TermType.level_exp
      type seq_hyps = Refiner1.TermType.seq_hyps * Refiner2.TermType.seq_hyps

      type level_exp_var' = { le_var : var; le_offset : int }
      type level_exp' = { le_const : int; le_vars : level_exp_var list }
      type operator' = { op_name : opname; op_params : param list }
      type term' = { term_op : operator; term_terms : bound_term list }
      type bound_term' = { bvars : var list; bterm : term }
      type object_id = param list
      type param' = (level_exp, param) poly_param
      type meta_term = term poly_meta_term
      type hypothesis = term poly_hypothesis
      type esequent = { sequent_args : term; sequent_hyps : seq_hyps; sequent_concl : term }

      type match_param =
         MatchNumber of Lm_num.num * int option
       | MatchString of string
       | MatchToken of string
       | MatchVar of var
       | MatchLevel of level_exp
       | MatchUnsupported

      type match_term =
         MatchTerm of string list * match_param list * bound_term' list
       | MatchSequent of string list * match_term list * hypothesis list * term

   end

   open TermType

   (* Helper functions *)
   module TermT1 = Refiner1.TermType
   module TermT2 = Refiner2.TermType
   module Term1 = Refiner1.Term
   module Term2 = Refiner2.Term

   (*
    * We use a separate error reporting function to have a single breakpoint
    * location that can be used to catch _all_ error in the debugger
    *)
   let report_error x msg =
      raise (Invalid_argument ("Found a mismatch in function " ^ x ^ ": " ^ msg))

   let split = List.split

   let split_term' { term_op = (op1, op2); term_terms = btl } =
      let btl1, btl2 = split btl in
         { TermT1.term_op = op1; TermT1.term_terms = btl1 },
         { TermT2.term_op = op2; TermT2.term_terms = btl2 }

   let merge_term x t1 t2 =
      if not (Opname.eq (Term1.opname_of_term t1) (Term2.opname_of_term t2)) then
         report_error x "opname mismatch"
      else
         (t1, t2)

   let merge_terms x tl1 tl2 =
      if not (List.length tl1 = List.length tl2) then
         report_error x "length mismatch"
      else
         List.map2 (merge_term x) tl1 tl2

   module Term = struct
      module TermTypes = TermType
      module SeqHyp = struct
         type elt = hypothesis
         type t = seq_hyps
      end

      let mk_term (o1, o2) btl =
         let btl1, btl2 = split btl in
            merge_term "Term.mk_term" (Term1.mk_term o1 btl1) (Term2.mk_term o2 btl2)

      let make_term t' =
         let t1, t2 = split_term' t' in
            merge_term "Term.make_term" (Term1.make_term t1) (Term2.make_term t2)
   end

   module TermOp = struct
      module OpTypes = TermType
   end

   module TermAddr = struct
      module AddrTypes = TermType
   end

   module TermMan = struct
      module ManTypes = TermType
   end

   module TermSubst = struct
      module SubstTypes = TermType
   end

   module TermShape = struct
      include TermType
   end

   module TermMeta = struct
      module MetaTypes = TermType
   end

   module TermEval = struct
      type term = TermType.term
   end

   module RefineError = struct
      module ErrTypes = struct
         module Types = TermType
         type address = TermType.address
      end
   end

   module Rewrite = struct
      include TermType
   end

   module Refine = struct
      include TermType
   end

   module TermHash = struct
      include TermType
   end

   module TermNorm = struct
      include TermType
   end

   module TermHeaderConstr (FromTerm : TermModuleSig) = struct

   end


end

