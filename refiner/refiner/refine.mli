(*
 * The refiner works on proof trees, which are trees of sequents.
 * A basic refinement takes a sequent (a "goal") and produces a
 * list of sequents (the "subgoals"), and an extract term.  The type
 * "tactic" packages refinements, and elements of tactics are
 * always "correct" in the sense that they can be reduced to steps
 * of primitive inferences.
 *
 * The refiner also tracks rewrites, and just as for tactics,
 * elements of type Rewrite are always "correct".
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *
 *)

open Term_sig
open Term_base_sig
open Term_man_sig
open Term_subst_sig
open Term_addr_sig
open Term_meta_sig
open Term_shape_sig
open Refine_error_sig
open Rewrite_sig
open Refine_sig

module Refine (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term' = TermType.term'
    with type term = TermType.term
    with type seq_hyps = TermType.seq_hyps
    with type seq_goals = TermType.seq_goals
    with type bound_term' = TermType.bound_term'
    with type bound_term = TermType.bound_term)
   (TermMan : TermManSig
    with type hypothesis = Term.hypothesis
    with type esequent = TermType.esequent
    with type term = TermType.term)
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (TermAddr : TermAddrSig
    with type term = TermType.term)
   (TermMeta : TermMetaSig
    with type term = TermType.term)
   (TermShape : TermShapeSig
    with type term = TermType.term)
   (Rewrite : RewriteSig
    with type term = TermType.term
    with type address = TermAddr.address)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = TermAddr.address
    with type meta_term = TermMeta.meta_term)
: RefineSig
  with type term = TermType.term
  with type address = TermAddr.address
  with type meta_term = TermMeta.meta_term

