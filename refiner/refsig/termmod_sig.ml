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

module type TermModuleSig =
sig
   (*
    * Terms and operations on terms.
    *)
   module TermType : TermSig
   module Term : TermBaseMinimalSig
      with module TermTypes = TermType

   module TermSubst: TermSubstMinimalSig
      with type param = TermType.param

   module TermMan : TermManMinimalSig
      with type term = TermType.term
      with type esequent = TermType.esequent
      with type hypothesis = TermType.hypothesis

   module TermMeta : sig  (* XXX HACK: TermMan is here only for ASCII IO format versions <= 1.0.7 support *)
      val term_of_parsed_term : TermType.term -> TermType.term
   end

   (*
    * Refiner is included because it defines msequent.
    *)
   module Refine : RefineMinimalSig
      with type term = TermType.term
end

module type TermModuleInternalSig =
sig
   (*
    * Terms and operations on terms.
    *)
   module TermType : TermSig
   module Term : TermBaseInternalSig
      with module TermTypes = TermType

   module TermSubst: TermSubstMinimalSig
      with type param = TermType.param

   module TermMan : TermManMinimalSig
      with type term = TermType.term
      with type esequent = TermType.esequent
      with type hypothesis = TermType.hypothesis

   module TermMeta : sig  (* XXX HACK: TermMan is here only for ASCII IO format versions <= 1.0.7 support *)
      val term_of_parsed_term : TermType.term -> TermType.term
   end

   (*
    * Refiner is included because it defines msequent.
    *)
   module Refine : RefineMinimalSig
      with type term = TermType.term
end

