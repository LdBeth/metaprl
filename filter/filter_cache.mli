(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
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
 *)
open Refiner_io
open Refiner.Refiner.TermType

open Filter_summary_type

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * Proofs are:
 *   1. primitive terms,
 *   2. tactics.
 *   3. inferred from interactive proofs
 *)
type 'a proof_type =
   Primitive of Refiner.Refiner.TermType.term
 | Derived of MLast.expr
 | Incomplete
 | Interactive of 'a

(*
 * Proof conversion.
 *)
module type ConvertProofSig =
sig
   type t
   type raw
   type cooked

   val to_raw  : t -> string -> cooked -> raw
   val of_raw  : t -> string -> raw -> cooked
   val to_expr : t -> string -> cooked -> MLast.expr
   val to_term : t -> string -> cooked -> term
   val of_term : t -> string -> term -> cooked
   val to_term_io : t -> string -> cooked -> term_io
   val of_term_io : t -> string -> term_io -> cooked
end

(*
 * Build a cache with a particular proof type.
 *)
module MakeCaches (Convert : ConvertProofSig) :
sig
   (*
    * The summary_cache for interfaces and implementations.
    *)
   module SigFilterCache :
      SummaryCacheSig
      with type sig_proof  = unit
      with type sig_ctyp   = MLast.ctyp
      with type sig_expr   = MLast.expr
      with type sig_item   = MLast.sig_item
      with type str_proof  = unit
      with type str_ctyp   = MLast.ctyp
      with type str_expr   = MLast.expr
      with type str_item   = MLast.sig_item
      with type select     = select_type
      with type arg        = Convert.t

   module StrFilterCache :
      SummaryCacheSig
      with type sig_proof  = unit
      with type sig_ctyp   = MLast.ctyp
      with type sig_expr   = MLast.expr
      with type sig_item   = MLast.sig_item
      with type str_proof  = Convert.cooked proof_type
      with type str_ctyp   = MLast.ctyp
      with type str_expr   = MLast.expr
      with type str_item   = MLast.str_item
      with type select     = select_type
      with type arg        = Convert.t
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
