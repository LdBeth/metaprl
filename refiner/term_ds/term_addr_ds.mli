(*
 * Addressed operations on terms.
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
 * Authors: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Refine_error_sig
open Term_ds_sig
open Term_ds
open Term_subst_sig
open Term_op_sig
open Term_addr_sig
open Term_man_sig

type addr =
   Subterm of int
 | ArgAddr
 | HypAddr of int
 | GoalAddr of int
 | Compose of addr * addr
 | Null

module TermAddr (**)
   (Term : TermDsSig with module TermTypes = TermType)
   (TermSubst : TermSubstSig
    with type term = TermType.term)
   (TermOp : TermOpSig
    with type term = TermType.term)
   (TermMan : TermManSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr)
: TermAddrSig
  with type term = TermType.term
  with type address = addr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
