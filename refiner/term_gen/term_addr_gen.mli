(*
 * Addressed operations on terms.
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
 * Copyright (C) 1998 Jason Hickey, Alexey Nogin, Cornell University
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
 * Authors: Jason Hickey, Aleksey Nogin
 *)

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_op_sig
open Term_addr_sig

type addr

module TermAddr (**)
   (TermType : TermSig)
   (Term : TermBaseSig
    with type term = TermType.term
    with type term' = TermType.term'
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term'
    with type operator = TermType.operator
    with type operator' = TermType.operator')
   (TermOp : TermOpSig
    with type term = TermType.term)
   (RefineError : RefineErrorSig
    with type term = TermType.term
    with type address = addr)
: TermAddrSig
  with type term = TermType.term
  with type address = addr

(*
 * Pseudo-addresses for sequent parts
 * Numeration starts with 1 (0 is sequent arg).
 * Second form returns the whole "tail" of the sequent, including the current named hyp.
 *)
val nth_hd_address : int -> addr
val nth_tl_address : int -> addr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
