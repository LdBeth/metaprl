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
open Term_subst_sig
open Term_op_sig
open Term_addr_sig
open Term_man_gen_sig

type addr

module TermAddr (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermSubst : TermSubstSig with module SubstTypes = TermType)
   (TermOp : TermOpSig with module OpTypes = TermType)
   (TermMan: TermManGenSig with module ManTypes = TermType)
   (RefineError : RefineErrorSig
    with module ErrTypes.Types = TermType
    with type ErrTypes.address = addr)
: (TermAddrSig
  with module AddrTypes = TermType
  with type address = addr)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
