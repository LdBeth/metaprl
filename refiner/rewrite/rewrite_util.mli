(*
 * Utilities used in the rewriter.
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

open Term_sig
open Term_base_sig
open Term_addr_sig
open Refine_error_sig

open Rewrite_util_sig

module MakeRewriteUtil
   (TermType : TermSig)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (Term : TermBaseSig with module TermTypes = TermType)
   (RefineError : RefineErrorSig with module ErrTypes.Types = TermType):
   RewriteUtilSig
   with type term = TermType.term
   with type rstack = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr).rstack

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
