(*
 * Debugging functions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
open Term_shape_sig
open Refine_error_sig
open Rewrite_debug_sig

module MakeRewriteDebug
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (TermShape : TermShapeSig with type term = TermType.term and type param = TermType.param)
   (RefineError : RefineErrorSig with module Types = TermType):
   RewriteDebugSig
   with type rwcontractum = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr).rwcontractum
   with type rwterm = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr).rwterm
   with type rstack = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr).rstack
   with type stack  = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr).stack
   with type varname = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr).varname

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
