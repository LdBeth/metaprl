(*
 * Manifest terms.
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
 * Authors: Jason Hickey, Alexey Nogin
 *)

open Refine_error_sig
open Term_sig
open Term_base_sig
open Term_op_sig
open Term_subst_sig
open Term_man_gen_sig

module TermMan (**)
   (Term : TermSig)
   (TermBase : TermBaseSig with module TermTypes = Term)
   (TermOp : TermOpSig with module OpTypes = Term)
   (TermSubst : TermSubstSig with module SubstTypes = Term)
   (RefineError : RefineErrorSig with module Types = Term)
: (TermManGenSig with module ManTypes = Term)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
