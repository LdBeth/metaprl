(*
 * Substituion, alpha equality, and unification.
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
 * Authors: Alexey Nogin
 *)

open Refine_error_sig
open Term_subst_sig
open Term_ds_sig
open Term_ds

module TermSubst
   (Term : TermDsSig with module TermTypes = TermType)
   (RefineError : RefineErrorSig
    with type ErrTypes.Types.level_exp = TermType.level_exp
    with type ErrTypes.Types.param = TermType.param
    with type ErrTypes.Types.term = TermType.term
    with type ErrTypes.Types.bound_term = TermType.bound_term)
: (TermSubstSig with module SubstTypes = TermType)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
