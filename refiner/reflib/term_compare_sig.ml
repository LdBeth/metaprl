(*
 * This function copies a term, producing as much
 * aliasing as possible.  It is parameterized by the
 * term type, so it can be used for conversion between
 * term types.
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
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 * Derived from: Term_copy
 *)

open Termmod_sig

module type TermCompareSig =
functor (Term : TermModuleSig) ->
sig

      type c_term = CTerm of Term.TermType.term' | CSeq of Term.TermType.esequent

      val compare_level_var :
        Term.TermType.level_exp_var' -> Term.TermType.level_exp_var' -> bool
      val compare_level : Term.TermType.level_exp' -> Term.TermType.level_exp' -> bool
      val compare_param : Term.TermType.param' -> Term.TermType.param' -> bool
      val compare_operator : Term.TermType.operator' -> Term.TermType.operator' -> bool
      val compare_term : Term.TermType.term' -> Term.TermType.term' -> bool
      val compare_hyps : Term.Term.SeqHyp.t -> Term.Term.SeqHyp.t -> int -> bool
      val compare_cterm : c_term -> c_term -> bool
      val compare_bterm : Term.TermType.bound_term' -> Term.TermType.bound_term' -> bool

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
