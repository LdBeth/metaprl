(*
 * Order over terms.
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
 * Copyright (C) 2003 Yegor Bryukhov, Moscow State University
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
 *)

module type TermOrderSig =
functor(R: Refiner_sig.RefinerSig) ->
sig
	open R.TermType

	type comparison = Less | Equal | Greater

	val compare_level_vars : level_exp_var -> level_exp_var -> comparison
	val compare_levels : level_exp -> level_exp -> comparison
	val compare_params : param -> param -> comparison
	val compare_operators : operator -> operator -> comparison
	val compare_terms : term -> term -> comparison
	val compare_bterms : bound_term -> bound_term -> comparison
	(*val compare_hyps : Term.Term.SeqHyp.t -> Term.Term.SeqHyp.t -> int -> comparison
	val compare_goals : Term.Term.SeqGoal.t -> Term.Term.SeqGoal.t -> int -> comparison*)
end

module TermOrder: TermOrderSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
