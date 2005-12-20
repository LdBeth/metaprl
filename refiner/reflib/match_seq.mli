(*
 * Tries to match hypothesys lists in a pair of sequents
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *
 *)

open Refiner.Refiner
open TermType

(*
 * match_hyps big small
 *   will try to match all the hypotheses in the small sequent
 *   agains hypotheses of the big one. If succeeds, will return
 *   an option array telling which hyp of small matches each hyp of
 *   large (numbering starts with 0).
 * The first argument is used for matching the conclusions (hypotheses are
 * always compared using alpha equality).
 *
 * match_some_hyps is simplar, but does not insist on matching all the hyps
 * or the goal (does insist on matching all Contexts, though). If succeeds,
 * will return the nimber of the hyps in the "small" sequent that were
 * succesfully matched.
 *
 * These functions raise RefineError, when they fail.
 *)
val match_hyps: (term -> term -> bool) -> esequent -> esequent -> int option array
val match_some_hyps: esequent -> esequent -> int
