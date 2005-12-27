(*
 * We define a simple auto tactic, where it
 * is possible to add tactics to be tried by the auto tactic.
 *
 * This is the simple-minded auto tactic.  Each tactic
 * is given a precedence, and the tactics are ordered
 * by their precedences before they are tried.
 *
 * The trivialT tactic is like autoT, but it is intended
 * that trivialT either proves the goal or fails.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Mp_resource
open Refiner.Refiner.TermType
open Tactic_type.Tactic

(*
 * The nthHypT resource is used to match the conclusion against the given
 * hypothesis. The nthHypT tactic is not supposed to produce subgoals - it
 * fails, or succeeds right away.
 *)
topval nthHypT : int -> tactic
val nth_hyp_mem : tactic_arg -> term -> term -> bool

(*
 * The input for the nth_hyp resource is the hypothesis term, conclusion term
 * and the tactic nthHypT should use when applying
 *)
resource (term * term * (int -> tactic), (int -> tactic) * (term -> term -> bool)) nth_hyp

val process_nth_hyp_resource_annotation :
   (pre_tactic, term * term * (int -> tactic)) annotation_processor

(*
 * The info provided is a name (used for debugging),
 * a precedence, and a function
 * to produce a tactic.  The function
 * is called once per run of the auto tactic.
 *)
type auto_prec

type auto_info = {
   auto_name : string;
   auto_prec : auto_prec;
   auto_tac : tactic;
   auto_type : auto_type;
}

and auto_type =
   AutoTrivial
 | AutoNormal
 | AutoComplete

resource (auto_info, tactic * tactic * tactic * tactic) auto

(*
 * Operations on precedences.
 * The create operation takes a list of precedences that
 * are smaller, and another list that are larger.
 *)
val create_auto_prec : auto_prec list -> auto_prec list -> auto_prec

(*
 * trivialT, nthHypT and reduceT are used by autoT.
 *)
val trivial_prec : auto_prec
val nth_hyp_prec : auto_prec
val reduce_prec : auto_prec

(*
 * Trivial tactic.
 *)
topval trivialT : tactic
topval strongAutoT : tactic (* use AutoComplete entries freely *)
topval completeAutoT : tactic (* equivalent to "completeT strongAutoT", but faster *)
topval tcaT : tactic (* equivalent to "tryT (completeT strongAutoT)", but faster *)
topval autoT : tactic (* "AutoTrivial" orthenT "AutoNormal" thenT tcaT *)

(*
 * "tac ttca" is a short for "tac thenT tcaT"
 * "tac tatca" is a short for "tac thenAT tcaT"
 * "tac twtca" is a short for "tac thenWT tcaT"
 * "tac taa" is a short for "tac thenAT autoT"
 * "tac twa" is a short for "tac thenWT autoT"
 *)
suffix ttca
suffix tatca
suffix twtca
suffix taa
suffix twa

topval prefix_ttca : tactic -> tactic
topval prefix_tatca : tactic -> tactic
topval prefix_twtca : tactic -> tactic
topval prefix_taa : tactic -> tactic
topval prefix_twa : tactic -> tactic

topval byDefT: conv -> tactic
topval byDefsT: conv list -> tactic

topval repeatWithRwsT : conv list -> tactic -> tactic

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
