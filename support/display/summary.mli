(*
 * These are the declares for the terms in a Filter_summary.summary_item.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

extends Perv
extends Nuprl_font
extends Base_dform
extends Comment
extends Ocaml_df

open Refiner.Refiner.TermType

(*
 * Structured comments
 *)
declare comment{'t : Dform} : Dform

(*
 * Proof Status
 *)
declare status_bad : Dform
declare status_partial : Dform
declare status_asserted : Dform
declare status_complete : Dform
declare status_primitive{'extract : Dform} : Dform
declare status_interactive[rules:n,nodes:n]{'status : Dform} : Dform

(*
 * A documentation form for creating ``pretty'' versions of ML rewrites
 *)
declare fake_mlrw[name:s]{'redex : Dform; 'contractum : Dform} : Dform

val term_of_proof_status : Tactic_type.Proof.status -> term

(*
 * Interface and implementation files.
 *)
val mk_interface_term : term list -> term
val mk_implementation_term : term list -> term

(*
 * Proofs.
 *)
val mk_href_term : string -> term -> term

val mk_status_term : term list -> term

val mk_int_arg_term : int -> term
val mk_term_arg_term : term -> term
val mk_type_arg_term : term -> term
val mk_bool_arg_term : bool -> term
val mk_string_arg_term : string -> term
val mk_term_list_arg_term : term list -> term
val mk_addr_arg_term : Term_addr_sig.addr_item list -> term
val mk_arglist_term : term list -> term

val mk_goal_label_term : string -> term
val mk_goal_term : term -> term -> term list -> term -> term
val mk_goal_list_term : term list -> term
val mk_subgoals_term : term list -> term list -> term
val mk_rule_box_string_term : string -> term
val mk_rule_box_term : term -> term
val append_rule_box : term -> string -> term
val mk_proof_term : term -> term -> term -> term -> term -> term
val dest_rule_box : term -> string
val dest_proof : term -> term * term * term * term * term

(************************************************************************
 * Export the declarations.
 * JYH: we probably want to export nearly all of these terms
 * eventually, but we need to decide how to control the namespace.
 *)
doc <:doc<
   Rules and axioms are described with @emph{meta}-terms.
   The meta-terms are defined inductively:
   a term (a @tt[meta_theorem]) is a meta-term,
   and given two meta-terms $A$ and $B$, so are the
   meta-implication @tt[meta_implies], the meta-@misspelled{bi}-implication
   @tt[meta_iff], and the dependent meta-function @tt{meta_function}
   where @it[arg] is a variable quantified over $A$ and bound in $B$.  The
   @tt{meta_labeled} term is used to add a label to a meta-term.
   @end[doc]
>>
declare typeclass MTerm -> Dform
declare "meta_theorem"{'A : Judgment} : MTerm
declare "meta_implies"{'A : MTerm; 'B : MTerm} : MTerm
declare "meta_function"{'arg : Judgment; 'A : MTerm; 'B : MTerm} : MTerm
declare "meta_iff"{'A : 'a; 'B : 'a} : MTerm
declare "meta_labeled"[label:s]{'meta : MTerm} : MTerm
doc <:doc< @docoff >>

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
