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

declare "interface"{'intf}
declare "implementation"{'impl}
declare "location"[start:n, finish:n]{'body}

declare "rewrite"[name:s]{'redex; 'contractum; 'proof; 'res}
declare "cond_rewrite"[name:s]{'params; 'args; 'redex; 'contractum; 'proof; 'res}
declare "rule"[name:s]{'params; 'stmt; 'proof; 'res}
declare "opname"[name:s]{'term}
declare "definition"[name:s]{'term; 'definition; 'res}
declare "mlterm"{'term; 'cons; 'oexpr}
declare "condition"{'term; 'cons; 'oexpr}
declare "mlrewrite"[name:s]{'params; 'redex; 'body; 'resources}
declare "parent"{'path; 'resources}
declare "module"[name:s]{'info}
declare "dform"[name:s]{'modes; 'redex; 'def}
declare "prec"[name:s]
declare "prec_rel"[op, left, right]
declare "id"[n:n]
declare "resource"[name:s]{'expr}
declare "resource"{'inp; 'outp; 'expr}
declare "infix"[name:s]
declare "suffix"[name:s]
declare "magic_block"[name:s]{'items}
declare "summary_item"{'term}
declare "resource_defs"[start:n, finish:n, name:s]{'res}
declare "resource_defs"[name:s]{'res}
declare "comment"{'t}

declare "inherit_df"
declare "prec_df"[name:s]
declare "parens_df"
declare "mode_df"[mode:s]

declare "df_none"
declare "df_term"{'t}
declare "df_ml"[printer:s, buffer:s]{'contracta; 'code}

declare "none"
declare "some"{'t}

declare "meta_theorem"{'A}
declare "meta_implies"{'A; 'B}
declare "meta_function"{'arg; 'A; 'B}
declare "meta_iff"{'A; 'B}
declare "meta_labeled"[label:s]{'meta}

declare "context_param"[name:v]
declare "term_param"{'t}

(* Proofs *)
declare "href"[command:s]{'t}

declare status_bad
declare status_partial
declare status_asserted
declare status_complete
declare status_primitive{'extract}
declare status_interactive[rules:n,nodes:n]{'status}

declare "goal_status"{'sl}
declare "goal_label"[s:s]
declare "goal_list"{'goals}
declare "goal"{'status; 'label; 'assums; 'goal}
declare "subgoals"{'subgoals; 'extras}
declare "rule_box"[text:s]
declare "proof"{'main; 'goal; 'status; 'text; 'subgoals}

(* PRL Bindings *)
declare term_binding{'t;v.'t2['v]}
declare opname_binding{'t;v.'t2['v]}

(************************************************************************
 * ML ACCESS                                                            *
 ************************************************************************)

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

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
