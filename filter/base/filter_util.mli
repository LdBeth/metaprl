(*
 * Common utilities for filtering modules.
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
open Lm_symbol
open Lm_printf

open Opname
open Refiner.Refiner.TermType
open Filter_type

(************************************************************************
 * UTILITIES								*
 ************************************************************************)

val context_vars_list : term list -> var list
val unzip_rewrite : string -> meta_term -> term list * term * term

val split_mfunction : meta_term -> string list list * term list * meta_term

(*
 * Module paths.
 *)
val string_of_path : module_path -> string
val output_path : out_channel -> module_path -> unit

(*
 * Command-line options for group and description of the theory
 *)
val make_groupdsc_opts : unit -> (unit -> string) * (unit -> string)

(*
 * MetaPRL bindings in str items
 *)
val add_binding : term prl_binding -> MLast.expr
val get_bindings : unit -> (string * term prl_binding) list
(* Do not convert bound contexts *)
val get_unparsed_bindings : unit -> (string * term prl_binding) list

val no_resources : (MLast.expr, term) resource_def

val dummy_loc : MLast.loc
val mk_proper_loc : Lm_num.num -> Lm_num.num -> MLast.loc (* XXX: temporary OCaml 3.06 -> 3.08 conversion HACK *)
val shift_pos : Lexing.position -> int -> Lexing.position
val adjust_pos : Lexing.position -> Lexing.position -> Lexing.position

(************************************************************************
 * OPNAMES								*
 ************************************************************************)

val string_of_opname_list : string list -> string
val translate_opname : opname -> string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
