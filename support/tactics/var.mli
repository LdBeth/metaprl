(*
 * Utilities for generating variable names.
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

open Tactic_type.Sequent
open Refiner.Refiner.Term

(* Generate a new var different from any in the list *)
val new_var           : string -> string list -> string
val maybe_new_var     : string -> string list -> string
val maybe_new_vars    : string list -> string list -> string list
val maybe_new_var_arg : tactic_arg -> string -> string

(* var_subst_to_bind 'A[t] t = bind{v.'A['v]} *)
val var_subst_to_bind : term -> term -> term
val get_bind_from_arg_or_concl_subst : tactic_arg -> term -> term
val get_bind_from_arg_or_hyp_subst : tactic_arg -> int -> term -> term

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
