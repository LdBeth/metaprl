(*
 * Convert between proofs and terms.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

include Io_proof_type

open Opname
open Dform
open Refiner.Refiner.Term

open Io_proof_type

val interface_op : opname
val implementation_op : opname

(*
 * Conversion to terms.
 *)
val term_of_proof : ('term -> term) -> 'term proof -> term
val proof_of_term : (term -> 'term) -> 'term Tactic_type.attributes -> term -> 'term proof

(*
 * This function extracts an expression that
 * evaluates to a (string * tactic) array.
 *)
val tactics_of_proof : 'term proof -> MLast.expr

(*
 * Compute the total number of nodes in the proof.
 *)
val node_count_of_proof : 'term proof -> int

(*
 * Print the proof.
 *)
val print_proof : dform_base -> term proof -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
