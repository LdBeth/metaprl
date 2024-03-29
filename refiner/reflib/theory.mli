(*
 * Simple mechanism for recording refiners and display forms.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *
 *)

open Refiner.Refiner.Refine

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;      (* e.g. "itt_list" *)
     thy_group : string;     (* e.g. "itt" *)
     thy_groupdesc : string; (* e.g. "Constructive Type Theory" *)
     thy_refiner : refiner;
   }

(* Save the theory *)
val record_theory : theory -> unit

(* Get back all the theories that have been recorded *)
val get_theory_names : unit -> string list
val theory_exists : string -> bool
val iter_theories : (theory -> unit) -> unit
val get_theory : string -> theory

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
