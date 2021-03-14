(*
 * Simple recorder for all the theories in the system.
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
 * Copyright (C) 1998-2004, MetaPRL Group
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
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *
 *)

open Lm_debug
open Lm_string_set

open Refiner.Refiner.Refine

(*
 * Show the file loading.
 *)
let () =
   show_loading "Loading Theory"

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_group : string;
     thy_groupdesc : string;
     thy_refiner : refiner;
   }

module ST = LexStringTable

(*
 * Save all the theories on a set.
 *)
let base = ref (ST.empty : theory ST.t)

(* unused
let groups = Hashtbl.create 19
*)

(*
 * Record a theory by pushing it onto the list.
 *)
let record_theory thy =
   base := ST.add !base thy.thy_name thy

(*
 * Get all the theory name.
 *)
let get_theory_names () =
   ST.keys !base

(*
 * Test is exists
 *)
let theory_exists name =
   ST.mem !base name

(*
 * iterate on theories.
 *)
let iter_theories f =
   ST.iter (fun _ t -> f t) !base

(*
 * Get a theory by name.
 *)
let get_theory name =
   ST.find !base name

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
