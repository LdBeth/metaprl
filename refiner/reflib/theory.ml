(*
 * Simple recorder for all the theories in the system.
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

open Refiner.Refiner.Refine

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Theory%t"

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_refiner : refiner;
   }

(*
 * Save all the theories on a list.
 *)
let base = ref ([] : theory list)

(*
 * Record a theory by pushing it onto the list.
 *)
let record_theory thy =
   Lm_ref_util.push thy base

(*
 * Get all the theories.
 *)
let get_theories () =
   !base

(*
 * Get a theory by name.
 *)
let get_theory name =
   let rec search = function
      thy :: t ->
         if thy.thy_name = name then
            thy
         else
            search t
    | [] ->
         invalid_arg ("Theory.get_theory: theory ``" ^ name ^ "'' not found")
   in
      search !base

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
