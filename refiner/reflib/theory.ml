(*
 * Simple recorder for all the theories in the system.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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
 *
 *)

open Printf
open Nl_debug

open Refiner.Refiner
open Refiner.Refiner.Refine
open Dform_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Theory%t" eflush

(*
 * Info is saved in this format.
 *)
type theory =
   { thy_name : string;
     thy_refiner : refiner;
     thy_dformer : dform_mode_base
   }

(*
 * Save all the theories on a list.
 *)
let base = ref ([] : theory list)

(*
 * Record a theory by pushing it onto the list.
 *)
let record_theory thy =
   Ref_util.push thy base

(*
 * Get all the theories.
 *)
let get_theories () =
   !base

(*
 * Get the parents of a theory.
 *)
let get_parents thy =
   let rec find_parent parent = function
      thy :: tl ->
         let { thy_refiner = refiner } = thy in
            if refiner == parent then
               thy
            else
               find_parent parent tl
    | [] ->
         raise Not_found
   in
   let rec search refiner =
      if is_null_refiner refiner then
         []
      else
         let item, refiner' = dest_refiner refiner in
            match item with
               RIParent parent ->
                  begin
                     try find_parent parent !base :: search refiner' with
                        Not_found ->
                           search refiner'
                  end
             | _ ->
                  search refiner'
   in
   let { thy_refiner = refiner } = thy in
      search refiner

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
