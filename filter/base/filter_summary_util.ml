(*
 * Utilities for the filter_summary module.
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

open Lm_debug

open Refiner.Refiner.TermMan

open Filter_type

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_summary_util%t"

(*
 * Extract the context var arguments.
 *)
let rec collect_cvars = function
   ContextParam v::t ->
      v :: collect_cvars t
 | _::t ->
      collect_cvars t
 | [] ->
      []

let rec collect_terms = function
   TermParam x :: t ->
      x :: collect_terms t
 | _ :: t ->
      collect_terms t
 | [] ->
      []

(*
 * Split parameters into the three types.
 *)
let rec split_params = function
   h::t ->
      let cvars, tparams = split_params t in
         begin
            match h with
               ContextParam v ->
                  v :: cvars, tparams
             | TermParam t ->
                  cvars, t :: tparams
         end
 | [] ->
      [], []

(*
 * Give names to all the parameters.
 *)
let name_params =
   let rec loop i = function
      h::t ->
         let aids, cids, xids = loop (i + 1) t in
         let name = "id_" ^ (string_of_int i) in
            begin
               match h with
                  ContextParam _ ->
                     name :: aids, name :: cids, xids
                | TermParam _ ->
                     name :: aids, cids, name :: xids
            end
    | [] ->
         [], [], []
   in
      loop 0

(*
 * Distinguish between context var parameters, and other parameters.
 *)
let extract_params cvars bvars =
   let aux h =
      if is_so_var_term h then
         let v, conts, terms = dest_so_var h in
            if List.mem v cvars then
               ContextParam v
            else
               TermParam h
      else
         TermParam h
   in
      List.map aux

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
