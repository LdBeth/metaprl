(*
 * Testing.
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

open Printf
open Lm_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.Refine

open Tacticals
open Conversionals

open Typeinf
open Dtactic
open Auto_tactic
open Itt_rfun
open Itt_fun
open Itt_int
open Itt_logic
open Itt_dprod
open Itt_union
open Itt_equal
open Itt_struct
open Itt_w
open Itt_derive

open Mp
open Test

open Czf_itt_set
open Czf_itt_eq
open Czf_itt_member
open Czf_itt_sep
open Czf_itt_union
open Czf_itt_map
open Czf_itt_all
open Czf_itt_exists
open Czf_itt_dall
open Czf_itt_dexists
open Czf_itt_sall
open Czf_itt_sexists
open Czf_itt_rel

(*
 * Proof saving.
 *)
let zT, z =
   let pf = ref None in
   let zT p =
      pf := Some p;
      idT p
   in
   let z () =
      match !pf with
         Some p ->
            p
       | None ->
            raise Not_found
   in
      zT, z

let _ = cd "czf_itt_axioms/subset_collection"
let _ = set_writeable ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
