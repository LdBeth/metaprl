(*
 * A precedence is really just a partial order,
 * where we ask only for the transitive closure.
 * This implementation is imperative.
 *
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
open Lm_imp_dag

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Precedence%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Relations.
 *)
type relation =
   NoRelation
 | LTRelation
 | EQRelation
 | GTRelation

(*
 * Actual precedence is an index into the base vect.
 *)
type precedence = unit ImpDag.node

(*
 * Create the one and only precedence graph.
 *)
let graph = ImpDag.create ()

(*
 * Min and max precedence.
 *)
let min_prec = ImpDag.insert graph ()
let max_prec = ImpDag.insert graph ()
let _ = ImpDag.add_edge graph min_prec max_prec

(*
 * New prec adds a new prec related to min and max.
 *)
let new_prec () =
   let p = ImpDag.insert graph () in
      ImpDag.add_edge graph min_prec p;
      ImpDag.add_edge graph p max_prec;
      p

(*
 * Add edges.
 *)
let add_lt p1 p2 =
   ImpDag.add_edge graph p1 p2

let add_eq p1 p2 =
   ImpDag.equate graph p1 p2

(*
 * Get relation.
 *)
let get_prec p1 p2 =
   match ImpDag.node_rel graph p1 p2 with
      Lm_dag_sig.NoRelation ->
         NoRelation
    | Lm_dag_sig.LessThan ->
         LTRelation
    | Lm_dag_sig.GreaterThan ->
         GTRelation
    | Lm_dag_sig.Equal ->
         EQRelation

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
