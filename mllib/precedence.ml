(*
 * A precedence is really just a partial order,
 * where we ask only for the transitive closure.
 * This implementation is imperative.
 *
 *)

open Printf
open Nl_debug
open Imp_dag

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Precedence%t" eflush

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
      Dag.NoRelation ->
         NoRelation
    | Dag.LessThan ->
         LTRelation
    | Dag.GreaterThan ->
         GTRelation
    | Dag.Equal ->
         EQRelation

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
