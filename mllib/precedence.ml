(*
 * A precedence is really just a partial order,
 * where we ask only for the transitive closure.
 * This implementation is imperative.
 *
 *)

open Imp_dag

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
 * $Log$
 * Revision 1.2  1998/04/08 15:08:26  jyh
 * Moved precedence to mllib.
 *
 * Revision 1.2  1998/04/08 14:57:30  jyh
 * ImpDag is in mllib.
 *
 * Revision 1.1  1997/04/28 15:51:28  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.4  1996/05/21 02:14:00  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.3  1996/04/11 13:29:39  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.2  1996/04/07 18:24:48  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.1  1996/04/04 20:19:17  jyh
 * This is a functional version of the precedentor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
