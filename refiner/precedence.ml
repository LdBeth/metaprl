(*
 * A precedence is really just a partial order,
 * where we ask only for the transitive closure.
 * This implementation is imperative.
 *
 * $Log$
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
 *)

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
type precedence = int

(*
 * For each precedence in the squashed DAG, we store a
 * list of entries of smaller elements.
 *)
type precedence_entry = int list

(*
 * The DAG has the following invariant:
 *    1. Vertices in the DAG are sorted in
 *       some topological order, and each vertex
 *       can contain edges only to smaller vertices.
 *
 * The base is implemented as a squashed DAG, where there
 * is a level of indeirection to collapse equal nodes in
 * the DAG.  We store the outedges at each node in the DAG,
 * and we support the operations:
 *    1. create_dag
 *    2. add an unconnected vertex
 *    3. add an edge
 *    4. equate two vertices
 *    5. get relationship of two vertices
 *)
type precedence_base =
   { mutable squash_map : int array;
     mutable squash_dag : precedence_entry array
   }

(*
 * For catching precedence cycles.
 *)
exception Cycle of precedence * precedence

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let min_prec = 0
let max_prec = 1

let base = { squash_map = [| 0; 1 |]; squash_dag = [| []; [0] |] }

(*
 * Check if a vertex is a descendent of another.
 *)
let is_child index1 index2 =
   let dag = base.squash_dag in
   let flags = Array.create (index2 + 1) false in
   let rec aux index =
      if index = index1 then
         true
      else if flags.(index) then
         false
      else
         begin
            flags.(index) <- true;
            List.exists aux dag.(index)
         end
   in
      aux index2

(*
 * A grand-descendent.
 *)
let is_grandchild index1 index2 =
   let dag = base.squash_dag in
   let flags = Array.create index2 false in
   let rec aux index =
      if index = index1 then
         true
      else if flags.(index) then
         false
      else
         begin
            flags.(index) <- true;
            List.exists aux dag.(index)
         end
   in
      List.exists aux dag.(index2)

(*
 * Move a vertex before another.
 * index1 occurs _after_ index2.
 *)
let move_vertex index1 index2 =
   (* Conversion function *)
   let convert index =
      if index < index2 then
         index
      else if index < index1 then
         index + 1
      else if index = index1 then
         index2
      else
         index
   in

   (* Map info *)
   let map = base.squash_map in
   let mlen = Array.length map in

   (* Convert the dag *)
   let dag = base.squash_dag in
   let dlen = Array.length dag in
   let entry = dag.(index1) in
      for i = index1 - 1 downto index2 do
         dag.(i + 1) <- List.map convert dag.(i)
      done;
      dag.(index2) <- List.map convert entry;

      (* Convert the map *)
      for i = 0 to mlen - 1 do
         map.(i) <- convert map.(i)
      done

(*
 * Install a less-than relation.
 * Make sure there are no cycles, by doing a depth first search for
 * an etnry with an index no smaller than p1.  If the check succeeds,
 * the recompute the index for p1, and bump up all entries above p1.
 *)
let add_lt p1 p2 =
   (* Make sure p1 is not an ancestor of p2 *)
   let map = base.squash_map in
   let dag = base.squash_dag in
   let index1 = map.(p1) in
   let index2 = map.(p2) in
      if index1 = index2 then
         (* These precedences are equal! *)
         raise (Cycle (p1, p2))
      else if index1 < index2 then
         (* No possible cycles *)
         if is_child index1 index2 then
            ()
         else
            dag.(index2) <- index1 :: dag.(index2)

      else if is_child index2 index1 then
         (* This is a cycle *)
         raise (Cycle (p1, p2))

      else
         (* Move index1 before index2 to preserve the invariant *)
         begin
            move_vertex index1 index2;
            dag.(index2 + 1) <- index2 :: dag.(index2 + 1)
         end

(*
 * Collapse two nodes in the DAG.  These nodes are unrelated, or
 * there is at most 1 edge between them.
 *
 * We remove the larger index, to preserve the invariant.
 *)
let collapse_nodes index1 index2 =
   (* Sort the vertices *)
   let lower, upper =
      if index1 < index2 then
         index1, index2
      else
         index2, index1
   in

   (* Convert references to upper *)
   let convert index =
      if index = upper then
         lower
      else if index > upper then
         index - 1
      else
         index
   in

   (* Map info *)
   let map = base.squash_map in
   let mlen = Array.length map in

   (* Convert the dag *)
   let dag = base.squash_dag in
   let dlen = Array.length dag in
   let dag' = Array.create (dlen - 1) [] in
      Array.blit dag 0 dag' 0 upper;
      for i = upper to dlen - 1 do
         dag'.(i) <- List.map convert dag.(i + 1)
      done;
      base.squash_dag <- dag;

      (* Convert the map *)
      for i = 0 to mlen do
         map.(i) <- convert map.(i)
      done
         
(*
 * Install an equality relation.  Make sure there is no path longer than length
 * 1 between the nodes, then remap all the references to node1 so they
 * point to node2.
 *)
let add_eq p1 p2 =
   (* Make sure there is no path longer *)
   let map = base.squash_map in
   let dag = base.squash_dag in
   let index1 = map.(p1) in
   let index2 = map.(p2) in
   let lower, upper =
      if index1 < index2 then
         index1, index2
      else
         index2, index1
   in
      if index1 = index2 then
         (* No change *)
         ()
      else if is_grandchild lower upper then
         (* Path of length 2 or more *)
         raise (Cycle (p1, p2))
      else
         (* Collapse the two nodes in the dag *)
         collapse_nodes lower upper

(*
 * Get the relation.
 *)
let get_prec p1 p2 =
   let map = base.squash_map in
   let index1 = map.(p1) in
   let index2 = map.(p2) in
      if index1 = index2 then
         EQRelation
      else if index1 < index2 then
         if is_child index1 index2 then
            LTRelation
         else
            NoRelation
      else
         if is_child index2 index1 then
            GTRelation
         else
            NoRelation

(*
 * New precedence in the base.
 * This adds a new vertex to the map, and
 * a new vertex that is unrelated to any other in
 * the DAG.
 *)
let new_prec () =
   let mlen = Array.length base.squash_map in
   let dlen = Array.length base.squash_dag in
   let map' = Array.create (mlen + 1) dlen in
   let dag' = Array.create (dlen + 1) [] in
      Array.blit base.squash_map 0 map' 0 mlen;
      Array.blit base.squash_dag 0 dag' 0 dlen;
      base.squash_map <- map';
      base.squash_dag <- dag';
      add_lt min_prec mlen;
      add_lt mlen max_prec;
      mlen

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
