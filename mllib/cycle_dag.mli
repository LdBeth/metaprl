(*
 * This is a DAG used for cycle detection.
 *)

(*
 * Exception for reporting cycles.
 *)
exception Cycle

(*
 * Nodes are labelled with 'node,
 * and edges have label 'edge.
 *
 * 'node should be an equality type.
 *)
type ('node, 'edge) t

(*
 * Create an empty DAG.
 *)
val create : unit -> ('node, 'edge) t

(*
 * Make it from a previous subst.
 *)
val make : ('node list * ('edge * 'node list) option) list -> ('node, 'edge) t

(*
 * Find an edge.
 *)
val find : ('node, 'edge) t -> 'node -> 'edge

(*
 * Equate two nodes.
 * This raises Cycle if the two nodes are already
 * related by an edge.
 *)
val equate : ('node, 'edge) t -> 'node -> 'node -> unit

(*
 * Insert an edge-list from the source
 * to a list of sinks, with the given label.
 * This may raise the Cycle exception.
 *)
val insert : ('node, 'edge) t -> 'node -> 'edge -> 'node list -> unit

(*
 * Sort the edges, and return them in a list.
 *)
val sort : ('node, 'edge) t -> ('node list * 'edge option) list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
