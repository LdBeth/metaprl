(*
 * Imperative DAGs.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:13  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/05/21 02:25:19  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/01 15:04:12  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

(* Abstract types *)
type 'a imp_dag
type 'a imp_dag_node

(* Constructors *)
val new : unit -> 'a imp_dag
val insert : 'a imp_dag -> 'a -> 'a imp_dag_node
val add_edge : 'a imp_dag -> 'a imp_dag_node -> 'a imp_dag_node -> unit

(* Values *)
val node_value : 'a imp_dag -> 'a imp_dag_node -> 'a
val node_out_edges : 'a imp_dag -> 'a imp_dag_node -> 'a imp_dag_node list
val node_in_edges : 'a imp_dag -> 'a imp_dag_node -> 'a imp_dag_node list

(*
 * Mappers.
 * Sweep_up maps a function up the DAG, calling on the leaves first;
 *    it returns a list of the roots in the DAG
 * Sweep_down maps a function down the DAG, calling on the roots first;
 *    it returns a list of the leaves in the DAG.
 * The _all versions are the same, except they return a list of all the nodes.
 *)
val sweep_up : 'a imp_dag -> ('a -> 'b list -> 'b) -> 'b list
val sweep_down : 'a imp_dag -> ('a -> 'b list -> 'b) -> 'b list
val sweep_up_all : 'a imp_dag -> ('a -> 'b list -> 'b) -> 'b list
val sweep_down_all : 'a imp_dag -> ('a -> 'b list -> 'b) -> 'b list

(* Sort *)
val sort_up : 'a imp_dag -> 'a imp_dag_node list
val sort_down : 'a imp_dag -> 'a imp_dag_node list

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
