(*
 * Imperative DAGS.  We implement the DAG as a vector.
 * Each entry in the vector contains the value for a node,
 * and all the outgoing edges, represented by their destination
 * index.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:12  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/05/21 02:25:17  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/01 15:04:08  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * An entry in the vector contains the value and
 * the out edges.  The "root" filed is a flag that is
 * true if the node has no incoming edges.
 *)
type 'a imp_dag_entry =
   { mutable entry_value : 'a;
     mutable entry_in_edges : int list;
     mutable entry_out_edges : int list
   }

(*
 * A DAG is just a vector of entries.
 *)
type 'a imp_dag =
   { mutable entries : ('a imp_dag_entry) array }

(*
 * A node is just an index into the vector.
 *)
type 'a imp_dag_node = int

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Construct a new DAG.
 *)
let new () = { entries = [||] }

(*
 * Add a new node to the end of the DAG.
 *)
let insert dag v =
   let entries = dag.entries in
   let len = Array.length entries in
   let entry =
      { entry_value = v;
        entry_in_edges = [];
        entry_out_edges = []
      }
   in
   let newvect = Array.create (len + 1) entry in
   let _ = Array.blit entries 0 newvect 0 len in
      dag.entries <- newvect;
      len

(*
 * Add an edge from node1 to node2.
 * There is _no_ checking for cycles.
 *)
let add_edge { entries = entries } node1 node2 =
   let entry1 = entries.(node1) in
   let entry2 = entries.(node2) in
   let out_edges = entry1.entry_out_edges in
      if not (List.mem node2 out_edges) then
         begin
            entry1.entry_out_edges <- node2::out_edges;
            entry2.entry_in_edges <- node1::entry2.entry_in_edges
         end

(*
 * Projections.
 *)
let node_value { entries = entries } i =
   entries.(i).entry_value

let node_out_edges { entries = entries } i =
   entries.(i).entry_out_edges

let node_in_edges { entries = entries } i =
   entries.(i).entry_in_edges

(*
 * Sweep a function up the DAG, calling on the children first.
 * The function is called only once on each node, so we keep a vector
 * of cached values.  The oproj and iproj are the projection
 * functions for entry.entry_out_edges and entry.entry_in_edges, or
 * vice versa.
 *)
let sweep_aux entries f oproj iproj =
   let len = Array.length entries in
   let values = Array.create len None in
   let rec expand i =
      match values.(i) with
         None ->
            let entry = entries.(i) in
            let children = List.map expand (oproj entry) in
            let result = f entry.entry_value children in
               values.(i) <- Some result;
               result

       | Some result ->
            result
   in
   let rec aux i =
      if i < len then
         let entry = entries.(i) in
         let result = expand i in
            if (iproj entry) = [] then
               result::(aux (i + 1))
            else
               aux (i + 1)
      else
         []
   in
      aux 0

let out_edges { entry_out_edges = edges } = edges

let in_edges { entry_in_edges = edges } = edges

let some_edges { entry_in_edges = _ } = []

let sweep_up { entries = entries } f =
   sweep_aux entries f out_edges in_edges
   
let sweep_down { entries = entries } f =
   sweep_aux entries f in_edges out_edges

let sweep_up_all { entries = entries } f =
   sweep_aux entries f out_edges some_edges
   
let sweep_down_all { entries = entries } f =
   sweep_aux entries f in_edges some_edges

(*
 * Perform a topological sort.
 *)
let sort_aux entries oproj =
   let len = Array.length entries in
   let values = Array.create len None in
   let index = ref 0 in
   let rec expand i =
      match values.(i) with
         None ->
            let out_edges = oproj entries.(i) in
            let _ = List.iter expand out_edges in
            let j = !index in
               values.(i) <- Some j;
               incr index;
               j
       | Some j -> j
   in
   let rec aux i =
      if i < len then
         let j = expand i in
            j::(aux (i + 1))
      else
         []
   in
      aux 0

let sort_up { entries = entries } =
   sort_aux entries out_edges

let sort_down { entries = entries } =
   sort_aux entries in_edges

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
