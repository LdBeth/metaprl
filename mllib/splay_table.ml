(*
 * This takes the splay set implementation,
 * and generalizes it to functional hashtables.
 *)
module type SplayTableSig =
sig
   type elt
   type 'a t

   val create : unit -> 'a t
   val add : 'a t -> elt -> 'a -> 'a t
   val union : ('a list -> 'a list -> 'a list) -> 'a t -> 'a t -> 'a t
   val mem : 'a t -> elt -> bool
   val find : 'a t -> elt -> 'a
   val find_all : 'a t -> elt -> 'a list
   val remove : 'a t -> elt -> 'a t
   val iter : (elt -> 'a -> unit) -> 'a t -> unit
   val map : (elt -> 'a -> 'b) -> 'a t -> 'b t
end

(*
 * Build the set from an ordered type.
 *)
module MakeSplayTable (Ord: Set.OrderedType) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type elt = Ord.t

   type 'a tree =
      Leaf
    | Node of 'a node

   and 'a node = (elt * 'a list * 'a t * 'a t) ref

   (*
    * The int is the total number
    * of entries in the table.
    *)
   and 'a t = 'a tree * int

   (*
    * Directions are used to define
    * paths in the tree.
    *)
   type 'a direction =
      Left of 'a node
    | Right of 'a node

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Rotate the left entry to the root.
    * The root becomes the right child.
    *)
   let rotate_left = function
      { contents = key,
                   node_contents,
                   (Node { contents = left_key,
                                      left_contents,
                                      left_left,
                                      ((_, slr) as left_right)
                    }, _),
                   ((_, sr) as right)
      } as node ->
         node := left_key,
                 left_contents,
                 left_left,
                 (Node (ref (key,
                             node_contents,
                             left_right,
                             right)),
                  slr + sr + 1)
    | _ ->
         raise (Invalid_argument "rotate_left")

   (*
    * Rotate the right entry to the root.
    * the root becomes the left child.
    *)
   let rotate_right = function
      { contents = key,
                   node_contents,
                   ((_, sl) as left),
                   (Node { contents = right_key,
                                     right_contents,
                                     ((_, srl) as right_left),
                                     right_right
                    }, _)
      } as node ->
         node := right_key,
                 right_contents,
                 (Node (ref (key,
                             node_contents,
                             left,
                             right_left)),
                  sl + srl + 1),
                 right_right
    | _ ->
         raise (Invalid_argument "rotate_right")

   (*
    * This function performs the action of moving an entry
    * to the root.  The argument is the path to the entry.
    *)
   let rec lift = function
      [] ->
         ()
    | [Left parent] ->
         rotate_left parent
    | [Right parent] ->
         rotate_right parent
    | Left parent :: Left grandparent :: ancestors ->
         begin
            rotate_left grandparent;
            rotate_left grandparent;  (* parent has moved into grandparent's position *)
            lift ancestors
         end
    | Right parent :: Right grandparent :: ancestors ->
         begin
            rotate_right grandparent;
            rotate_right grandparent;  (* parent has moved into grandparent's position *)
            lift ancestors
         end
    | Left parent :: Right grandparent :: ancestors ->
         begin
            rotate_left parent;
            rotate_right grandparent;
            lift ancestors
         end
    | Right parent :: Left grandparent :: ancestors ->
         begin
            rotate_right parent;
            rotate_left grandparent;
            lift ancestors
         end
   
   let rec lift_right = function
      [] ->
         ()
    | [parent] ->
         rotate_right parent
    | parent :: grandparent :: ancestors ->
         begin
            rotate_right grandparent;
            rotate_right grandparent;  (* parent has moved into grandparent's position *)
            lift_right ancestors
         end

   (*
    * Find an entry in the tree.
    * Returns true iff the entry is found.
    * Transforms the tree so that either the
    * entry becomes the root, or an adjacent entry
    * becomes the root if the entry is not found.
    *)
   let rec splay key0 path = function
      Node ({ contents = key, _, left, right } as node), _ ->
         let comp = Ord.compare key0 key in
            if comp = 0 then
               begin
                  lift path;
                  true
               end
            else if comp < 0 then
               (* node is down the left branch *)
               splay key0 (Left node :: path) left
            else
               (* node is down the right branch *)
               splay key0 (Right node :: path) right

    | (Leaf, _) ->
         match path with
            [] ->
               false
          | _ :: path' ->
               lift path';
               false

   let rec splay_right path = function
      Node ({ contents = key, _, left, right } as node), _ ->
         splay_right (node :: path) right

    | (Leaf, _) ->
         match path with
            [] ->
               ()
          | _ :: path' ->
               lift_right path'

   (*
    * An empty tree is just a list.
    *)
   let empty =
      Leaf, 0

   let create () =
      empty

   (*
    * Check if a key is listed in the table.
    *)
   let mem t key =
      splay key [] t

   let find t key =
      if splay key [] t then
         match t with
            Node { contents = _, data :: _, _, _ }, _ ->
               data
          | _ ->
               raise (Failure "find")
      else
         raise Not_found

   let find_all t key =
      if splay key [] t then
         match t with
            Node { contents = _, data, _, _ }, _ ->
               data
          | _ ->
               raise (Failure "find")
      else
         raise Not_found

   (*
    * Add an entry to the table.
    * If the entry already exists,
    * the new value is added to the data.
    *)
   let add_list t key data =
      if splay key [] t then
         match t with
            Node { contents = key, data', left, right }, size ->
               Node (ref (key, data @ data', left, right)), size
          | Leaf, _ ->
               raise (Failure "add_list")
      else
         match t with
            Node { contents = key', data', ((_, sl) as left), ((_, sr) as right) }, _ ->
               if Ord.compare key key' < 0 then
                  (* Root should become right child *)
                  Node (ref (key,
                             data,
                             left,
                             (Node (ref (key', data', empty, right)), sr + 1))),
                  sl + sr + 2
               else
                  (* Root should become left child *)
                  Node (ref (key,
                             data,
                             (Node (ref (key', data', left, empty)), sl + 1),
                             right)),
                  sl + sr + 2
          | Leaf, _ ->
               (* Tree is empty, so make a new root *)
               Node (ref (key, data, empty, empty)), 1

   let add t key data =
      add_list t key [data]

   (*
    * Remove the first entry from the hashtable.
    * If the value list becomes empty, remove the
    * entire entry from the tree.
    *)
   let remove t key =
      if splay key [] t then
         match t with
            Node { contents = key, _ :: data, left, right }, size ->
               Node (ref (key, data, left, right)), size
          | Node { contents = _, [], (Leaf, _), right }, _ ->
               right
          | Node { contents = _, [], left, (Leaf, _) }, _ ->
               left
          | Node { contents = _, [], left, ((_, sr) as right) }, _ ->
               begin
                  splay_right [] left;
                  match left with
                     Node { contents = left_key, left_data, ((_, sll) as left_left), (Leaf, _) }, _ ->
                        Node (ref (left_key, left_data, left_left, right)), 1 + sll + sr
                   | _ ->
                        raise (Failure "remove")
               end
          | _ ->
               raise (Failure "remove")
      else
         t

   (*
    * Merge two hashtables.
    * Data fields get concatenated.
    *)
   let rec union append s1 s2 =
      match s1, s2 with
         (Leaf, _), _ ->
            s2
       | _, (Leaf, _) ->
            s1
       | (Node n1, size1), (Node n2, size2) ->
            if size1 >= size2 then
               if size2 = 1 then
                  let key2, data2, _ ,_ = !n2 in
                     add_list s1 key2 data2
               else
                  let key1, data1, left1, right1 = !n1 in
                     if splay key1 [] s2 then
                        let _, data2, left2, right2 = !n2 in
                        let (_, sll) as ll = union append left1 left2 in
                        let (_, srr) as rr = union append right1 right2 in
                           Node (ref (key1, append data1 data2, ll, rr)), 1 + sll + srr
                     else
                        let key2, data2, ((_, sl2) as left2), ((_, sr2) as right2) = !n2 in
                           if Ord.compare key1 key2 < 0 then
                              let (_, sll) as ll = union append left1 left2 in
                              let (_, srr) as rr =
                                 union append right1 (Node (ref (key2, data2, empty, right2)), succ sr2)
                              in
                                 Node (ref (key1, data1, ll, rr)), 1 + sll + srr
                           else
                              let (_, sll) as ll =
                                 union append left1 (Node (ref (key2, data2, left2, empty)), succ sl2)
                              in
                              let (_, srr) as rr = union append right1 right2 in
                                 (Node (ref (key1, data1, ll, rr)), 1 + sll + srr)
            else if size1 = 1 then
               let key1, data1, _, _ = !n1 in
                  add_list s2 key1 data1
            else
               let key2, data2, left2, right2 = !n2 in
                  if splay key2 [] s1 then
                     let _, data1, left1, right1 = !n1 in
                     let (_, sll) as ll = union append left2 left1 in
                     let (_, srr) as rr = union append right2 right1 in
                        Node (ref (key2, append data1 data2, ll, rr)), 1 + sll + srr
                  else
                     let key1, data1, ((_, sl1) as left1), ((_, sr1) as right1) = !n1 in
                        if Ord.compare key2 key1 < 0 then
                           let (_, sll) as ll = union append left2 left1 in
                           let (_, srr) as rr =
                              union append right2 (Node (ref (key1, data1, empty, right1)), succ sr1)
                           in
                              Node (ref (key2, data2, ll, rr)), 1 + sll + srr
                        else
                           let (_, sll) as ll =
                              union append left2 (Node (ref (key1, data1, left1, empty)), succ sl1)
                           in
                           let (_, srr) as rr = union append right2 right1 in
                              Node (ref (key2, data2, ll, rr)), 1 + sll + srr

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter f = function
      Node { contents = key, data, left, right }, _ ->
         List.iter (f key) data;
         iter f left;
         iter f right
    | Leaf, _ ->
         ()

   (*
    * Map a function over the table.
    *)
   let rec map f = function
      Node { contents = key, data, left, right }, size ->
         Node (ref (key,
                    List.map (f key) data,
                    map f left,
                    map f right)), size
    | Leaf, _ ->
         Leaf, 0
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
